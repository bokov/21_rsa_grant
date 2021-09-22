#' ---
#' title: "Preparation of data for analysis of successful community discharge from rehab by RSAs"
#' author: 'Timothy Reistetter ^1,✉^, Alex Bokov ^1^, Susanne Schmidt ^1^, Mei-Lin Min ^1^'
#' abstract: |
#'   | Merging and cleanup of CMS and Census data, with some preliminary
#'   unsupervised variable selection
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' css: 'production.css'
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---
#+ init, echo=FALSE, message=FALSE, warning=FALSE,results='hide'
# Init ----
debug <- 0;
knitr::opts_chunk$set(echo=debug>0, warning=debug>0, message=debug>0);
inputdata <- c(dat0='data/SIM_SDOH_ZCTA.xlsx'      # census data by ZCTA
               ,cx0='data/SIM_ALLCMS.csv'          # RSA-ZCTA crosswalk
               ,rsa0='data/SIM_RSAv4 SCD RSRs.csv' # outcomes (RSR)
               ,dct0='data/data_dictionary.tsv'    # data dictionary for the
                                                   # dat1 dataset that _this_
                                                   # scriport produces
);

# Load libraries ----
library(rio); library(dplyr); library(tidbits); # data handling
library(pander); library(broom);                # formatting
#library(GGally);
#library(mice);
library(Boruta);                                # variable selection
library(nFactors);                              # optimal number of factors


# Local project settings ----
if(file.exists('local.config.R')) source('local.config.R',local=T,echo = F);

# Import data ----
dat0 <- import(inputdata['dat0']);
cx0 <- import(inputdata['cx0']);
rsa0 <- import(inputdata['rsa0']);
dct0 <- import(inputdata['dct0']);
# temporary fix while rebuilding the simulated files properly
#rsa0 <- group_by(rsa0,RSA,Quartile) %>% summarise(RSR=max(RSR)) %>% select(RSR,RSA,Quartile) %>% data.frame
# Merge data ----
#' # Merging and Preparing Data
#'
#' `dat1` is the combined dataset, aggregated by RSA (`CN`) and weighted by
#' `ACS_TOTAL_POP_WT`.
#' The `dat2` dataset is `dat1` with numeric values scaled and the following
#' columns omitted: `ZCTA`, `YEAR`, and `Quartile`.
#' Finally, `dat3` is the just the numeric columns from the `dat3` dataset.
#+ dat1
dat1 <- left_join(dat0,cx0,by="ZCTA") %>%
  group_by(CN,YEAR) %>% summarise(across(matches('ACS_TOTAL_POP_WT'),sum,na.rm=T)
                             ,across(matches('REGION|STATE'),function(xx){
                               paste0(unique(xx),collapse='|')})
                             ,across(matches('ZCTA'),function(xx){
                               length(unique(xx))})
                             ,across(where(is.numeric) &
                                       !matches('ACS_TOTAL_POP_WT') ,function(xx) {
                               sum(ACS_TOTAL_POP_WT,na.rm=T)*
                                 sum(xx/pmax(ACS_TOTAL_POP_WT,1),na.rm=T)})
                             ) %>% subset(!is.na(CN)) %>%
  left_join(rsa0,by=c(CN='RSA'));
dct0 <- subset(dct0,column %in% colnames(dat1));

# data prep ----
# Scale the numeric variables
dat2 <- select(dat1,-c('ZCTA','YEAR','Quartile'));
dat2[,sapply(dat2,is.numeric)] <- scale(dat2[,sapply(dat2,is.numeric)]);
# Obtain the numeric-only columns as dat3
dat3 <- ungroup(dat2) %>% select(where(is.numeric));

# factor analysis ----
#' # How many factors to use?
#'
#+ scree, cache=TRUE
scdat3 <- nScree(as.data.frame(dat3),model='factors');
plot(scdat3);
.junk<-capture.output(.nfdat3 <- print(scdat3));
pander(.nfdat3);
#'
#' Looks like it's `r .nfdat3$noc`
#'
#' # Factor analysis
#'
#+ fa, fig.width=10, cache=TRUE
fadat3 <- factanal(select(dat3,-'RSR'),factors=.nfdat3$noc,lower=0.1
                   ,nstart=4,scores='regression',rotation='varimax');
pvdat3 <- colSums(loadings(fadat3)^2)/nrow(loadings(fadat3));
barplot(pvdat3,ylab='Proportion of Variance Explained');
varsdat3 <- apply(loadings(fadat3),2,function(xx) names(xx[xx>0.2]),simplify = F);
varsdat3a <- lapply(varsdat3,function(xx) base::ifelse(xx %in% v(c_domainexpert)
                                                 ,pander::wrap(xx,'**'),xx));
message('About to print factors');
pander(varsdat3a);

# Missing values----
#' # Charcterize missing values
#'
#' WIP: no longer any missing variables in processed data, will need to use data
#' dictionary.
#'
# d1missing <- sapply(dat1,function(xx) sum(is.na(xx)));
# namesd1missing <- names(d1missing[d1missing>0]);
# #' `r length(namesd1missing)` columns have missing values, with
# #' `r sum(is.na(dat1[,namesd1missing]))` missing from a total of
# #' `r length(namesd1missing)*nrow(dat1)` or
# #' `r round(sum(is.na(dat1[,namesd1missing]))/(length(namesd1missing)*nrow(dat1)),3)*100`
# #' %.
# Variable selection ----
#' # Variable Selection
#'
#' ## Method: permutation (Boruta w/ Random Forests) (http://www.jstatsoft.org/v36/i11/)
#+ borutaplot, fig.height=10, cache=TRUE
d1boruta0 <- Boruta(RSR ~ ., data=dat3);
d1boruta1 <- TentativeRoughFix(d1boruta0);
par(mar=c(0.5, 6, 1, 0.5), mgp=c(0, 0.2, 0), cex=0.9, tcl=0.2);
plot(d1boruta1, las=2,xlab="",ylab="", cex.axis=0.4
     ,main="Variable Importance",horizontal=T);
#' ## Method: stepwise bidirectional selection
#'
# Have to exclude CN, possibly only in the sim data. Also have to exclude STATE
# because otherwise hard to interpret in a linear model.
#' ### The _a priori_ model based on domain knowledge
#'
frm_exp0 <- paste(v(c_domainexpert),collapse='+') %>% paste('RSR ~',.) %>%
  as.formula(env = NULL);
pander(frm_exp0);
#+ stepaic, results='hide', cache=TRUE
lmbasedat3 <- lm(RSR~1,data=dat3);
lmstartdat3 <- update(lmbasedat3,formula=frm_exp0);
lmalldat3 <- lm(RSR~.,data=dat3);
#d1lmall <- update(d1lmall,.~.-CN-Quartile-STATE);
aicdat3 <- step(lmstartdat3,scope=list(lower=lmbasedat3,upper=lmalldat3),direction='both');
#' ## Comparison of prioritized variables
#'
#+ comparison
o1 <- tidy(aicdat3) %>% arrange(desc(abs(statistic))) %>%
  select(c('term','statistic')) %>% subset(term!='(Intercept)');
o2 <- attStats(d1boruta1) %>%
  subset(.,decision!='Rejected'|rownames(.) %in% c(o1$term,v(c_domainexpert))) %>%
  arrange(desc(medianImp)) %>% select(medianImp) %>%
  tibble::rownames_to_column('term');
#' ### The following variables were chosen by both methods:
#+ allmethods
.allmethods <- intersect(o1$term,o2$term) %>%
  ifelse(. %in% v(c_domainexpert),pander::wrap(.,'**'),.);
if(length(.allmethods)>0) pander(.allmethods) else pander('None');
#'
#' ### The following variables were chosen by stepwise elimination only:
#+ swonly
.swonly <- setdiff(o1$term,o2$term) %>%
  ifelse(. %in% v(c_domainexpert),pander::wrap(.,'**'),.);
if(length(.swonly)>0) pander(.swonly) else pander('None');
#'
#' ### The following variables were chosen by Boruta/random-forest only:
#+ rfonly
.rfonly <- setdiff(o2$term,o1$term) %>%
  ifelse(. %in% v(c_domainexpert),pander::wrap(.,'**'),.);
if(length(.rfonly)>0) pander(.rfonly) else pander('None');
#'
#' ### Here is a table of all variables selected by either method:
#+ finalcompare
full_join(o2,o1) %>%
  full_join(data.frame(term=v(c_domainexpert),apriori=TRUE)) %>%
  mutate(apriori=coalesce(apriori,FALSE)) %>%
  arrange(desc(apriori),desc(abs(statistic)),desc(medianImp)) %>%
  select(term,apriori,statistic,medianImp) %>%
  setNames(c('','Manually Chosen','t-Statistic','Boruta Importance')) %>%
  pander(row.names=F,missing='-')
