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
library(metamedian);                            # median-of-medians01


# Local project settings ----
# tweak base plot settings to avoid captions going off-screen
.par_default <- par(no.readonly = TRUE);
.par_borutaplot <- list(mar=c(0.5, 6, 1, 0.5), mgp=c(0, 0.2, 0), cex=0.9,
                       tcl=0.2);
# overwrite previously set values if needed
if(file.exists('local.config.R')) source('local.config.R',local=T,echo = F);

# Local functions ----

# Perecents. Use with den=ACS_TOTAL_POP_WT for c_pct columns. For c_per1k, use
# the same function but with per=1000. For c_byarea columns, use
# den=CEN_AREALAND_SQM and per=1
.aggpct <-function(num,den,per=100) per*sum(num*pmax(den,1)/per,na.rm=T)/sum(den,na.rm=T);
# Medians. For c_median columns, use den=ACS_TOTAL_POP_WT but for c_medianhh
# columns use den=ACS_TOTAL_HOUSEHOLD
.aggmed <-function(num,den,...) pool.med(num,pmax(den,1))$pooled.est;
# Means
# For c_meanhh, den= ACS_TOTAL_HOUSEHOLD
.aggmean <-function(num,den,...) weighted.mean(num,coalesce(den,0),na.rm=T);
# Per capita
.aggpcap <-function(num,den,...) sum(num*den,na.rm=T)/sum(den,na.rm=T);
# Largest by area (for discrete values)
.aggmaxa <-function(num,den,...) rep.int(num,den) %>% table %>% sort %>% rev %>%
  head(1) %>% names;

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
#+ dat1,cache=debug<=0
.c_numericother <- setdiff(v(c_numeric,dat=dat0)
                           ,c(v(c_pct),v(c_per1k),v(c_byarea),v(c_median)
                              ,v(c_medianhh),v(c_total),v(c_percap),v(c_meanhh)
                              ,v(c_factor)));
dat1 <- left_join(dat0,cx0,by="ZCTA") %>%
  group_by(CN,YEAR) %>% summarise(
    across(.cols=v(c_pct,dat=(.)),.aggpct,den=ACS_TOTAL_POP_WT)
    ,across(.cols=v(c_per1k,dat=(.)),.aggpct,den=ACS_TOTAL_POP_WT,per=1000)
    ,across(.cols=v(c_byarea,dat=(.)),.aggpct,den=CEN_AREALAND_SQM,per=1)
    ,across(.cols=v(c_median,dat=(.)),.aggmed,den=ACS_TOTAL_POP_WT)
    ,across(.cols=v(c_medianhh,dat=(.)),.aggmed,den=ACS_TOTAL_HOUSEHOLD)
    ,across(.cols=v(c_meanhh,dat=(.)),.aggmean,den=ACS_TOTAL_HOUSEHOLD)
    ,across(.cols=v(c_percap,dat=(.)),.aggpcap,den=ACS_TOTAL_POP_WT)
    ,across(.cols=all_of(.c_numericother),.aggmean,den=ACS_TOTAL_POP_WT)
    ,across(.cols=v(c_factorarea,dat=(.)),.aggmaxa,den=CEN_AREALAND_SQM)
    ,across(.cols=v(c_total,dat=(.)),sum,na.rm=T)
    # across(matches('ACS_TOTAL_POP_WT'),sum,na.rm=T)
    #                          ,across(matches('REGION|STATE'),function(xx){
    #                            paste0(unique(xx),collapse='|')})
    #                          ,across(matches('ZCTA'),function(xx){
    #                            length(unique(xx))})
    #                          ,across(where(is.numeric) &
    #                                    !matches('ACS_TOTAL_POP_WT') ,function(xx) {
    #                            sum(ACS_TOTAL_POP_WT,na.rm=T)*
    #                              sum(xx/pmax(ACS_TOTAL_POP_WT,1),na.rm=T)})
                             ) %>% subset(!is.na(CN)) %>%
  left_join(rsa0,by=c(CN='RSA')) %>% ungroup %>%
  mutate(across(v(c_factor,dat=(.)),factor));
dct0 <- subset(dct0,column %in% colnames(dat1));

# data prep ----
# Scale the numeric variables
dat2 <- select(dat1,-c('YEAR','Quartile'));
dat2[,sapply(dat2,is.numeric)] <- scale(dat2[,sapply(dat2,is.numeric)]);
# Obtain the numeric-only columns as dat3
dat3 <- select(dat2,where(is.numeric));

# factor analysis ----
#' # How many factors to use?
#'
#+ scree, cache=debug<=0
scdat3 <- nScree(as.data.frame(dat3),model='factors');
plot(scdat3);
.junk<-capture.output(.nfdat3 <- print(scdat3));
pander(.nfdat3);
#'
#' Looks like it's `r .nfdat3$noc`
#'
#' # Factor analysis
#'
#+ fa, cache=debug<=0
fadat3 <- factanal(select(dat3,-'RSR'),factors=.nfdat3$noc,lower=0.1
                   ,nstart=8,scores='regression',rotation='varimax');
#+ faplot, fig.width=10
pvdat3 <- colSums(loadings(fadat3)^2)/nrow(loadings(fadat3));
names(pvdat3) <- scales::percent(pvdat3,accuracy=0.1) %>%
  paste0(names(.),', ',.);
par(mar=c(7,4.1,4.1,2.1));
barplot(pvdat3,ylab='Proportion of Variance Explained',las=2);
par(.par_default);
varsdat3 <- apply(loadings(fadat3),2,function(xx){
  names(xx[xx>0.2])},simplify = F) %>% setNames(names(pvdat3));
varsdat3a <- lapply(varsdat3,function(xx) base::ifelse(xx %in% v(c_domainexpert)
                                                 ,pander::wrap(xx,'**'),xx)) ;
#message('About to print factors');
#'
#+ factorlist
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
#+ borutacalc, cache=debug<=0
d3boruta0 <- Boruta(RSR ~ ., data=dat3);
d3boruta1 <- TentativeRoughFix(d3boruta0);
#+ borutaplot, fig.height=14
.borutameds <- apply(d3boruta1$ImpHistory,2,function(xx){
  median(xx[is.finite(xx)],na.rm=T)}) %>% sort;
.borutanames0 <- names(.borutameds);
.borutatf <- .borutanames0 %in% v(c_domainexpert);
.borutanames1 <- ifelse(.borutatf,'',.borutanames0);
.borutanames2 <- ifelse(!.borutatf,'',.borutanames0);
.borutahilights <- ifelse(.borutatf,'darkviolet','darkgray');
.borutapos <- seq_along(.borutameds)[.borutatf];
par(.par_borutaplot);
plot(d3boruta1, las=2,xlab="",ylab="", cex.axis=0.4
     ,main="Variable Importance",horizontal=T,border=.borutahilights
     ,names=.borutanames1);
axis(2,at=.borutapos,labels=.borutanames0[.borutapos],font = 2,las=2
     ,cex.axis=0.4);
segments(-20,.borutapos,.borutameds[.borutatf],col="#9400D350",lwd=8);
#abline(h=.borutapos,lty=3);
par(.par_default);
#' ## Method: stepwise bidirectional selection
#'
# Have to exclude CN, possibly only in the sim data. Also have to exclude STATE
# because otherwise hard to interpret in a linear model.
#' ### The _a priori_ model based on domain knowledge
#'
frm_exp0 <- paste(v(c_domainexpert,dat3),collapse='+') %>% paste('RSR ~',.) %>%
  as.formula(env = NULL);
pander(frm_exp0);
#+ stepaic, results='hide', cache=debug<=0
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
o2 <- attStats(d3boruta1) %>%
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
full_join(o2,o1,by=c(term='term')) %>%
  full_join(data.frame(term=v(c_domainexpert),apriori=TRUE),by=c(term='term')) %>%
  mutate(apriori=coalesce(apriori,FALSE)) %>%
  arrange(desc(apriori),desc(abs(statistic)),desc(medianImp)) %>%
  select(term,apriori,statistic,medianImp) %>%
  setNames(c('','Manually Chosen','t-Statistic','Boruta Importance')) %>%
  pander(row.names=F,missing='-')
