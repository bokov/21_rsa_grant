#' ---
#' title: "Preparation of data for analysis of successful community discharge from rehab by RSAs"
#' author: 'Timothy Reistetter ^1,✉^, Alex Bokov ^1^, Susanne Schmidt ^1^'
#' abstract: |
#'   | Merging and cleanup of CMS and Census data
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' css: 'production.css'
#' ---
#+ init, echo=FALSE, message=FALSE, warning=FALSE,results='hide'
# Init ----
debug <- 0;
knitr::opts_chunk$set(echo=debug>0, warning=debug>0, message=debug>0);
inputdata <- c(dat0='data/SIM_SDOH_ZCTA.xlsx'      # census data by ZCTA
               ,cx0='data/SIM_ALLCMS.csv'          # RSA-ZCTA crosswalk
               ,rsa0='data/SIM_RSAv4 SCD RSRs.csv' # outcomes (RSR)
);

# Load libraries ----
library(GGally);
library(rio);
library(dplyr);
library(pander);
library(mice);
library(Boruta);

# Local project settings ----
if(file.exists('local.config.R')) source('local.config.R',local=T,echo = F);

# Import data ----
dat0 <- import(inputdata['dat0']);
cx0 <- import(inputdata['cx0']);
rsa0 <- import(inputdata['rsa0']);
# temporary fix while rebuilding the simulated files properly
#rsa0 <- group_by(rsa0,RSA,Quartile) %>% summarise(RSR=max(RSR)) %>% select(RSR,RSA,Quartile) %>% data.frame
# Merge data ----
#' # Merging Data
#'
#' `dat1` is the combined dataset, aggregated by RSA (CN).
#'
dat1 <- left_join(cx0,rsa0,by=c("CN"="RSA")) %>% left_join(dat0,.,by="ZCTA") %>%
  group_by(CN) %>% summarise(across(where(is.numeric),median,na.rm=T)
                             ,across(matches('REGION|STATE'),function(xx){
                               paste0(unique(xx),collapse='|')})
                             ,across(matches('ZCTA'),function(xx){
                               length(unique(xx))})) %>% subset(!is.na(CN));
# Missing values----
#' # Charcterize missing values
#'
d1missing <- sapply(dat1,function(xx) sum(is.na(xx)));
namesd1missing <- names(d1missing[d1missing>0]);
#' `r length(namesd1missing)` columns have missing values, with
#' `r sum(is.na(dat1[,namesd1missing]))` missing from a total of
#' `r length(namesd1missing)*nrow(dat1)` or
#' `r round(sum(is.na(dat1[,namesd1missing]))/(length(namesd1missing)*nrow(dat1)),3)*100`
#' %.
# Variable selection ----
#' # Variable Selection
#'
#' ## Method: permutation (Boruta w/ Random Forests) (http://www.jstatsoft.org/v36/i11/)
#+ borutaplot, fig.height=10, cache=TRUE
d1boruta0 <- Boruta(RSR ~ ., data=select(dat1,-all_of(namesd1missing)));
d1boruta1 <- TentativeRoughFix(d1boruta0);
par(mar=c(0.5, 6, 1, 0.5), mgp=c(0, 0.2, 0), cex=0.9, tcl=0.2);
plot(d1boruta1, las=2,xlab="",ylab="", cex.axis=0.4
     ,main="Variable Importance",horizontal=T);
#' ## Method: stepwise bidirectional selection
#'
#' Have to exclude CN, possibly only in the sim data. Also have to exclude STATE
#' because otherwise hard to interpret in a linear model.
#+ stepaic, results='hide'
d1lmbase <- lm(RSR~1,data=select(dat1,-all_of(namesd1missing)));
d1lmall <- lm(RSR~.,data=select(dat1,-all_of(namesd1missing)));
d1lmall <- update(d1lmall,.~.-CN-STATE-REGION);
d1aic <- step(d1lmbase,scope=list(lower=d1lmbase,upper=d1lmall),direction='both');
#' ## Comparison of prioritized variables
#'
o1 <- tidy(d1aic) %>% arrange(desc(abs(statistic))) %>% select(c('term','statistic')) %>% subset(term!='(Intercept)');
o2 <- subset(attStats(d1boruta1),decision!='Rejected') %>% arrange(desc(medianImp)) %>% select(medianImp) %>% tibble::rownames_to_column('term');
#' The following variables were chosen by both methods: `r intersect(o1$term,o2$term) %>% pander()`
#'
#' The following variables were chosen by stepwise elimination only: `r setdiff(o1$term,o2$term) %>% pander()`
#'
#' The following variables were chosen by Boruta/random-forest only: `r setdiff(o2$term,o1$term) %>% pander()`
#'
#' Here is a table of all variables selected by either method:
full_join(o2,o1) %>% setNames(c('','Boruta Importance','t-Statistic')) %>% pander(row.names=F,missing='-')
