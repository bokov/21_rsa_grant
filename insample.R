#' ---
#' title: "Model comparison and evalution, in-sample, for predicting successful community discharge from rehab by RSAs"
#' author: 'Timothy Reistetter ^1^, Alex Bokov ^1^, Susanne Schmidt ^1^, Mei-Lin Min ^1^'
#' abstract: |
#'   | Comparing various proposed predictive models
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
inputdata <- c(dat0='data/SIM_SDOH_ZCTA.xlsx'          # census data by ZCTA
               ,cx0='data/SIM_ALLCMS.csv'              # RSA-ZCTA crosswalk
               ,rsa0='data/SIM_RSAv4 SCD RSRs.csv'     # outcomes (RSR)
               ,dct0='data/data_dictionary.csv'        # data dictionary
               ,dat1='SDOH_RSR_2013_prelim.csv'        # the dat1 dataset
               ,dat2='SDOH_RSR_2013_scaled_prelim.csv' # the scaled version of
);

# Load libraries ----
library(rio); library(dplyr); library(tidbits); # data handling
library(pander); library(broom);                # formatting
#library(GGally);
#library(mice);
library(psych);                                 # factor analysis
library(caret);                                 # cross-validation
library(Boruta);                                # variable selection
library(nFactors);                              # optimal number of factors

# Make tables never split
panderOptions('table.split.table',Inf);
panderOptions('table.split.cells',Inf);

# Local project settings ----
# tweak base plot settings to avoid captions going off-screen
.par_default <- par(no.readonly = TRUE);
.par_borutaplot <- list(mar=c(0.5, 6, 1, 0.5), mgp=c(0, 0.2, 0), cex=0.9,
                        tcl=0.2);
# overwrite previously set values if needed
source('config.R',local=T,echo = debug>0);
if(file.exists('local.config.R')) source('local.config.R',local=T,echo = debug>0);



# Import data ----
# if variable selection not already done, run the script that performs it
if(!file.exists('variableselection.R.rdata')){
  system('R --vanilla -q -s -f variableselection.R',ignore.stdout = debug==0
         ,ignore.stderr = debug==0)};
load('variableselection.R.rdata');


# Model performance ----
#'
#' # SDI
#'
set.seed(project_seed);
sdi3tr <- train(RSR~sdi_score,data=dat3tr,method='lm'
                ,trControl=trainControl(method='repeatedcv',number=5,repeats=10));
#+ sdi3tr, comment=''
sdi3tr;
plot(dat3tr$RSR~predict(sdi3tr,dat3tr),xlab='Predicted',ylab='Observed');
#'
pander(sdi3tr$finalModel,caption='RSR ~ sdi_score');
#' # Stepwise
#'
set.seed(project_seed);
aic3tr <- train(aicdat3$call$formula,data=dat3tr,method='lm'
                ,trControl=trainControl(method='repeatedcv',number=5,repeats=10));
#+ aic3tr, comment=''
aic3tr;
plot(dat3tr$RSR~predict(aic3tr,dat3tr),xlab='Predicted',ylab='Observed');
#'
pander(aic3tr$finalModel,caption=aic3tr$call$form %>% eval %>%
         deparse(width.cutoff = 400));

#' # Manually Chosen
#'
set.seed(project_seed);
exp3tr <- train(lmstartdat3$call$formula,data=dat3tr,method='lm'
                ,trControl=trainControl(method='repeatedcv',number=5,repeats=10));
#+ exp3tr, comment=''
exp3tr;
plot(dat3tr$RSR~predict(exp3tr,dat3tr),xlab='Predicted',ylab='Observed');
#'
pander(exp3tr$finalModel,caption=exp3tr$call$form %>% eval %>% deparse %>%
         paste0(collapse=''));
#' # Using Factor Analysis

dat3trfa <- data.frame(RSR=dat3tr$RSR,fa3$scores);
.fa3form <- colnames(dat3trfa)[-1] %>% paste0(collapse='+') %>%
  paste0('RSR~',.) %>% formula;
set.seed(project_seed);
fa3tr <- train(RSR~.,data=dat3trfa,method='lm'
                ,trControl=trainControl(method='repeatedcv',number=5,repeats=10));
#+ fa3tr, comment=''
fa3tr;
plot(dat3trfa$RSR~predict(fa3tr,dat3trfa),xlab='Predicted',ylab='Observed');
#'
pander(fa3tr$finalModel,caption=reformulate(labels(terms(fa3tr)),'RSR') %>%
         deparse(width.cutoff=400));
#'
#' \
#' \
#' \
#' \
#' \
#' \
#'
#'
#' # Summary
#'
bind_rows(SDI=sdi3tr$results,Stepwise=aic3tr$results,`Manually Selected`=exp3tr$results
          ,`Factor Analysis`=fa3tr$results,.id='Model') %>%
  select(-c('intercept')) %>% arrange(MAE) %>% pander()
