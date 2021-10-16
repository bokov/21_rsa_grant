#' ---
#' title: "Simulation of successful community discharge from rehab by RSAs data"
#' author: 'Timothy Reistetter ^1,✉^, Alex Bokov ^1^'
#' abstract: |
#'   | Using the real data to create a sharaeable simulated dataset for
#'   | development and testing of analysis scripts.
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' ---
#+ init, echo=FALSE, message=FALSE, warning=FALSE,results='hide'
# Init ----
debug <- 0;
knitr::opts_chunk$set(echo=debug>0, warning=debug>0, message=debug>0);
#' # Load libraries
library('dplyr');
library('rio');
# can be obtained via devtools::install_github('bokov/tidbits',ref='integration')
library('tidbits');
#' # Config
# Global project settings ----
inputdata <- c();
source('config.R',local=T,echo=debug>0);
inputdata['cx0'] <- 'local/in/ALLCMSv4.csv'
inputdata['rsa0'] <- 'local/in/RSAv4 SCD RSRs 20210625.csv';

# Local project settings ----
# overwrite previously set global values if needed
if(file.exists('local.config.R')){
  source('local.config.R',local=TRUE,echo = debug>0);
  if(exists('.local.inputdata')){
    inputdata <- replace(inputdata,names(.local.inputdata),.local.inputdata)};
};

#' # Import data
#'
#' Get all the SDOH files from AHRQ
if(!file.exists('SDOH_ZCTA.rdata')){
  orig0 <- sapply(sprintf('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/SDOH_ZCTA_20%d.xlsx',11:18)
                  ,import,simplify=F);
  save(orig0,file='SDOH_ZCTA.rdata')} else orig0 <- import('SDOH_ZCTA.rdata');
#' Find the variables conserved across all the years and use only those
sharedVars <- sapply(orig0[-(1:2)],names) %>% Reduce(intersect,.,unlist(head(.,1)));
orig1 <- bind_rows(orig0) %>% select(all_of(sharedVars)) %>% group_by(ZCTA);
#' Import the RSA-ZCTA mapping (cx0) and RSR by RSA outcome (rsa0)
cx0 <- import(inputdata['cx0']);
rsa0 <- import(inputdata['rsa0']);
#' Simulate the SDOH variables (sampling randomly within ZCTAs over all
#' available years)
set.seed(project_seed);
sim0 <- summarise(orig1,across(.fns=~sample(.x,1)));
sim1 <-left_join(cx0,rsa0,by=c("CN"="RSA")) %>% left_join(sim0,.,by="ZCTA") %>%
  subset(!is.na(RSR)) %>% mutate(YEAR=2099);
#' For simulating values, just keep the numeric columns and fill NAs with
#' medians
sim2 <- select(sim1,where(is.numeric)) %>% select(-YEAR) %>%
  mutate(across(.fns=~ifelse(is.na(.x),median(.x,na.rm=T),.x)));
#' Now simulate the RSRs
sim1$predRSR <- predict(lm(RSR~.,sim2));
#'
#' ## Simulated, RSA-indexed outcomes (scrambling RSA names)
set.seed(project_seed);
rsascramble <- paste0(sample(LETTERS[-24],25),collapse='');
cx1 <- mutate(cx0,CN=chartr(CN,old=paste0(LETTERS[-24],collapse='')
                            ,new=rsascramble));
rsa1 <- mutate(sim1,RSA=CN,RSR=predRSR) %>% group_by(RSA) %>%
  summarise(RSR=mean(RSR,na.rm=T)) %>%
  mutate(Quartile=cut(RSR,breaks=c(0,quantile(RSR,c(.25,.5,.75),na.rm=T),Inf)
                      ,labels=c('Q1','Q2','Q3','Q4')) %>% as.character
         ,RSA = chartr(RSA,old=paste0(LETTERS[-24],collapse='')
                       ,new=rsascramble)) %>%
  na.omit %>% select(names(rsa0));
#' # Export data
#'
#' ## Census data
select(sim1,names(orig1)) %>% arrange(STATE,ZCTA) %>%
  export("data/SIM_SDOH_ZCTA.xlsx",overwrite=T);
#' ## Outcomes
export(rsa1,'data/SIM_RSAv4 SCD RSRs.csv');
#' ## Crosswalk
export(cx1,'data/SIM_ALLCMS.csv');
system('R --vanilla -q -s -f data.R',ignore.stdout = debug==0
       ,ignore.stderr = debug==0,);
file.rename('SDOH_RSR_2013_prelim.csv','SIM_SDOH_RSR_201X.csv');
file.rename('SDOH_RSR_2013_scaled_prelim.csv','SIM_SDOH_RSR_201X_scaled.csv');
