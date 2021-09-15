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
library('synthpop');
# can be obtained via devtools::install_github('bokov/tidbits',ref='integration')
library('tidbits');
#' # Config
source('config.R');
inputdata <- c(orig0='/tmp/SDOH_ZCTA_2013.xlsx'
               ,cx0='/tmp/mozilla_a0/ALLCMSv4.csv'
               ,rsa0='/tmp/mozilla_a0/RSAv4 SCD RSRs 20210625.csv');
#' # Import data
orig0 <- import(inputdata['orig0']);
cx0 <- import(inputdata['cx0']);
rsa0 <- import(inputdata['rsa0']);
dat0 <-left_join(cx0,rsa0,by=c("CN"="RSA")) %>% left_join(orig0,.,by="ZCTA");
#' # Create codebook
orig0cb <- codebook.syn(dat0);
#' # Simulations
#'
#' ## All data
sim0 <- select(dat0,-c("Quartile","REGION","STATE","CN","ZCTA")) %>% syn;
sim1 <- select(dat0,c('ZCTA','REGION','STATE','CN')) %>%
  mutate(cntemp=rank(RSR,ties='random')) %>%
  left_join(mutate(sim0$syn,cntemp=rank(RSR,ties='random')),.);
#compare(sim0,dat0);
sim1$Quartile <- with(sim1
                   ,cut(RSR,breaks=c(0,quantile(RSR,c(.25,.5,.75),na.rm=T),Inf)
                        ,labels=c('Q1','Q2','Q3','Q4')));
sim1 <- arrange(sim0$syn,STATE) %>%
  mutate(.,ZCTA=sprintf('%05d',sample(0:99999,nrow(.)))
         ,STATE=as.character(STATE),REGION=as.character(REGION)
         ,CN=as.character(factor(CN
                                 ,labels=sprintf('A%04d'
                                                 ,cumsum(sample(1:5
                                                                ,length(levels(CN))
                                                     ,rep=T)))))
         ,RSA=CN
         ,Quartile=as.character(Quartile));
#' ## Simulated, RSA-indexed outcomes
rsascramble <- paste0(sample(LETTERS[-24],25),collapse='');
cx1 <- mutate(cx0,CN=chartr(CN,paste0(LETTERS[-24],collapse=''),rsascramble));
rsa1 <- rename(sim1,RSA=CN) %>% group_by(RSA) %>%
  summarise(RSR=mean(RSR,na.rm=T)) %>%
  mutate(Quartile=cut(RSR,breaks=c(0,quantile(RSR,c(.25,.5,.75),na.rm=T),Inf)
                      ,labels=c('Q1','Q2','Q3','Q4')) %>% as.character
         ,RSA = chartr(RSA,paste0(LETTERS[-24],collapse=''),rsascramble)) %>%
  na.omit %>% select(names(rsa0));

#' # Export data
#'
#' ## Census data
select(sim1,names(orig0)) %>% arrange(STATE,ZCTA) %>%
  export("data/SIM_SDOH_ZCTA.xlsx",overwrite=T);
#' ## Outcomes
export(rsa1,'data/SIM_RSAv4 SCD RSRs.csv');
#' ## Crosswalk
export(cx1,'data/SIM_ALLCMS.csv');
