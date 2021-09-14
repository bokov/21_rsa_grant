#' ---
#' title: "Preparation of data for analysis of successful community discharge from rehab by RSAs"
#' author: 'Timothy Reistetter ^1,✉^, Alex Bokov ^1^', Susanne Schmidt ^1^
#' abstract: |
#'   | Merging and cleanup of CMS and Census data
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
knitr::opts_chunk$set(echo=debug>0, warning=debug>0, message=debug>0);
debug <- 0;
inputdata <- c(dat0='data/SIM_SDOH_ZCTA.xlsx'      # census data by ZCTA
               ,cx0='data/SIM_ALLCMS.csv'          # RSA-ZCTA crosswalk
               ,rsa0='data/SIM_RSAv4 SCD RSRs.csv' # outcomes (RSR)
);

# Load libraries ----
library(GGally);
library(rio);
library(dplyr);
library(pander);

# Local project settings ----
if(file.exists('local.config.R')) source('local.config.R',local=T,echo = F);

# Import data ----
dat0 <- import(inputdata['dat0']);
cx0 <- import(inputdata['cx0']);
rsa0 <- import(inputdata['rsa0']);

# Merge data ----
dat1 <- left_join(cx0,rsa0,by=c("CN"="RSA")) %>% left_join(dat0,.,by="ZCTA");
