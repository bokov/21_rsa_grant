#' ---
#' title: "Preparation of data for analysis of successful community discharge from rehab by RSAs"
#' author: 'Timothy Reistetter ^1^, Alex Bokov ^1^, Susanne Schmidt ^1^, Mei-Lin Min ^1^'
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
inputdata <- c(dat0='data/SIM_SDOH_ZCTA.xlsx'          # census data by ZCTA
               ,cx0='data/SIM_ALLCMS.csv'              # RSA-ZCTA crosswalk
               ,rsa0='data/SIM_RSAv4 SCD RSRs.csv'     # outcomes (RSR)
               ,dct0='data/data_dictionary.csv'        # data dictionary for the
                                                       # dat1 dataset that _this_
                                                       # scriport produces
               ,dat1='SDOH_RSR_2013_prelim.csv'        # the dat1 dataset
               ,dat2='SDOH_RSR_2013_scaled_prelim.csv' # the scaled version of
                                                       # dat1
);

# Load libraries ----
library(rio); library(dplyr); library(tidbits);  # data handling
library(RCurl);
library(metamedian);                             # median-of-medians01


# Project settings ----
# global project settings
source('config.R',local=T,echo=debug>0);
# overwrite previously set values if needed
if(file.exists('local.config.R')) source('local.config.R',local=T,echo = debug>0);

# Local functions ----

# Percents. Use with den=ACS_TOTAL_POP_WT for c_pct columns. For c_per1k, use
# the same function but with per=1000. For c_byarea columns, use
# den=CEN_AREALAND_SQM and per=1
.aggpct <-function(num,den,per=100) per*sum(num*pmax(den,1,na.rm=T)/per,na.rm=T)/sum(den,na.rm=T);
# Medians. For c_median columns, use den=ACS_TOTAL_POP_WT but for c_medianhh
# columns use den=ACS_TOTAL_HOUSEHOLD
.aggmed <-function(num,den,...) if(all(is.na(num))) return(NA) else {
  pool.med(num,pmax(den,1,na.rm=T))$pooled.est};
# Means
# For c_meanhh, den= ACS_TOTAL_HOUSEHOLD
.aggmean <-function(num,den,...) weighted.mean(num,coalesce(den,0),na.rm=T);
# Per capita
.aggpcap <-function(num,den,...) {out <- try(sum(num*den,na.rm=T)/sum(den,na.rm=T));
if(is(out,'try-error')) browser(); out;}
# Largest by area (for discrete values)
.aggmaxa <-function(num,den,...) rep.int(num,den) %>% table %>% sort %>% rev %>%
  head(1) %>% names;

# Import data ----
# Downloading and importing AHRQ ZCTA data, which presumably we will eventually use
if(!file.exists('SDOH_ZCTA.rdata')){
  sdoh0 <- sapply(sprintf('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/SDOH_ZCTA_20%d.xlsx',13:18)
                  ,import,simplify=F);
  save(sdoh0,file = 'SDOH_ZCTA.rdata');
} else load('SDOH_ZCTA.rdata');
# Downloading and importing AHRQ SDOH codebook, which presumably we will eventually use
if(!file.exists('AHRQ_SDOH_codebook.xlsx')){
  writeBin(getBinaryURL('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/sdoh_codebook_final.xlsx')
           ,'AHRQ_SDOH_codebook.xlsx')};
sdohdct0 <- sapply(paste0('ZCTA_20',13:18),function(xx){
  import('AHRQ_SDOH_codebook.xlsx',which=xx)},simplify=F);
# Downloading and importing the Social Deprivation Index
if(!file.exists('ACS2015_zctaallvars.xlsx')){
  writeBin(getBinaryURL('https://www.graham-center.org/content/dam/rgc/documents/maps-data-tools/sdi/ACS2015_zctaallvars.xlsx'
                        ,ssl.verifypeer=F,ssl.verifyhost=F)
           ,'ACS2015_zctaallvars.xlsx')};
sdi <- import('ACS2015_zctaallvars.xlsx') %>%
  mutate(ZCTA=as.character(zcta)) %>% select(-'zcta') %>%
  subset(!is.na(sdi_score));
# Importing the local data files
dat0 <- import(inputdata['dat0']);
cx0 <- import(inputdata['cx0']);
rsa0 <- import(inputdata['rsa0']);
dct0 <- import(inputdata['dct0']);
# Merge data ----
#' # Merging and Preparing Data
#'
#' `dat1` is the combined dataset, aggregated by RSA (`CN`) and weighted by
#' `ACS_TOTAL_POP_WT`.
#' The `dat2` dataset is `dat1` with numeric values scaled and the following
#' columns omitted: `ZCTA`, `YEAR`, and `Quartile`.
#' Finally, `dat3tr` is the just the numeric columns from the `dat2` dataset.
#+ dat1,cache=F
.c_numericother <- setdiff(v(c_numeric,dat=dat0)
                           ,c(v(c_pct),v(c_per1k),v(c_byarea),v(c_median)
                              ,v(c_medianhh),v(c_total),v(c_percap),v(c_meanhh)
                              ,v(c_factor))) %>% c('sdi_score');
set.seed(project_seed);
dat1 <- inner_join(dat0,cx0,by="ZCTA") %>%
  left_join(sdi[,c('ZCTA','sdi_score')] ,by="ZCTA") %>%
  group_by(CN,YEAR) %>% summarise(
    # using the aggregation functions declared above on their respective sets of columns
    across(.cols=v(c_pct,dat=(.)),.aggpct,den=ACS_TOTAL_POP_WT)
    ,across(.cols=v(c_per1k,dat=(.)),.aggpct,den=ACS_TOTAL_POP_WT,per=1000)
    ,across(.cols=v(c_byarea,dat=(.)),.aggpct,den=CEN_AREALAND_SQM,per=1)
    ,across(.cols=v(c_median,dat=(.)),.aggmed,den=ACS_TOTAL_POP_WT)
    ,across(.cols=v(c_medianhh,dat=(.)),.aggmed,den=ACS_TOTAL_HOUSEHOLD)
    ,across(.cols=v(c_meanhh,dat=(.)),.aggmean,den=ACS_TOTAL_HOUSEHOLD)
    ,across(.cols=v(c_percap,dat=(.)),.aggpcap,den=ACS_TOTAL_POP_WT)
    ,across(.cols=all_of(.c_numericother),.aggmean,den=ACS_TOTAL_POP_WT)
    ,across(.cols=v(c_factorarea,dat=(.)),.aggmaxa,den=CEN_AREALAND_SQM)
    # The c_total group of columns needs to get aggregated last because their
    # de-aggregated forms may be needed for the other aggregations. Maybe it
    # won't make a difference but decided to play it safe for no.
    ,across(.cols=v(c_total,dat=(.)),sum,na.rm=T)
    ) %>% subset(!is.na(CN)) %>%
  left_join(rsa0,by=c(CN='RSA')) %>% ungroup %>%
  mutate(across(v(c_factor,dat=(.)),factor)
         ,sdi_score=ifelse(is.na(sdi_score)
                           ,median(sdi_score,na.rm=T),sdi_score)
         ,Quintile=cut(RSR,breaks=quantile(RSR,seq(0,1,len=6))
                       ,include.lowest=T,labels=paste0('Q',1:5))
         ,Decile=cut(RSR,breaks=quantile(RSR,seq(0,1,len=11))
                     ,include.lowest=T,labels=paste0('Q',1:10))
         # this column will be used to assign rows to the training and the testing sets
         ,subsample=sample(c('train','test'),n(),replace = TRUE)
         );


dct0 <- subset(dct0,column %in% colnames(dat1));

# data prep ----
# Scale the numeric variables
dat2 <- select(dat1,-c('YEAR','Quartile',v(c_factor)));
dat2[,sapply(dat2,is.numeric)] <- scale(dat2[,sapply(dat2,is.numeric)]);
export(dat1,inputdata['dat1']);
export(dat2,inputdata['dat2']);

