#' ---
#' title: "Download of raw source data for analysis of successful community discharge from rehab by RSAs"
#' abstract: |
#'   | Download of all beta and production versions of CMS and Census data
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
# Global project settings ----
inputdata <- c();
source('config.R',local=T,echo=debug>0);
throttle <- 60; # the amount of time to wait between downloads to avoid being cancelled

# Load libraries ----
library(rio); library(dplyr); library(tidbits);  # data handling
library(DescTools);                              # some useful commands
library(RCurl);
#library(metamedian);                             # median-of-medians01
old_data_years <- 18:11;
new_data_years <- 20:11;

# Set because apparently now AHRQ wants to prevent tax-funded scientists from
# accessing their data programmatically. Shame on them.
options(HTTPUserAgent = useragent);

# Local project settings ----
# overwrite previously set global values if needed
if(file.exists('local.config.R')){
  source('local.config.R',local=TRUE,echo = debug>0);
  message('Found local.config.R, loading local settings');
  if(exists('.local.inputdata')){
    message('Found .local.inputdata object, merging into inputdata');
    inputdata <- replace(inputdata,names(.local.inputdata),.local.inputdata)};
};

dir.create(download_dir,recursive = T,showWarnings = F);

fn_modalvals <- function(xx,collapse=';'){
  paste0(sort(if(anyDuplicated(xx)==0) xx else Mode(xx,na.rm=T)),collapse=collapse)
  };
# Import data ----

#' Downloading and importing AHRQ ZCTA data (beta, deprecated)
if(!file.exists('data/SDOH_ZCTA.rdata')){
  sdohzcta <- sapply(sprintf('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/SDOH_ZCTA_20%d.xlsx',old_data_years)
                  ,function(xx){
                    xxtemp <- file.path(download_dir,basename(xx));
                    if(!file.exists(xxtemp)){
                      download.file(xx,destfile = xxtemp);
                      Sys.sleep(throttle); #20 was too short
                    }
                    try(import(xxtemp));
                    },simplify=F);
  failed<-Filter(function(xx) is(xx,'try-error'),sdohzcta) %>% names()
  if(length(failed)==0) save(sdohzcta,file = 'data/SDOH_ZCTA.rdata');
} else if(debug) sdohzcta <- import('data/SDOH_ZCTA.rdata');

# Downloading and importing AHRQ SDOH codebook (beta, deprecated)
if(!file.exists('AHRQ_SDOH_codebook.xlsx')){
  #writeBin(getBinaryURL('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/sdoh_codebook_final.xlsx'),'AHRQ_SDOH_codebook.xlsx');
  download.file('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/sdoh_codebook_final.xlsx',destfile = 'AHRQ_SDOH_codebook.xlsx')};

sdohdctzcta <- sapply(paste0('ZCTA_20',old_data_years),function(xx){
  import('AHRQ_SDOH_codebook.xlsx',which=xx)},simplify=F);
#' Obtain the CN/RSA - ZCTA mappings
cx0 <- import(inputdata['cx0']);
#' Obtain the RSR outcomes
rsa0 <- import(inputdata['rsa0']) %>% transmute(RSR,RSA,RSA_bin=Value) ;
#' Column groupings, for the custom v() command
dct0 <- import(inputdata['dct0']);
colmap <- import(inputdata['colmap']) %>% with(setNames(new,old));

#' Download and import AHRQ ZIPCODE SDOH data (current version)
if(!file.exists('data/SDOH_ZIP.rdata')){
  sdohzip <- sapply(sprintf('https://www.ahrq.gov/sites/default/files/wysiwyg/sdoh/SDOH_20%d_ZIPCODE_1_0.xlsx',new_data_years)
                    ,function(xx){
                      xxtemp <- file.path(download_dir,basename(xx));
                      if(!file.exists(xxtemp)){
                        download.file(xx,destfile = xxtemp);
                        Sys.sleep(throttle); #20 was too short
                      }
                      try(import(xxtemp,which='Data'));
                    },simplify=F);
  # Save the R object containing all available SDOH new-version files
  failed<-Filter(function(xx) is(xx,'try-error'),sdohzip) %>% names() %>% c(failed);
  if(length(failed)>0){
    message('
The following files failed to download likely due to bandwidth throttling.
Please try waiting a few minutes and then re-running this script. Or you
can download the below files manually to ',download_dir,'...

',paste0(failed,collapse='\n'));
  } else save(sdohzip,file = 'data/SDOH_ZIP.rdata');
} else sdohzip <- import('SDOH_ZIP.rdata');

# Identify the SDOH file for the year we will be using, to save separately.
sdohzipselected <- grep(sprintf('SDOH_%s_ZIPCODE',sdohyear),names(sdohzip),val=T);
# crosswalk that file to RSA using the ZCTA field that both have, discarding
# any ZCTAs that are not present in both data frames.
sdohrsazip <- sdohzip[[sdohzipselected]] %>% inner_join(cx0) %>%
  rename(RSA=CN) %>%
  mutate(across(any_of(c('ZCTA','ZIPCODE')),~sprintf("'%s'",.x))) %>%
  inner_join(rsa0,by='RSA');

sdohrsazip_oldnames <- rename(sdohrsazip,any_of(colmap));
message(sprintf('Number of ZCTAs omitted from SDOH file: %s',length(setdiff(sprintf("'%s'",sdohzip[[sdohzipselected]]$ZCTA),sdohrsazip$ZCTA))));
message(sprintf('Number of ZCTAs omitted from crosswalk file: %s',length(setdiff(sprintf("'%s'",cx0$ZCTA),c('XXXXX',sdohrsazip$ZCTA)))));
message(sprintf('Number of RSAs omitted from crosswalk file: %s',length(setdiff(cx0$CN,c('XXXXX',sdohrsazip$RSA)))));
# Save the SDOH file that will be analyzed as CSV, for sharing with group
sdohrsazipfilename <- sdohzipselected %>% basename %>% gsub('\\.xlsx','.csv',.) %>% paste0('RSA_',.) %>% file.path('data',.);
# export(sdohrsazip,file=sdohrsazipfilename);
message('Analysis-ready file has been saved to ',normalizePath(sdohrsazipfilename));
sdohrsazipfilename_oldnames <- gsub('.csv','_oldnames.csv',sdohrsazipfilename);
export(sdohrsazip_oldnames,file=sdohrsazipfilename_oldnames);
message('Analysis-ready file has been saved to ',normalizePath(sdohrsazipfilename_oldnames));


#' Downloading and importing AHRQ SDOH codebook (current version)
sdohdctzip0 <- sapply(sprintf('https://www.ahrq.gov/sites/default/files/wysiwyg/sdoh/SDOH_20%d_Codebook_1_0.xlsx',11:20)
                     ,function(xx){
                       xxtemp <- file.path(tempdir(),basename(xx));
                       Sys.sleep(throttle);
                       download.file(xx,destfile = xxtemp);
                       try(import(xxtemp,which='ZIPCODE'));
                     },simplify=F);
failed<-Filter(function(xx) is(xx,'try-error'),sdohdctzip0) %>% names()
if(length(failed)>0) message('The following files failed to download likely due to bandwidth throttling. Please download them manually:\n',paste0(failed,collapse='\n'))

c()