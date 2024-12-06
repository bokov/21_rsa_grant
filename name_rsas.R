#' ---
#' title: "Download mapping files and assign unique geographic names to RSAs"
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

# Load libraries ----
library(rio); library(dplyr); library(tidbits);  # data handling
library(DescTools);                              # some useful commands
library(RCurl);
#library(metamedian);                             # median-of-medians01
library(stringdist);                             # to find closest matching names
library(english);
library(stringr);

sdohzipselected <- 'https://www.ahrq.gov/sites/default/files/wysiwyg/sdoh/SDOH_2018_ZIPCODE_1_0.xlsx';
sdohzctaselected <- 'https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/SDOH_ZCTA_2018.xlsx';

# Local project settings ----
# overwrite previously set global values if needed
if(file.exists('local.config.R')){
  source('local.config.R',local=TRUE,echo = debug>0);
  message('Found local.config.R, loading local settings');
  if(exists('.local.inputdata')){
    message('Found .local.inputdata object, merging into inputdata');
    inputdata <- replace(inputdata,names(.local.inputdata),.local.inputdata)};
};

# custom functions ----
fn_modalvals <- function(xx,collapse=';'){
  paste0(sort(if(anyDuplicated(xx)==0) xx else Mode(xx,na.rm=T)),collapse=collapse)
};

fn_plurality <- function(xx,weights='POP',collapse='-',ignore=c(weights,'CN'),missing=NA){
  if(cur_column() %in% ignore || length(unique(na.omit(xx)))<2) return(coalesce(max(na.omit(xx)),missing)) else {
    # sum up the weights column grouped by distinct values of xx and find the
    # maximal or modal values
    out <- split(pick(weights)[[1]],xx) %>% sapply(sum,na.rm=T) %>% {.[.==max(.)]} %>%
      # obtain and sort their names
      names %>% setdiff('') %>% sort %>% paste0(collapse=collapse);
    if(length(out)>1) browser();
    #if(grepl('-',out)) browser();
    out;
  }
}

fn_automapper <- . %>% gsub('STAMP','STMP',.) %>% gsub('TOTAL','TOT',.) %>%
  gsub('INCOME','INC',.) %>% gsub('RENTED','RENTER',.) %>%
  gsub('OWNED','OWNER',.) %>% gsub('VA_','VET_',.) %>% gsub('POOR','POV',.) %>%
  gsub('^ACS_PCT_|^ACS_MEDIAN_|_ZC$|_18_64_ZC$','',.) %>%
  gsub('^ACS_','',.) %>% gsub('_MEDIAN_','',.) %>%
  gsub('^CCBP_RATE','CCBP_TOT',.) %>% gsub('_PER_1000','_ZP',.) %>%
  gsub('_BELOW64','64',.) %>% gsub('CFHEORS','EORS',.);

fn_zapambiguous <- function(xx) ifelse(xx>min(xx,na.rm=T),NA,xx);

fn_replace_trailing_numbers <- function(xx) {
  str_replace(xx, "\\b(\\d+)$", function(mm) as.character(str_to_title(english::english(as.numeric(mm)))))
};


# Import data ----

#' Downloading and importing AHRQ ZCTA data (beta, deprecated)
if(!all(file.exists(c('data/SDOH_ZCTA.rdata','data/SDOH_ZIP.rdata')))) system('R download_data.R',wait=T);
sdohzcta <- import('data/SDOH_ZCTA.rdata');
sdohzip <- import('data/SDOH_ZIP.rdata');
rsa2cmsregion <- import('data/RSA2CMSregion.tsv');
#sdohrsazip <- import(sprintf('data/RSA_SDOH_%s_ZIPCODE_1_0.csv',sdohyear));
sdohrsazip <- sdohzip$`https://www.ahrq.gov/sites/default/files/wysiwyg/sdoh/SDOH_2020_ZIPCODE_1_0.xlsx` ;

#' Obtain the CN/RSA - ZCTA mappings
cx0 <- import(inputdata['cx0']);
#' Obtain the RSR outcomes
rsa0 <- import(inputdata['rsa0']) %>% transmute(RSR,RSA,RSA_bin=Value) ;
#' Column groupings, for the custom v() command
dct0 <- import(inputdata['dct0']);
colmap <- import(inputdata['colmap']) %>% with(setNames(new,old));
cntcodes <- import('https://www2.census.gov/programs-surveys/popest/geographies/2020/all-geocodes-v2020.xlsx',skip=4) %>%
  {setNames(.,make.names(gsub('[)(]','',names(.))))} %>% mutate(COUNTY=paste0(State.Code.FIPS,County.Code.FIPS));
statecodes <-import('https://www2.census.gov/geo/docs/reference/state.txt');

automapper <- . %>% gsub('STAMP','STMP',.) %>% gsub('TOTAL','TOT',.) %>%
  gsub('INCOME','INC',.) %>% gsub('RENTED','RENTER',.) %>%
  gsub('OWNED','OWNER',.) %>% gsub('VA_','VET_',.) %>% gsub('POOR','POV',.) %>%
  gsub('^ACS_PCT_|_ZC$','',.);


ruca <- import('https://www.ers.usda.gov/webdocs/DataFiles/53241/RUCA2010zipcode.xlsx?v=5559.8',which='Data') %>%
  transmute(ZIPCODE=ZIP_CODE,STATE,RUCA=as.character(RUCA1)
            ,RuralUrban=case_match(RUCA1
                                   ,1:3 ~ 'Urban core'
                                   ,4:6 ~ 'Suburban'
                                   ,7:9 ~ 'Large rural'
                                   ,10 ~ 'Small town/rural'
                                   ,99 ~ 'NC'));
# rucadct <- import('https://www.ers.usda.gov/webdocs/DataFiles/53241/RUCA2010zipcode.xlsx?v=5559.8',which='RUCA code description',range='A1:A12')[,1] %>% gsub('^([0-9]*)\\s*','\\1=',.) %>% strsplit('=') %>% do.call(rbind,.) %>% data.frame %>% setNames(c('RUCA','description')) %>% mutate(description=trimws(description));
cnt2metro <- import('https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2023/delineation-files/list1_2023.xlsx',skip=2) %>%
  subset(!is.na(`FIPS County Code`) & !is.na(`FIPS State Code`)) %>%
  mutate(COUNTY=paste0(`FIPS State Code`,`FIPS County Code`)) %>%
  setNames(.,make.names(names(.)));
# The following require/s a (free) login and can be downloaded from https://www.huduser.gov/apps/public/uspscrosswalk/home
# zip to CBSA
zip2cbsa <- import('data/ZIP_CBSA_032024.xlsx');
# zip to county
zip2cnt <- import('data/ZIP_COUNTY_032024.xlsx');

# ZCTA populations apportioned equally between their zipcodes
zippop <- sdohrsazip[,c('ZIPCODE','ZCTA','ACS_TOT_POP_WT_ZC')] %>% na.omit %>%
  unique %>% group_by(ZCTA) %>%
  mutate(ACS_TOT_POP_WT_ZC=max(ACS_TOT_POP_WT_ZC)/n()) %>%
  ungroup  %>% na.omit %>% unique %>% transmute(ZIPCODE,POP=ACS_TOT_POP_WT_ZC);

# Crosswalk from CN/RSA/ZCTA to RUCA by way of zipcode
cx1 <- sdohrsazip[,c('ZIPCODE','ZCTA')] %>% inner_join(cx0,.) %>% left_join(ruca);
# Eliminating rows that are not linkable to cntcodes or cnt2metro
cx1 <- subset(zip2cnt,COUNTY %in% c(cntcodes$COUNTY,cnt2metro$COUNTY) )$ZIP %>%
  union(subset(zip2cbsa,CBSA %in% cnt2metro$CBSA.Code)$ZIP) %>% {subset(cx1,ZIPCODE %in% .)}

# crosswalk from CN/ZIP/ZCTA , by way of county, to metro area
cx2 <- select(zip2cnt,ZIP,COUNTY,USPS_ZIP_PREF_STATE) %>%
  inner_join(cx1,.,by=c(ZIPCODE='ZIP'));
cxmetro2 <- transmute(cnt2metro,COUNTY,CSA=CSA.Title,CBSA=CBSA.Title
                      ,Metro=Metropolitan.Division.Title
                      ,CountyName=County.County.Equivalent
                      ,Centrality=Central.Outlying.County) %>%
  inner_join(cx2,.);

cx3 <- select(zip2cbsa,ZIP,CBSA,USPS_ZIP_PREF_STATE) %>%
  inner_join(cx1,.,by=c(ZIPCODE='ZIP')) %>%
  subset(!ZIPCODE %in% cxmetro2$ZIPCODE) %>% rename(CBSA.Code=CBSA);
cxmetro3 <- transmute(cnt2metro,CBSA.Code,CSA=CSA.Title,CBSA=CBSA.Title
                      ,Metro=Metropolitan.Division.Title
                      ,CountyName=County.County.Equivalent
                      ,Centrality=Central.Outlying.County) %>% unique %>%
  inner_join(cx3,.);

cx4 <- subset(cntcodes,Summary.Level=='050') %>%
  select(CountyNameGC=Area.Name.including.legal.statistical.area.description,COUNTY) %>%
  inner_join(cx2,.,by=c(COUNTY='COUNTY'));
cx5 <- bind_rows(cxmetro2,cxmetro3) %>%
  # get the metro-area information
  select(ZIPCODE,CSA,CBSA,Metro,CountyName,Centrality,COUNTY1=COUNTY) %>%
  # get the CN, ZCTA, RUCA/RuralUrban information
  left_join(cx1,.) %>%
  # get the county information
  left_join(cx4[,c('ZIPCODE','COUNTY','USPS_ZIP_PREF_STATE','CountyNameGC')]) %>%
  # get the population and split it between zipcodes that got duplicated during
  # the above pipeline so that the sum of POP for each zipcode is the same as
  # is would be in the original zippop table created at the start
  left_join(zippop) %>% group_by(ZIPCODE) %>% mutate(POP=max(POP)/n());

# Test to ensure that the populations held up
stopifnot(sum(cx5$POP)==sum(subset(zippop,ZIPCODE %in% cx5$ZIPCODE)$POP));

# This is the regexp that matches all of label0 (might be handy for QC/debugging)
label0rx <- "^[ñA-Za-z -;]+, [A-Z]{2}(?:[-;][A-Z]{2})*(?: \\[[A-Za-z;]+\\])?(?: \\[[A-Za-z;]+\\])?(?: \\d+)?$";

# This is for moving the states to the front.
states2frontrx <- "^(.*?), ([A-Z]{2}(?:[-;][A-Z]{2})*) (.*)$"

# reusable code to deduplicate the various columns, weight them by population
# (split up in as many equal pieces as there were duplictations of those zipcodes)
# find the plurality value or values, and collapse it/them into one string.
#
# I.e. this is the part where most of the sorcery happens.
fn_final_labels <- . %>% summarise(
  across(!where(is.numeric),~fn_plurality(.x,ignore = ''))) %>%
  mutate(
    #across(starts_with('CountyName'),~gsub(';','-',.x))
    across(starts_with('CountyName'),~ifelse(.x %in% c(NA,''),.x,paste0(.x,', ',STATE)))
    #,Centrality = gsub('Central;Outlying','Spanning',Centrality)
    ,across(all_of(c('CSA','CBSA','Metro','CountyName','CountyNameGC'))
            ,~ifelse(.x %in% c(NA,''),.x,paste0(.x,' [',RuralUrban,']')))
    , label0 = case_when(
      !(duplicated(Metro)|duplicated(Metro,fromLast=T)) & Metro != '' ~ Metro
      ,!(duplicated(CSA)|duplicated(CSA,fromLast=T)) & CSA != '' ~ CSA
      ,!(duplicated(CBSA)|duplicated(CBSA,fromLast=T)) & CBSA != '' ~ CBSA
      ,!(duplicated(CountyName)|duplicated(CountyName,fromLast=T)) & CountyName != '' ~ CountyName
      ,!(duplicated(CountyNameGC)|duplicated(CountyNameGC,fromLast=T)) & CountyNameGC != '' ~ CountyNameGC)
    ,across(all_of(c('CSA','CBSA','Metro','CountyName','CountyNameGC'))
            ,~ifelse(!is.na(label0) | Centrality=='' | .x == '',.x,paste0(.x,' [',Centrality,']')))
    ,label0 = coalesce(label0,case_when(
      !(duplicated(Metro)|duplicated(Metro,fromLast=T)) & Metro != '' ~ Metro
      ,!(duplicated(CSA)|duplicated(CSA,fromLast=T)) & CSA != '' ~ CSA
      ,!(duplicated(CBSA)|duplicated(CBSA,fromLast=T)) & CBSA != '' ~ CBSA
      ,!(duplicated(CountyName)|duplicated(CountyName,fromLast=T)) & CountyName != '' ~ CountyName
      ,!(duplicated(CountyNameGC)|duplicated(CountyNameGC,fromLast=T)) & CountyNameGC != '' ~ CountyNameGC
    ))
    ,uniquelabel = !is.na(label0)
    ,label0 = coalesce(label0,CountyNameGC)
  ) %>% group_by(label0) %>%
  mutate(seqid = seq_len(n())
         ,label0 = if(n()==1) label0 else paste0(label0,' ',seqid)
         # Rearrange string components by reordering capture groups from states2frontrx
         ,label1 = gsub(states2frontrx, "\\2, \\1 \\3", label0)
         );

# Here we apply the above code to generate plurality values for everything
# on the CN level. Then, we do the same at the ZCTA and ZIP levels but we don't
# use the ZCTA or ZIP versions of the names, we copy over the names from the CN
# level, repeating them for every zipcode or ZCTA that is in the same CN.
cnlabels <-list();
cnlabels$labels_cn <- group_by(cx5,CN) %>% fn_final_labels();
cnlabels$labels_zcta <- group_by(cx5,ZCTA) %>% fn_final_labels() %>%
  ungroup %>% dplyr::select(!matches('label|seqid')) %>%
  left_join(cnlabels$labels_cn[,c('CN','label0','label1')]);
cnlabels$labels_zip <- group_by(cx5,ZIPCODE) %>% fn_final_labels() %>%
  ungroup %>% dplyr::select(!matches('label|seqid')) %>%
  left_join(cnlabels$labels_cn[,c('CN','label0','label1')]);


# duplication/uniqueness report
# sapply(cnlabels,function(xx) {
#   browser();
#   xx_augmented <- with(xx,paste(RuralUrban,Centrality,STATE));
#   c(multis=sum(grepl(';',xx)),multis2plus=sum(grepl(';.*;',xx))
#     ,na=sum(is.na(xx)),missing=sum(xx=='',na.rm = T)
#     ,nonmissing=sum(na.omit(xx!='')),nonmissing_unique=length(setdiff(na.omit(xx),''))
#     ,nondups=sum(!(duplicated(xx)|duplicated(xx,fromLast=T)))
#     ,aug_nondups=sum(!(duplicated(xx_augmented)|duplicated(xx_augmented,fromLast=T))))})

# export rsa shared files ----
export(cnlabels,file='data/RSA_labels.xlsx');

rsa_xwalk_public <- ungroup(cnlabels$labels_zcta) %>%
  transmute(RSA=CN,ZCTA,RSA_Name=fn_replace_trailing_numbers(label1)
            ,prefix=str_extract(RSA,'^.')) %>% left_join(rsa2cmsregion) %>%
  select(-prefix) %>%
  arrange(RSA,ZCTA);

rsa_rsr_public <- ungroup(cnlabels$labels_cn) %>%
  transmute(RSA=CN,RSA_Name=fn_replace_trailing_numbers(label1)
            ,prefix=str_extract(RSA,'^.')) %>%
  left_join(rsa0) %>% select(-'RSA_bin') %>%
  rename(Community_Discharge=RSR) %>%
  left_join(rsa2cmsregion) %>% select(-prefix) %>%  arrange(RSA);

export(rsa_xwalk_public,'rsa_xwalk_public.csv');
export(rsa_rsr_public,'rsa_rsr_public.csv');


# old2new name mapping ----
# Old ZCTA column names to new ZIP column names
newnames <- names(sdohzip[[sdohzipselected]]) %>%
  setdiff(c('YEAR','ZCTA','STATE','REGION',if(exists('colmap')) colmap else c()));
newnames_std <- fn_automapper(newnames);
oldnames <- names(sdohzcta[[sdohzctaselected]]) %>%
  setdiff(c('YEAR','ZCTA','STATE','REGION',if(exists('colmap')) names(colmap) else c()));
oldnames_std <- fn_automapper(oldnames);
matched_names_std <- intersect(newnames_std,oldnames_std);
old2new <- data.frame(oldnames,oldnames_std
                      ,pass0=ifelse(oldnames_std %in% matched_names_std,oldnames_std,NA));

newnames_unmatched_std <- setdiff(newnames_std,matched_names_std);

fn_closestmatch <- function(fullinput,alreadymatched,matchto){
  matchfrom <- fullinput[is.na(alreadymatched)];
  matchto <- setdiff(matchto,alreadymatched);
  closestmatch <- stringdistmatrix(fullinput,matchto) %>%
    apply(1,fn_zapambiguous) %>% apply(1,fn_zapambiguous);
  closestmatch[,colSums(!is.na(closestmatch))>1] <- NA;
  closestmatch[rowSums(!is.na(closestmatch))>1,] <- NA;
  closestunique <- apply(closestmatch,1
                         ,function(xx){
                           if(all(is.na(xx))) NA else matchto[!is.na(xx)]
                         });
  alreadymatched[is.na(alreadymatched)] <- closestunique;
  alreadymatched;
}

closestmatch <- stringdistmatrix(old2new$oldnames_std,newnames_unmatched_std) %>%
  apply(1,fn_zapambiguous) %>% apply(1,fn_zapambiguous);
closestmatch[,colSums(!is.na(closestmatch))>1] <- NA;
closestmatch[rowSums(!is.na(closestmatch))>1,] <- NA;
# old2new$pass0 <- apply(closestmatch,1
#                        ,function(xx){
#                          if(all(is.na(xx))) NA else newnames_unmatched_std[!is.na(xx)]
#                          });
#
#   apply(1,function(xx) newnames_unmatched_std[which(xx==min(xx))]) %>%
#   setNames(old2new$oldnames_std);
closestsinglematch <- Filter(function(xx) length(xx)==1,closestmatch) %>% unlist;
old2new$newnames_std <- closestsinglematch[old2new$oldnames_std];
newnames_unmatched_std0 <- setdiff(newnames_unmatched_std,old2new$newnames_std);

closestmatch0 <- subset(old2new,is.na(newnames_std))$oldnames_std %>% stringdistmatrix(.,newnames_unmatched_std0) %>%
  apply(1,function(xx) newnames_unmatched_std0[which(xx==min(xx))]) %>%
  setNames(subset(old2new,is.na(newnames_std))$oldnames_std) %>%
  Filter(function(xx) length(xx)>1,.) %>% unlist;


c()