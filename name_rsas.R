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


# Local project settings ----
# overwrite previously set global values if needed
if(file.exists('local.config.R')){
  source('local.config.R',local=TRUE,echo = debug>0);
  message('Found local.config.R, loading local settings');
  if(exists('.local.inputdata')){
    message('Found .local.inputdata object, merging into inputdata');
    inputdata <- replace(inputdata,names(.local.inputdata),.local.inputdata)};
};

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
  gsub('^ACS_PCT_|_ZC$','',.);


# Import data ----

#' Downloading and importing AHRQ ZCTA data (beta, deprecated)
if(!all(file.exists(c('data/SDOH_ZCTA.rdata','data/SDOH_ZIP.rdata')))) system('R download_data.R',wait=T);
#sdohzcta <- import('data/SDOH_ZCTA.rdata');
sdohzip <- import('data/SDOH_ZIP.rdata');
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
                                   ,1:3 ~ 'Metropolitan'
                                   ,4:6 ~ 'Micropolitan'
                                   ,7:9 ~ 'Smalltown'
                                   ,10 ~ 'Rural'
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

# This is the regexp that matches all of label0:
label0rx <- "^[ñA-Za-z -;]+, [A-Z]{2}(?:[-;][A-Z]{2})*(?: \\[[A-Za-z;]+\\])?(?: \\[[A-Za-z;]+\\])?(?: \\d+)?$";
# This would clean up semicolon-delimited states: gsub("(?<=\\b[A-Z]{2});(?=[A-Z]{2}\\b)","-",perl=T,cnlabels$label0)

# This is for moving the states to the front.
states2frontrx <- "^(.*?), ([A-Z]{2}(?:[-;][A-Z]{2})*) (.*)$"
# Rearrange string components by reordering capture groups


cnlabels <- group_by(cx5,CN) %>% summarise(
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
         ,label1 = gsub(states2frontrx, "\\2, \\1 \\3", label0)
         );

# Might want to replace Central;Outlying with 'Spanning'


# duplication/uniqueness report
sapply(cnlabels,function(xx) {
  xx_augmented <- paste(xx,cnlabels$RuralUrban,cnlabels$Centrality,cnlabels$STATE);
  c(multis=sum(grepl(';',xx)),multis2plus=sum(grepl(';.*;',xx))
    ,na=sum(is.na(xx)),missing=sum(xx=='',na.rm = T)
    ,nonmissing=sum(na.omit(xx!='')),nonmissing_unique=length(setdiff(na.omit(xx),''))
    ,nondups=sum(!(duplicated(xx)|duplicated(xx,fromLast=T)))
    ,aug_nondups=sum(!(duplicated(xx_augmented)|duplicated(xx_augmented,fromLast=T))))})

export(cnlabels,file='data/RSA_labels.csv');
c()