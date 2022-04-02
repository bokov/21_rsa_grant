#' ---
#' title: "Confirmatory Factor Analysis of AHRQ SDOH Domains"
#' author: 'RSA Grant Team'
#' abstract: |
#'   | CFA
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
#'   pdf_document: default
#'   word_document: default
#' ---
#+ init, echo=FALSE, message=FALSE, warning=FALSE,results='hide'
# Init ----
debug <- 0;
knitr::opts_chunk$set(echo=debug>0, warning=debug>0, message=debug>0);
# Global project settings ----
inputdata <- c();
source('config.R',local=T,echo=debug>0);

# Load libraries ----
library(rio); library(dplyr); library(tidbits); # data handling
library(pander); library(broom);                # formatting
#library(GGally);
#library(mice);
library(psych);                                 # factor analysis
library(caret);                                 # cross-validation
library(Boruta);                                # variable selection
library(nFactors);                              # optimal number of factors
library(lavaan);
library(car);
library(semPlot);

# Make tables never split
panderOptions('table.split.table',Inf);
panderOptions('table.split.cells',Inf);
panderOptions('p.copula',', ');
panderOptions('missing','-');
options(tinytex.verbose=TRUE)
# Get stepAICc (like stepAIC but adjusting for small sample sizes)
source('project_functions.R');

# Local project settings ----
# tweak base plot settings to avoid captions going off-screen
.par_default <- par(no.readonly = TRUE);
# overwrite previously set global values if needed
if(file.exists('local.config.R')){
  source('local.config.R',local=TRUE,echo = debug>0);
  if(exists('.local.inputdata')){
    inputdata <- replace(inputdata,names(.local.inputdata),.local.inputdata)};
};



# Import data ----

# if merged files not already built, run the script that builds them
if(!all(file.exists(inputdata[c('dat2')]))){
  system('R --vanilla -q -s -f data.R',ignore.stdout = debug==0,ignore.stderr = debug==0, wait=TRUE,intern=TRUE)};
# dat1 <- import(inputdata['dat1']);
dat2 <- import(inputdata['dat2']);
# Obtain the numeric-only columns as dat3tr and dat3ts for training and test
# sets, respectively
dat3tr <- subset(dat2,subsample=='train') %>% select(where(is.numeric));
#dat3ts <- subset(dat2,subsample=='test') %>% select(where(is.numeric));
dct0 <- import(inputdata['dct0']);
dct0 <- subset(dct0,column %in% colnames(dat3tr));

colorizeVars <- cbind(dct0$column,colorByList(dct0$column,template="[%1$s]{.%2$stext}"));
cGroupRename <- cbind(c('c_AHRQsocial','c_AHRQecon','c_AHRQedu','c_AHRQphysinfr'
                        ,'c_AHRQhealth','c_AHRQgeo')
                      ,c('Social','Economic','Education'
                         ,'Physical Infrastructure','Health','Geography'));
#'
#' ----
formals(colorByList)$colorList %>% eval %>%
  sprintf('[%s]{.%stext}',.,names(.)) %>% submulti(cGroupRename) %>%
  cbind() %>%
  pander(col.names='AHRQ Domain Key',justify='left');
#' ----
ahrqvars <- v() %>% grep('c_AHRQ',.,val=T) %>%
  sapply(function(xx) sprintf('v(%s)',xx) %>% parse(text=.) %>% eval);
names(ahrqvars) <- submulti(names(ahrqvars),cGroupRename);
#'
#' There are `r length(unlist(ahrqvars))` numeric variables in the AHRQ data:
lapply(ahrqvars,function(xx) unname(colorByList(xx))) %>% pander #pander(ahrqvars);
#'
# confirmatory factor analysis ----
#' # CFA
#'
#+ specifymodel,cache=T
AHRQmodel <- lapply(ahrqvars,paste,collapse=' + ') %>%
  paste(names(.),'=~',.,collapse='\n') %>% gsub('Physical ','',.);
set.seed(project_seed);
AHRQfit <- lavaan::cfa(AHRQmodel, data=dat3tr,std.lv=TRUE);
#' # Model Results
#'
#' ## Summary
#+ modelsummary
#.junk <- capture.output(AHRQfitSummary<-summary(AHRQfit,standardized=T,fit.measures=T))
# pander(AHRQfitSummary));
#'
#' ## Parameter Estimates
#+ paramestimates
parameterEstimates(AHRQfit, standardized=TRUE) %>%
  filter(op == "=~") %>%
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue
         , Beta=std.all) %>% pander;

#'
#' ## Normality of Residuals
#'
#' Normally distributed residuals would be on a straight diagonal line.
AHRQres <- residuals(AHRQfit, type = "cor")$cov
AHRQres[upper.tri(AHRQres,diag=T)] <- NA
v1 <- as.vector(AHRQres); v2 <- v1[!is.na(v1)]; qqPlot(v2,id=F);
#'
#' ## Modification Indexes
#+ MIs, cache=T
if(file.exists('data/AHRQmi.rdata')){
  load('data/AHRQmi.rdata')
} else {
  AHRQmi <- modificationIndices(AHRQfit, sort.=TRUE, minimum.value=3);
  save(AHRQmi,file='data/AHRQmi.rdata');
}
pander(head(AHRQmi,20),row.names=F);
#'
#' # Path Plot
#+ SEMplot, cache=T, fig.asp=4, fig.width=16
semPlot::semPaths(AHRQfit,'std',rotation = 2,vTrans=10,borders=F
                  ,residuals = F,curvature = 3, nCharNodes=15
                  ,mar=c(3,30,3,3));
semCors(AHRQfit,titles=T,vertical=F);
#'
#' # Comparison to Orthogonal Model
#+ cfaortho, cache=T
AHRQfitortho <- lavaan::cfa(AHRQmodel, data=dat3tr,std.lv=TRUE,orthogonal=T);
anova(AHRQfit,AHRQfitortho) %>% pander;
#'
#' # Goodness of Fit
#+ fitmeasures
AHRQmeasures <- capture.output(fitmeasures(AHRQfit,output = 'text'));
gsub('([^[:space:]]) {2,}([-0-9])','\\1|\\2',AHRQmeasures) %>%
  strsplit('|',fixed=T) %>% lapply(function(xx) c(xx,'','')[1:2]) %>%
  do.call(rbind,.) %>% as.data.frame %>%
  subset(!(grepl('^ *$',V1)&grepl('^ *$',V2))) %>%
  pander(row.names=F,justify='lr',col.names=c('',''));
