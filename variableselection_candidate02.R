#' ---
#' title: "Variable selection by SDOH domain for analysis of successful community discharge from rehab by RSAs"
#' author: 'RSA Grant Team'
#' abstract: |
#'   | Some preliminary unsupervised variable selection
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
# inputdata <- c(dat0='data/SIM_SDOH_ZCTA.xlsx'          # census data by ZCTA
#                ,cx0='data/SIM_ALLCMS.csv'              # RSA-ZCTA crosswalk
#                ,rsa0='data/SIM_RSAv4 SCD RSRs.csv'     # outcomes (RSR)
#                ,dct0='data/data_dictionary.csv'        # data dictionary
#                ,dat1='SDOH_RSR_2013_prelim.csv'        # the dat1 dataset
#                ,dat2='SDOH_RSR_2013_scaled_prelim.csv' # the scaled version of
# );

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
panderOptions('p.copula',', ');
options(tinytex.verbose=TRUE)
# Get stepAICc (like stepAIC but adjusting for small sample sizes)
source('project_functions.R');

# Local project settings ----
# tweak base plot settings to avoid captions going off-screen
.par_default <- par(no.readonly = TRUE);
.par_borutaplot <- list(mar=c(0.5, 6, 1, 0.5), mgp=c(0, 0.2, 0), cex=0.9,
                        tcl=0.2);
# overwrite previously set global values if needed
if(file.exists('local.config.R')){
  source('local.config.R',local=TRUE,echo = debug>0);
  if(exists('.local.inputdata')){
    inputdata <- replace(inputdata,names(.local.inputdata),.local.inputdata)};
};



# Import data ----

# if merged files not already built, run the script that builds them
if(!all(file.exists(inputdata[c('dat1','dat2')]))){
  system('R --vanilla -q -s -f data.R',ignore.stdout = debug==0,ignore.stderr = debug==0, wait=TRUE,intern=TRUE)};
dat1 <- import(inputdata['dat1']);
dat2 <- import(inputdata['dat2']);
dct0 <- import(inputdata['dct0']);
dct0 <- subset(dct0,column %in% colnames(dat1));
# Obtain the numeric-only columns as dat3tr and dat3ts for training and test
# sets, respectively
dat3tr <- subset(dat2,subsample=='train') %>% select(where(is.numeric));
dat3ts <- subset(dat2,subsample=='test') %>% select(where(is.numeric));

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
#' # How many variables to consider in factor analysis?
#'
#' There are `r length(unlist(ahrqvars))` numeric variables in the AHRQ data:
lapply(ahrqvars,function(xx) unname(colorByList(xx))) %>% pander #pander(ahrqvars);
#'
#' From the [main variable selection report](https://www.dropbox.com/home/Reistetter%20RSA%20SDOH%20Grant%202021?preview=variableselection.html)
#' we see that factors derived from this full set of variables do not fit
#' cleanly into the five SDOH domains.
#'
#' Of these, we selected the following based on domain knowledge of
#' post-rehabilitation patient outcomes:

v(c_domainexpert) %>% colorByList() %>% unname() %>% pander();

# How can we weed out some of the redundant or non-informative ones to see if
# the remaining variables have better separation? A possible approach is
# splitting performing stepwise selection (bi-directional, using the Bayes
# Information Criterion to determine when convergence has been achieved).
#+ ahrqvarsBIC, results='hide', cache=debug<=0
# ahrqvarsBIClms <- sapply(ahrqvars,intersect,names(dat3tr)) %>%
#   lapply(paste,collapse='+') %>% lapply(function(xx) paste('RSR~',xx))%>%
#   lapply(function(xx) update(lm(xx,data=dat3tr),.~.)) %>%
#   lapply(function(xx) stepAICc(xx
#                                ,scope=list(lower=RSR~1
#                                            ,upper=update(xx,.~(.)^2))
#                                ,direction='both',k=log(nrow(dat3tr))));
# ahrqvarsBIC <- lapply(ahrqvarsBIClms,terms) %>% lapply(attr,'term.labels') %>%
#   lapply(intersect,names(dat3tr));
# ahrqcolsBIC <- unlist(ahrqvarsBIC) %>% unname();
#
# Now there are only `r length(ahrqcolsBIC)` variables:
# lapply(ahrqvarsBIC,function(xx) unname(colorByList(xx))) %>% pander; #pander(ahrqvarsBIC);
#' We then can perform factor analysis on just this set of variables.
#'
# factor analysis ----
#' # How many factors to use?
#'
#+ scree, cache=debug<=0
set.seed(project_seed);
.junk<-capture.output(fapar3 <- fa.parallel(dat3tr
                                            ,fm='ml',fa='fa'
                                            ,nfactors = 25
                                            ,show.legend=F));
#' Looks like it's `r fapar3$nfact`
#'
#' # Factor analysis
#'
#'
#+ fa, cache=debug<=0,messages=FALSE
fa3 <-fa(dat3tr,nfactors = fapar3$nfact
         ,rotate='varimax',fm='ml') %>% fa.sort();

faload3 <- with(fa3,apply(loadings[],2
                         ,function(xx) rownames(loadings)[which.max(abs(xx))]));
# faload3 <- fa3$loadings[];
# faload3[faload3<0.15] <- NA;
# faload3 <- apply(faload3,2,function(xx) names(na.omit(xx)),simplify = F) %>%
#   Filter(function(xx) length(xx)>0,.);

#+ faplot, fig.width=10
pvdat3 <- with(fa3,Vaccounted['Proportion Explained'
                              ,intersect(colnames(Vaccounted), names(faload3))]);
names(pvdat3) <- scales::percent(pvdat3,accuracy=0.1) %>%
  paste0(names(.),', ',.);
par(mar=c(7,4.1,4.1,2.1));
barplot(pvdat3,ylab='Proportion of Variance Explained',las=2);
par(.par_default);
# faload3a <- lapply(faload3,function(xx) base::ifelse(xx %in% v(c_domainexpert)
#                                                      ,gsub('text\\{([^}]*)\\}','text{[\\1]}',colorByList(xx))
#                                                      ,colorByList(xx)));
# faload3b <- sapply(faload3,function(xx) lapply(ahrqvars,function(yy) {
#   oo<-intersect(xx,yy);
#   ifelse(oo %in% v(c_domainexpert)
#          ,gsub('\\[(.*)\\]','[**\\1**]',colorByList(oo))
#          #, gsub('text\\{([^}]*)\\}','text{[\\1]}',colorByList(oo))
#          ,colorByList(oo))}) %>%
#     Filter(function(zz) length(zz)>0,.) ,simplify = F);
#'
#'
#' These `r length(pvdat3)` factors account for
#' `r scales::percent(sum(pvdat3),accuracy=0.1)` of the variation. If for each
#' factor we only retain the variable with the highest loading, we are left with
#' the following set:
#+ factorlist
#pander(varsdat3a);
colorByList(faload3) %>% cbind %>% pander();
#'
# fit models ----
lmexp0 <- paste(v(c_domainexpert),collapse=' + ') %>% paste('RSR ~ ',.) %>%
  lm(data=dat3tr) %>% update(.~.);
frm_exp0disp <- paste(colorByList(v(c_domainexpert)),collapse=' + ') %>%
  paste('RSR ~',.);
lmexp1 <- paste(faload3,collapse=' + ') %>% paste('RSR ~ ',.) %>%
  lm(data=dat3tr) %>% update(.~.);
frm_exp1disp <- paste(colorByList(faload3),collapse=' + ') %>%
  paste('RSR ~',.);
#' # Using all the manually-selected variables
#'
#+ lmexp0, comment=''
train(lmexp0$call$formula,method='lm',data=dat3tr
      ,trControl=trainControl(method='repeatedcv',number=5,repeats=10));
plot(dat3tr$RSR~predict(lmexp0),xlab='Predicted',ylab='Observed')
#+ lmexp0model
pander(lmexp0,caption=frm_exp0disp);
#' # Using the reduced set of manually-selected variables
#'
#+ lmexp1, comment=''
train(lmexp1$call$formula,method='lm',data=dat3tr
      ,trControl=trainControl(method='repeatedcv',number=5,repeats=10));
plot(dat3tr$RSR~predict(lmexp1),xlab='Predicted',ylab='Observed')
#+ lmexp1model
pander(lmexp1,caption=frm_exp1disp);
