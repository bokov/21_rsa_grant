# required CRAN packages
install.packages(.crandeps<-c('rio','tidyverse','devtools','pander','rmarkdown'
                              ,'GGally','Boruta','nFactors','metamedian'
                              ,'RCurl')
                 ,repos = 'https://cloud.r-project.org');

# required author package
devtools::install_github('bokov/tidbits',ref='integration');

# Optional CRAN packages. Uncomment the below lines in order to install them
# install.packages(.cranoptional<-c('synthpop','GGally')
#                  ,repos='https://cloud.r-project.org');

# Testing install
.results <- sapply(c(.crandeps,'tidbits'),require,character.only=T);
if(!all(.results)){
  message('Problems found. Before you will be able to run these scripts you will need to figure out why the following packages failed to install:\n    '
          ,paste0(names(.results[!.results]),collapse=', '))
} else {
  message('All required packages successfully installed or updated.');
}
