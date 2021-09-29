# 21_rsa_grant

## Purpose

These are the analytics portion of our proposed study. This is a self-contained
set of scripts that does analysis and prepares reports on that analysis. All of
this is a work in progress. The data files are not included and need to be
obtained from our PI but simulated data-files are bundled with the project and
have everything you need in order to test out these scripts.

## Installation

As far as I know this project will work on Mac, Windows, and Linux.

The first time you clone or download this project from GitHub onto a new
computer you will need to install some required R packages in the script
`onetime_setup.R`. You can do this automatically from an R console started in
the project folder with `source('onetime_setup.R')`. Or you can do it from the
command-line in the project folder with `R -f onetime_setup.R`.

## Basic Operation

At the moment the only script you are likely to need is `variableselection.R`.
In an interactive session you can open it and run it line by line, or you can
`source('variableselection.R')`. You can also compile it directly into a
self-contained HTML document with the following command:
`rmarkdown::render('variableselection.R')` and that is how the results are
intended to be viewed.

If you want to compile a Word version of the output, you could use: 
`rmarkdown::render('variableselection.R',output_format='word_document')`

## Swapping out the Simulated Data

By default, all the data used is the simulated version. 

If you downloaded the `SDOH_RSR_2013_prelim.csv` and `SDOH_RSR_2013_prelim.csv`
from our shared folder just put them in the top level of this project folder and
start working with `variableselection.R` (or a new script of your own). Please
remember to not add the data files to your repository.

If you have the raw data and wish to use build `SDOH_RSR_2013_prelim.csv` and
`SDOH_RSR_2013_prelim.csv` from scratch, create a script named `local.config.R`
in the top level of this project folder and put the following code into it,
replacing all the file paths that begin with `SIM_` with paths to the real
versions of those files wherever you keep them on your computer...
```
inputdata <- c(dat0='data/SIM_SDOH_ZCTA.xlsx'          # census data by ZCTA
               ,cx0='data/SIM_ALLCMS.csv'              # RSA-ZCTA crosswalk
               ,rsa0='data/SIM_RSAv4 SCD RSRs.csv'     # outcomes (RSR)
               ,dct0='data/data_dictionary.tsv'        # data dictionary for the
                                                       # dat1 dataset that _this_
                                                       # scriport produces
               ,dat1='SDOH_RSR_2013_prelim.csv'        # the dat1 dataset
               ,dat2='SDOH_RSR_2013_scaled_prelim.csv' # the scaled version of
                                                       # dat1
               );
```

This is all you need in order to generate the same results that I've been
posting to our shared space. There is no need to change paths for any of the
other file paths-- all of them get generated as needed if they are missing.

If you are collaborating with us you are welcome to create forks.
We welcome your pull requests and suggestions. You should _never_ check the
following items into github:

* `local.config.R`. Doing so will ruin the portability of your version of the 
  project. As a result, anybody who lacks the needed files or keeps them in a
  different location than you do will get error messages when they try to run it
  instead of example results from the simulated test-data.

* Any real data, unless explicitly directed to do so by TR. Unless TR says
  otherwise, we are not posting the real data publicly. Furthermore, data is
  potentially large and will slow down the repo. Git is for code. So even if and
  when we do release the data proper place for datasets is https://zenodo.org
  or something similar
  

