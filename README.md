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
command-line in the project folder with `R -e "source('onetime_setup.R')"`.

## Basic Operation

At the moment the only script you are likely to need is `data.R`. In an
interactive session you can open it and run it line by line, or you can
`source('data.R')`. You can also compile it directly into a self-contained HTML
document with the following command: `rmarkdown::render('data.R')` and that is
how the results are intended to be viewed.

If you want to compile a Word version of the output, you could use: 
`rmarkdown::render('data.R',output_format='word_document')`

## Swapping out the Simulated Data

By default, all the data used is the simulated version. If you wish to use the
real data, create a script named `local.config.R` in this folder and put the
following code into it, replacing all the file paths that begin with `SIM_` with
paths to the real versions of those files wherever you keep them on your
computer...
```
inputdata <- c(dat0='data/SIM_SDOH_ZCTA.xlsx'      # census data by ZCTA
               ,cx0='data/SIM_ALLCMS.csv'          # RSA-ZCTA crosswalk
               ,rsa0='data/SIM_RSAv4 SCD RSRs.csv' # outcomes (RSR)
               ,dct0='data/data_dictionary.tsv'    # data dictionary for the
                                                   # dat1 dataset that _this_
                                                   # scriport produces
               );
```

This is all you need in order to generate the same results that I've been 
posting to our shared space.

If you are collaborating with us you are welcome to create forks.
We welcome your pull requests and suggestions. You should _never_ check the
following items into github:

* `local.config.R`. Doing so will ruin the portability of your version of the 
  project. As a result, anybody who lacks the needed files or keeps them in a different 
  location than you do will get error messages when they try to run it instead of
  example results from the simulated test-data.
* Any real data, unless explicitly directed to do so by TR. Unless TR says
  otherwise, we are not posting the real data publicly until after we publish on
  it. Furthermore, data is potentially large and will slow down the repo. Git is
  for code. The proper place to share datasets is something like
  https://zenodo.org
  

