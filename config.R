project_seed <- 2021091045;

sdohyear <- '2020';

useragent <- 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:128.0) Gecko/20100101 Firefox/128.0';

inputdata <- c(dat0= 'data/SIM_SDOH_ZCTA.xlsx'         # census data by ZCTA
               ,cx0= 'data/SIM_ALLCMS.csv'             # RSA-ZCTA crosswalk
               ,rsa0='data/SIM_RSAv4 SCD RSRs.csv'     # outcomes (RSR)
               ,dct0='data/data_dictionary.csv'        # data dictionary for the
                                                       # dat1 dataset that _this_
                                                       # scriport produces
               ,dat1='SDOH_RSR_201X_prelim.csv'        # the dat1 dataset
               ,dat2='SDOH_RSR_201X_scaled_prelim.csv' # the scaled version of
                                                       # dat1
               ,colmap='data/sdohmapping_ZCTA2ZIP.csv' # columns from beta zcta
                                                       # version and their
                                                       # production zip version
                                                       # equivalents
);


c()