# Survey of Consumer Finances

## Summary

This is a repository that contains analyses around the [Survey of Consumer Finances](https://www.federalreserve.gov/econres/scfindex.htm) (SCF) put out by the Federal Reserve every 3 years. This takes a cross-sectional survey of the finances of U.S. families, in terms of their balance sheets, pensions, income, demographics, etc. 

Analysis of the data associated to the survey is a bit complicated, for several reasons: 

* The survey design of the SCF itself is complicated. It involves the creation of 5 "replicates" for each family that participates in the survey. The data associated to each replicate is slightly different and imputed using a method described in [this paper](https://www.federalreserve.gov/econresdata/scf/files/impute98.pdf). This provides both privacy and more robust statistics due to the imputation of missing data in a variety of ways. Associated to this design is a collection of replicate weights that dictate how to analyze the data obtained from the replicates in an appropriate way.
* It is a long survey, with many questions that have conditional responses based on previous questions;
* The variables are coded in a variety of ways that require an examination of the [codebook](https://www.federalreserve.gov/econres/files/codebk2016.txt) to interpret;
* The public data set has a lot of censoring (e.g. of racial demographics) to protect the privacy of participants. 


## Methodology

* The data is loaded using the [`lodown`](https://github.com/ajdamico/lodown) package as recommended on the SCF website, which automatically obtains and parses the SCF survey data into 5 chunks of data, one for each replicate.

    It also obtains the replicate weight data and parses it into a format suitable for usage with the `survey` package.

* The `survey` package is used to analyze the data. This package understands how to perform calculations on surveys when replicate weights are given, e.g. summary statistics, fitting linear models, etc.


## Usage

Each analysis is contained in a subfolder of this directory. They are generally wrapped up in R project files and have dependencies managed via [packrat](https://rstudio.github.io/packrat/), which should make them completely reproducible. Go to a subfolder and view the `README` for more specific information.

