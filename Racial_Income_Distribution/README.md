# Relative Racial Income 

This is a short analysis of the relative income percentiles of different racial groups, as derived from data in the SCF. This reproduces variants of a plot found at the [PPP](http://peoplespolicyproject.org/2017/09/29/the-best-way-to-represent-the-overall-racial-wealth-gap/). 

## Methodology

* Obtain the 2016 SCF data;
* Subset the data to Black, White and Latino participants;
* Calculate the overall income percentiles for the entire population using the `survey` package;
* Calculate the internal income percentiles by race;
* Plot the internal income percentiles against the overall income percentiles.

### A Note on Coding

The [codebook](https://www.federalreserve.gov/econres/files/bulletin.macro.txt): from The Fed around this data codes reported primary/secondary race as two variables: `X6809`, `X6810`:

```
                     1.    *WHITE (INCLUDE MIDDLE EASTERN/ARAB WITH WHITE);
                            Caucasian
                     2.    *BLACK/AFRICAN-AMERICAN
                     3.    *HISPANIC/LATINO
                     4.    *ASIAN
                     5.    *AMERICAN INDIAN/ALASKA NATIVE
                     6.    *NATIVE HAWAIIAN/PACIFIC ISLANDER
                    -7.    *OTHER
                     0.     Inap. (/no further responses)
                *********************************************************
                    FOR THE PUBLIC DATA SET:
                    ONLY X6809 AND X6810 ARE INCLUDED.
                    FOR X6809, CODES 4, 5, AND 6 ARE
                    COMBINED WITH CODE -7.
                    IF AN ADDITIONAL RESPONSE WAS GIVEN IN X6810-X6814,
                    X6810 IS SET TO 1; OTHERWISE X6810 IS SET TO 5.
                *********************************************************
```

In the summary file, the variable `race` is determined by the following logic: 

```
*     1=white non-Hispanic, 2=black/African-American, 3=Hispanic,
      4=Asian (only available in internal data set, see codebook), 
      5=other;
      IF X6809=1 THEN RACE=1;
      ELSE IF X6809=2 THEN RACE=2;
      ELSE IF X6809=3 THEN RACE=3;
      ELSE IF X6809=4 THEN RACE=4;
      ELSE RACE=5;


      IF X6809=1 & X7004^=1 THEN H_RACE=1;
      ELSE IF X6809=2 & X7004^=1 THEN H_RACE=2;
      ELSE IF X6809=3 | X7004=1 THEN H_RACE=3;
      ELSE IF X6809=4 THEN H_RACE=4;
      ELSE H_RACE=5;
```

which means that `race == 5` corresponds to anyone who does not identify as White, Black, or Hispanic/Latino.

## Output

The output is several plots that show the relative income percentiles for different races. These are found in the `plots` folder, with both transparent and non-transparent versions. In addition, the underlying percentile data is stored in CSV format in the `data` folder. 

## Usage

This project is built with the `Makefile`. Open the `RProj` file in RStudio in order to install all of the `packrat` dependencies. The project can then be built using the Build pane. 


