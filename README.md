EconData
================
Created by Bradley Setzler, University of Chicago

This is a directory for managing publicly-available economics data. It
contains two
    components:

  - [EconData](https://github.com/setzler/EconData/tree/master/EconData):
    R package to automatically download and prepare various
    publicly-available economic data sets from source.
  - [DataRepo](https://github.com/setzler/EconData/tree/master/DataRepo)
    A repository of prepared data sets that were created by the EconData
    package.

To use the R package, run the following:

``` r
devtools::install_github("setzler/EconData/EconData")
```

``` r
library(EconData)
```

Data sets currently available include:

  - [State corporate tax
    rates, 2000-2020.](https://github.com/setzler/EconData/tree/master/DataRepo/StateCorpTax/)
  - [Census CBP employment, earnings, and establishments at various
    levels of
    aggregation, 2001-2017.](https://github.com/setzler/EconData/tree/master/DataRepo/CensusCBP/)
