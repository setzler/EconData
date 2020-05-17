EconData
================
Created by Bradley Setzler, University of Chicago

This directory contains two components:

  - [EconData](): R package to automatically download and prepare
    various publicly-available economic data sets.
  - [DataRepo]() A repository of prepared data sets that were created by
    the EconData package.

<!-- end list -->

``` r
devtools::install_github("setzler/EconData/EconData")
```

``` r
library(EconData)
```

The included data sets are listed below.

# 1\. Census CBP

### Download and clean the Census CBP

CBP data is managed by the `getCBP()` function. The arguments are:

  - `years`: years of CBP data to download (integer vector). Supported
    year range is 2002 to 2017.
  - `aggregation`: Geography and industry level of variation.
    `"state_total"` means state total, `"state_industry"` means
    state-by-industry total. Other options are `"county_total"`,
    `"county_industry"`, `"national_total"`, and `"national_industry"`.
  - `LFO`: Choose a legal form of organization (LFO). Options include
    `'C'` for C-corporations, `'S'` for S-corporations, `'P'` for
    partnerships. Default is `'-'`, which means to use all LFO types.

The resulting data set includes the variables `year, employment_march,
payroll_quarter1, establishments` as well as fips codes and industry
codes corresponding to the `aggregation` choice.

### Example

Here, we download and clean the CBP files during 2001 - 2017. They are
saved to the `DataRepo/` directory as
    `CBP_state_total.csv`.

``` r
CBP <- getCBP(years = 2001:2017, aggregation = "state_total") 
```

    ## INFO [2020-05-17 15:49:30] downloading CBP for year 2001 aggregated by state_total.
    ## INFO [2020-05-17 15:49:31] downloading CBP for year 2002 aggregated by state_total.
    ## INFO [2020-05-17 15:49:32] downloading CBP for year 2003 aggregated by state_total.
    ## INFO [2020-05-17 15:49:33] downloading CBP for year 2004 aggregated by state_total.
    ## INFO [2020-05-17 15:49:34] downloading CBP for year 2005 aggregated by state_total.
    ## INFO [2020-05-17 15:49:35] downloading CBP for year 2006 aggregated by state_total.
    ## INFO [2020-05-17 15:49:36] downloading CBP for year 2007 aggregated by state_total.
    ## INFO [2020-05-17 15:49:37] downloading CBP for year 2008 aggregated by state_total.
    ## INFO [2020-05-17 15:49:38] downloading CBP for year 2009 aggregated by state_total.
    ## INFO [2020-05-17 15:49:39] downloading CBP for year 2010 aggregated by state_total.
    ## INFO [2020-05-17 15:49:42] downloading CBP for year 2011 aggregated by state_total.
    ## INFO [2020-05-17 15:49:44] downloading CBP for year 2012 aggregated by state_total.
    ## INFO [2020-05-17 15:49:46] downloading CBP for year 2013 aggregated by state_total.
    ## INFO [2020-05-17 15:49:48] downloading CBP for year 2014 aggregated by state_total.
    ## INFO [2020-05-17 15:49:50] downloading CBP for year 2015 aggregated by state_total.
    ## INFO [2020-05-17 15:49:53] downloading CBP for year 2016 aggregated by state_total.
    ## INFO [2020-05-17 15:49:55] downloading CBP for year 2017 aggregated by state_total.

``` r
write.csv(CBP,file="~/github/EconData/DataRepo/CBP_state_total.csv", row.names=F)
print(CBP[])
```

    ##      state_fips employment_march establishments payroll_quarter1 year
    ##   1:          1          1620952          99261      10962678000 2001
    ##   2:          2           214297          18589       1903256000 2001
    ##   3:          4          1941599         116304      14905239000 2001
    ##   4:          5           995521          62725       6411694000 2001
    ##   5:          6         13239616         806733     133485023000 2001
    ##  ---                                                                 
    ## 863:         51          3310542         201893      44736222000 2017
    ## 864:         53          2768660         191045      41344018000 2017
    ## 865:         54           549413          36522       5552016000 2017
    ## 866:         55          2561381         142136      29796266000 2017
    ## 867:         56           201864          21148       2253557000 2017

Let’s plot the resulting data for the 5 most populous states:

``` r
library(data.table)
library(ggplot2)
library(scales)

CBP <- setDT(read.csv("~/github/EconData/DataRepo/CBP_state_total.csv"))
CBP[, state := '']
CBP[state_fips==6, state := 'California']
CBP[state_fips==12, state := 'Florida']
CBP[state_fips==17, state := 'Illinois']
CBP[state_fips==36, state := 'New York']
CBP[state_fips==48, state := 'Texas']

gg <- ggplot(aes(x=year,y=employment_march/1e6,color=state,linetype=state),data=CBP[state != '']) + 
  geom_line() +
  theme_bw(base_size=14) + 
  labs(x="Year", y="March Employment (millions)",title="Census CBP",color="State",linetype="State") +
  scale_x_continuous(breaks= pretty_breaks()) +
  scale_y_continuous(breaks= pretty_breaks())
ggsave(gg,file='EconData/inst/CBP_state_employment.png',width=8,height=4)
```

![](EconData/inst/CBP_state_employment.png)
