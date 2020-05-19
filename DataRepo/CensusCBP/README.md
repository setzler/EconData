CensusCBP: prepared CBP data sets
================
Created by Bradley Setzler, University of Chicago

[Prepared Census CBP data sets are located
here.](https://github.com/setzler/EconData/tree/master/DataRepo/CensusCBP/)

### Download and clean the Census CBP

Census County Business Patterns (CBP) data is managed by the `getCBP()`
function. The arguments are:

  - `years`: years of CBP data to download (integer vector). Supported
    year range is 2001 to 2017.
  - `location`: Can be `"county"`, `"state"`, or `"national"`
    (character).
  - `industry`: Number of digits used in the NAICS code. `0` indicates
    use all industries.
  - `LFO`: Choose a legal form of organization (LFO). Options include
    `'C'` for C-corporations, `'S'` for S-corporations, `'P'` for
    partnerships. Default is `'-'`, which means to use all LFO types.

The resulting data set includes the variables `year, employment_march,
payroll_quarter1, establishments` as well as fips codes and industry
codes corresponding to the `aggregation` choice.

### Demonstration

Here, we download and clean the CBP files during 2001 - 2017. They are
saved to the `DataRepo/CensusCBP` directory as `CBP_state_total.csv`.

``` r
library(EconData)
CBP <- getCBP(years = 2001:2017, location = "state", industry = 0) 
write.csv(CBP,file="~/github/EconData/DataRepo/CensusCBP/CBP_state_total.csv", row.names=F)
kable(CBP[1:5])
```

Let’s plot the resulting data for the 5 most populous
states:

``` r
CBP <- setDT(read.csv(file="~/github/EconData/DataRepo/CensusCBP/CBP_state_total.csv"))
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
ggsave(gg,file='CBP_state_employment.png',width=8,height=5)
```

![](CBP_state_employment.png)
