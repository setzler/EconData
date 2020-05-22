
library(EconData)
library(data.table)
library(ggplot2)
library(scales)

## prepare corporate tax data sets
getCorpTaxSources(source_path = "~/github/EconData/DataRepo/StateCorpTax/sources/")

## add state fips codes
rates <- setDT(read.csv(sprintf("%sStateCorpTax_without_fips.csv","~/github/EconData/DataRepo/StateCorpTax/sources/")))
misc_path = "~/github/EconData/DataRepo/Miscellaneous/"
state_fips_file <- setDT(read.csv(file=sprintf("%sstate_fips_crosswalk.csv",misc_path)))
rates <- merge(state_fips_file,rates,by='state_name')
rates <- rates[,.(state_fips,state_name,year,cit)]
write.csv(rates, file=sprintf("%sStateCorpTax.csv","~/github/EconData/DataRepo/StateCorpTax/"), row.names=F)
