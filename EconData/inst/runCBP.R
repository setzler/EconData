
library(EconData)
library(futile.logger)

output_path <- "~/github/EconData/DataRepo/CensusCBP/"
misc_path <- "~/github/EconData/DataRepo/Miscellaneous/"

# main
for (ind in c(0, 2, 3, 4, 6)) {
  for (loc in c("national", "county")) {
    if (!(loc == "county" & ind > 2) & !(loc == "state" & ind == 6)) {
      flog.info("location %s, industry %s", loc, ind)
      CBP <- getCBP(years = 2001:2017, location = loc, industry = ind)
      if (ind > 0) {
        write.csv(CBP, file = sprintf("%sCBP_%s_industry%s.csv", output_path, loc, ind), row.names = F)
      } else {
        write.csv(CBP, file = sprintf("%sCBP_%s_total.csv", output_path, loc), row.names = F)
      }
    }
  }
}



# CZ collapses

state_names <- setDT(read.csv(file=sprintf("%sstate_fips_crosswalk.csv",misc_path)))
cz <- setDT(read.csv(file=sprintf("%scz_crosswalk_2000.csv",misc_path)))
loc = 'county'
for (ind in c(0, 2, 3, 4, 6)) {
  if (!(loc == "county" & ind > 2) & !(loc == "state" & ind == 6)) {
    flog.info("location %s, industry %s", loc, ind)
    if (ind > 0) {
      CBP <- setDT(read.csv(file = sprintf("%sCBP_%s_industry%s.csv", output_path, loc, ind)))
      CBP <- merge(CBP,cz,by=c('state_fips','county_fips'))
      CBP[, state_fips := NULL][, county_fips := NULL]
      CBP <- CBP[,lapply(.SD,sum,na.rm=T),list(year,cz,naics)][order(cz,naics,year)]
      write.csv(CBP, file = sprintf("%sCBP_%s_industry%s.csv", output_path, "cz", ind), row.names = F)
      
      CBP <- setDT(read.csv(file = sprintf("%sCBP_%s_industry%s.csv", output_path, loc, ind)))
      CBP[, county_fips := NULL]
      CBP <- CBP[,lapply(.SD,sum,na.rm=T),list(year,state_fips,naics)][order(state_fips,naics,year)]
      CBP <- merge(state_names,CBP,by=c('state_fips'))
      write.csv(CBP, file = sprintf("%sCBP_%s_industry%s.csv", output_path, "state", ind), row.names = F)
      
      
    } else {
      CBP <- setDT(read.csv(file = sprintf("%sCBP_%s_total.csv", output_path, loc)))
      CBP <- merge(CBP,cz,by=c('state_fips','county_fips'))
      CBP[, state_fips := NULL][, county_fips := NULL]
      CBP <- CBP[,lapply(.SD,sum,na.rm=T),list(year,cz)][order(cz,year)]
      write.csv(CBP, file = sprintf("%sCBP_%s_total.csv", output_path, "cz"), row.names = F)
      
      CBP <- setDT(read.csv(file = sprintf("%sCBP_%s_total.csv", output_path, loc)))
      CBP[, county_fips := NULL]
      CBP <- CBP[,lapply(.SD,sum,na.rm=T),list(year,state_fips)][order(state_fips,year)]
      CBP <- merge(state_names,CBP,by=c('state_fips'))
      write.csv(CBP, file = sprintf("%sCBP_%s_total.csv", output_path, "state"), row.names = F)
      
    }
  }
}



