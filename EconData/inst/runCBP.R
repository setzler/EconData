# required packages
library(EconData)
library(futile.logger)
library(data.table)

# global parameters
output_path <- "~/github/EconData/DataRepo/CensusCBP"
input_path <- "~/github/EconData/DataRepo/CensusCBP/raw"
misc_path <- "~/github/EconData/DataRepo/Miscellaneous"
the_year_set <- 2001:2021


# download the files
runDownloads <- function(){
  downloadCBP(years = the_year_set, location = "national", input_path = input_path)
  downloadCBP(years = the_year_set, location = "county", input_path = input_path)
}
runDownloads()

# clean the downloads
cleanDownloads <- function(){
  for (ind in c(0, 2, 3, 4, 6)) {
    for (loc in c("national", "county")) {
      if(loc=='national' | ind <= 4){
        getCBP(years = the_year_set, location = loc, industry = ind, LFO = "-", input_path = input_path, output_path = output_path)
      }
    }
  }
}
cleanDownloads()


# perform the CZ and state collapses
state_names <- setDT(read.csv(file=sprintf("%s/state_fips_crosswalk.csv",misc_path)))
cz <- setDT(read.csv(file=sprintf("%s/cz_crosswalk_2000.csv",misc_path)))
loc = 'county'
for (ind in c(0, 2, 3, 4, 6)) {
  if (!(loc == "county" & ind > 3) & !(loc == "state" & ind > 4)) {
    flog.info("location %s, industry %s", loc, ind)
    if (ind > 0) {
      CBP <- setDT(read.csv(file = sprintf("%s/CBP_%s_industry%s.csv", output_path, loc, ind)))
      CBP <- merge(CBP,cz,by=c('state_fips','county_fips'))
      CBP[, state_fips := NULL][, county_fips := NULL]
      CBP <- CBP[,lapply(.SD,sum,na.rm=T),list(year,cz,naics)][order(cz,naics,year)]
      write.csv(CBP, file = sprintf("%s/CBP_%s_industry%s.csv", output_path, "cz", ind), row.names = FALSE)
      
      CBP <- setDT(read.csv(file = sprintf("%s/CBP_%s_industry%s.csv", output_path, loc, ind)))
      CBP[, county_fips := NULL]
      CBP <- CBP[,lapply(.SD,sum,na.rm=T),list(year,state_fips,naics)][order(state_fips,naics,year)]
      CBP <- merge(state_names,CBP,by=c('state_fips'))
      write.csv(CBP, file = sprintf("%s/CBP_%s_industry%s.csv", output_path, "state", ind), row.names = FALSE)
      
      if(ind==2){ # sector names
        sector_names <- setDT(read.csv(file=sprintf("%s/sector_names.csv",misc_path)))
        for(loc2 in c("national", "state", "cz")){
          CBP <- setDT(read.csv(file = sprintf("%s/CBP_%s_industry%s.csv", output_path, loc2, ind)))
          CBP <- merge(CBP,sector_names,by=c('naics'))
          write.csv(CBP, file = sprintf("%s/CBP_%s_industry%s.csv", output_path, loc2, ind), row.names = FALSE)
          CBP[, sector := NULL]
          CBP[, naics := NULL]
          if(loc2 == "national"){
            CBP <- CBP[,lapply(.SD,sum,na.rm=T),list(year,supersector)][order(supersector,year)]
          }
          if(loc2 == "state"){
            CBP <- CBP[,lapply(.SD,sum,na.rm=T),list(year,state_fips,state_name,supersector)][order(state_fips,state_name,supersector,year)] 
          }
          if(loc2 == "cz"){
            CBP <- CBP[,lapply(.SD,sum,na.rm=T),list(year,cz,supersector)][order(cz,supersector,year)]
          }
          write.csv(CBP, file = sprintf("%s/CBP_%s_%s.csv", output_path, loc2, "supersector"), row.names = FALSE)
        }
      }
      
    } else {
      CBP <- setDT(read.csv(file = sprintf("%s/CBP_%s_total.csv", output_path, loc)))
      CBP <- merge(CBP,cz,by=c('state_fips','county_fips'))
      CBP[, state_fips := NULL][, county_fips := NULL]
      CBP <- CBP[,lapply(.SD,sum,na.rm=T),list(year,cz)][order(cz,year)]
      write.csv(CBP, file = sprintf("%s/CBP_%s_total.csv", output_path, "cz"), row.names = FALSE)
      
      CBP <- setDT(read.csv(file = sprintf("%s/CBP_%s_total.csv", output_path, loc)))
      CBP[, county_fips := NULL]
      CBP <- CBP[,lapply(.SD,sum,na.rm=T),list(year,state_fips)][order(state_fips,year)]
      CBP <- merge(state_names,CBP,by=c('state_fips'))
      write.csv(CBP, file = sprintf("%s/CBP_%s_total.csv", output_path, "state"), row.names = FALSE)
      
    }
  }
  if(loc == "county" & ind > 3){
    file.remove(sprintf("%s/CBP_%s_industry%s.csv", output_path, loc, ind), showWarnings = FALSE)
  }
}

