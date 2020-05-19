
#' Download and prepare commuting zone data
#' @params output_path (character)
#' @export
getCZ <- function(output_path = "~/github/EconData/DataRepo/Miscellaneous/"){
  
  path <- tempdir()
  destfile <- sprintf("%s/cz.xls", path, year)
  county_cz_file <- 'https://www.ers.usda.gov/webdocs/DataFiles/48457/cz00_eqv_v1.xls?v=0'
  download.file(county_cz_file,destfile)
  county_cz <- setDT(readxl::read_xls(destfile))[,.SD,.SDcols=c("FIPS","Commuting Zone ID, 2000")]
  setnames(county_cz,c("county_fips",'cz'))
  county_cz[, state_fips := as.integer(substr(county_fips,1,2))]
  county_cz[, county_fips := as.integer(substr(county_fips,3,5))]
  write.csv(county_cz,file=sprintf("%scz_crosswalk_2000.csv",output_path),row.names=F)
  file.remove(destfile)
  
}

#' Download and prepare state FIPS codes
#' @params output_path (character)
#' @export
getStateFips <- function(output_path = "~/github/EconData/DataRepo/Miscellaneous/"){
  
  path <- tempdir()
  destfile <- sprintf("%s/state.xls", path, year)
  state_fips_file <- 'https://www2.census.gov/programs-surveys/popest/geographies/2016/state-geocodes-v2016.xls'
  download.file(state_fips_file,destfile)
  state_fips <- setDT(readxl::read_xls(destfile)[6:69,3:4])#[,.SD,.SDcols=c("FIPS","Commuting Zone ID, 2000")]
  setnames(state_fips,c("state_fips",'state_name'))
  state_fips <- state_fips[state_fips != '00']
  write.csv(state_fips,file=sprintf("%sstate_fips_crosswalk.csv",output_path),row.names=F)
  file.remove(destfile)
  
}

