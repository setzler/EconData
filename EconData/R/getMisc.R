
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
  county_cz <- county_cz[order(state_fips, county_fips)]
  county_cz <- county_cz[,.(state_fips, county_fips, cz)]
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
  write.csv(state_fips[order(state_name)],file=sprintf("%sstate_fips_crosswalk.csv",output_path),row.names=F)
  file.remove(destfile)
  
}



#' Download county distances
#' @params miles
#' @params output_path (character)
#' @export
getCountyDistances <- function(miles=100, output_path = "~/github/EconData/DataRepo/Miscellaneous/"){
  
  path <- tempdir()
  destfile <- sprintf("%s/county_dist.dta", path)
  county_dist_file <- sprintf("https://data.nber.org/distance/2000/sf1/county/sf12000countydistance%smiles.dta.zip",miles)
  download.file(county_dist_file,destfile)
  unzip(zipfile = destfile, exdir = path)
  county_dist <- setDT(readstata13::read.dta13(file=sprintf("%s/sf12000countydistance%smiles.dta", path, miles)))
  county_dist <- county_dist[,list(county1,county2,distance=mi_to_county)][order(county1,county2)]
  write.csv(county_dist,file=sprintf("%sdistances/county_distance_%smiles.csv", output_path, miles),row.names=F)
  file.remove(destfile)
  
  
}



#' CZ distance
#' @params miles
#' @params output_path (character)
#' @export
buildCZDistance <- function(miles=100, output_path = "~/github/EconData/DataRepo/Miscellaneous/"){
  
  county_dist <- setDT(read.csv(file=sprintf("%sdistances/county_distance_%smiles.csv", output_path, miles)))
  county_dist[, county1 := as.character(county1)]
  county_dist[, county2 := as.character(county2)]
  county_dist[, len_county1 := nchar(county1)]
  county_dist[len_county1==4, county1 := paste0("0",county1)]
  county_dist[, len_county2 := nchar(county2)]
  county_dist[len_county2==4, county2 := paste0("0",county2)]
  county_dist[, state_fips1 := as.integer(substr(county1,1,2))]
  county_dist[, county_fips1 := as.integer(substr(county1,3,5))]
  county_dist[, state_fips2 := as.integer(substr(county2,1,2))]
  county_dist[, county_fips2 := as.integer(substr(county2,3,5))]
  county_dist <- county_dist[,list(state_fips1,county_fips1,state_fips2,county_fips2,distance)][state_fips1 != "72" & state_fips2 != "72"]
  
  county_to_cz <- setDT(read.csv(file=sprintf("%scz_crosswalk_2000.csv",output_path)))
  setnames(county_to_cz, c("county_fips","cz","state_fips"), c("county_fips1","cz1","state_fips1"))
  county_dist <- merge(county_dist, county_to_cz, by=c("state_fips1","county_fips1"))
  setnames(county_to_cz, c("county_fips1","cz1","state_fips1"), c("county_fips2","cz2","state_fips2"))
  county_dist <- merge(county_dist, county_to_cz, by=c("state_fips2","county_fips2"))
  county_dist <- county_dist[cz1 != cz2][, list(distance = min(distance)), list(cz1,cz2)][order(cz1,cz2)]
  county_dist <- unique(rbind(county_dist, county_dist[,list(cz1=cz2, cz2=cz1, distance)]))[order(cz1,cz2)]
  
  write.csv(county_dist,file=sprintf("%sdistances/CZ_distance_%smiles.csv", output_path, miles),row.names=F)
  
}

#' State distance
#' @params miles
#' @params output_path (character)
#' @export
buildStateDistance <- function(miles=100, output_path = "~/github/EconData/DataRepo/Miscellaneous/"){
  
  county_dist <- setDT(read.csv(file=sprintf("%sdistances/county_distance_%smiles.csv", output_path, miles)))
  county_dist[, county1 := as.character(county1)]
  county_dist[, county2 := as.character(county2)]
  county_dist[, len_county1 := nchar(county1)]
  county_dist[len_county1==4, county1 := paste0("0",county1)]
  county_dist[, len_county2 := nchar(county2)]
  county_dist[len_county2==4, county2 := paste0("0",county2)]
  county_dist[, state_fips1 := as.integer(substr(county1,1,2))]
  county_dist[, county_fips1 := as.integer(substr(county1,3,5))]
  county_dist[, state_fips2 := as.integer(substr(county2,1,2))]
  county_dist[, county_fips2 := as.integer(substr(county2,3,5))]
  county_dist <- county_dist[,list(state_fips1,state_fips2,distance)][state_fips1 != "72" & state_fips2 != "72"]
  county_dist <- county_dist[state_fips1 != state_fips2][, list(distance = min(distance)), list(state_fips1, state_fips2)][order(state_fips1, state_fips2)]
  county_dist <- unique(rbind(county_dist, county_dist[,list(state_fips1=state_fips2, state_fips2=state_fips1, distance)]))
  write.csv(county_dist,file=sprintf("%sdistances/state_distance_%smiles.csv", output_path, miles),row.names=F)
  
}


#' Get all distances
#' @params miles
#' @params output_path (character)
#' @export
getDistances <- function(miles=100, output_path = "~/github/EconData/DataRepo/Miscellaneous/"){
  
  getCountyDistances(miles=miles, output_path=output_path)
  buildCZDistance(miles=miles, output_path=output_path)
  buildStateDistance(miles=miles, output_path=output_path)
  
}


