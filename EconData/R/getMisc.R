
#' Download and prepare commuting zone data
#' @param output_path (character)
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
#' @param output_path (character)
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
#' @param miles (integer)
#' @param output_path (character)
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
#' @param miles (integer)
#' @param output_path (character)
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
#' @param miles (integer)
#' @param output_path (character)
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
#' @param miles (integer)
#' @param output_path (character)
#' @export
getDistances <- function(miles=100, output_path = "~/github/EconData/DataRepo/Miscellaneous/"){
  
  getCountyDistances(miles=miles, output_path=output_path)
  buildCZDistance(miles=miles, output_path=output_path)
  buildStateDistance(miles=miles, output_path=output_path)
  
}



#' Download and prepare tradables data
#' @param output_path (character)
#' @export
getTradables <- function(output_path = "~/github/EconData/DataRepo/Miscellaneous/"){
  
  path <- tempdir()
  url <- "https://scholar.princeton.edu/sites/default/files/atif/files/miansufieconometrica_publicreplicationfiles.zip"
  destfile <- sprintf("%s/MianSufi.zip", path)
  unzipped <- sprintf("%s/MianSufiEconometrica_PublicReplicationFiles/miansufieconometrica_countyindustrylevel.dta", path)
  download.file(url,destfile)
  unzip(zipfile = destfile, exdir = path)
  dd <- setDT(foreign::read.dta(file=unzipped))
  dd <- unique(dd[,.(naics,industry,indcat)])
  dd[, naics := readr::parse_number(naics)]
  setnames(dd,'indcat','tradable')
  dd <- dd[tradable %in% c('tradable','non-tradable')][order(naics)]
  write.csv(dd,file=sprintf("%sMianSufi2014_tradables.csv",output_path),row.names=F)
  
}

#' Download and prepare FRED CPI data (measured on January 1)
#' @param output_path (character)
#' @export
getCPI <- function(output_path = "~/github/EconData/DataRepo/Miscellaneous/"){
  
  # https://fred.stlouisfed.org/series/CPALTT01USA661S
  path <- tempdir()
  url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=748&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPALTT01USA661S&scale=left&cosd=1960-01-01&coed=2020-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Annual&fam=avg&fgst=lin&fgsnd=2020-01-01&line_index=1&transformation=lin&vintage_date=2021-03-16&revision_date=2021-03-16&nd=1960-01-01"
  destfile <- sprintf("%s/FRED_CPI.csv", path)
  download.file(url, destfile)
  cpi <- setDT(read.csv(file=destfile))
  cpi <- cpi[, year := as.integer(substr(DATE,1,4))][, DATE := NULL][, cpi := CPALTT01USA661S/100][,list(year,cpi,cpi_factor=1/cpi)]
  write.csv(cpi,file=sprintf("%sFRED_CPI.csv",output_path),row.names=F)
  
}


#' Download and prepare BEA GDP data, collapsed by CZ and NAICS-2
#' @param output_path (character)
#' @export
getGDP <- function(output_path = "~/github/EconData/DataRepo/Miscellaneous/"){
  
  path <- tempdir()
  url <- "https://apps.bea.gov/regional/zip/CAGDP2.zip"
  destfile <- sprintf("%s/BEA_GDP.zip", path)
  unzipped <- sprintf("%s/CAGDP2__ALL_AREAS_2001_2019.csv", path)
  download.file(url,destfile)
  unzip(zipfile = destfile, exdir = path)
  dd <- setDT(read.csv(file=unzipped))
  
  dd <- dd[IndustryClassification  %in% c('11','21','22','23','31-33','42','44-45','48-49','51','52','53','54','55','56','61','62','71','72','81')]
  dd[, fips3 := substr(GeoFIPS, 4,6)]
  dd <- dd[fips3 != '000']
  setnames(dd,c('IndustryClassification','GeoFIPS'),c('naics','fips'))
  dd[, TableName := NULL][, Unit := NULL][, GeoName := NULL][, Region := NULL][, LineCode := NULL][, Description := NULL][, fips3 := NULL]
  dd[naics=='31-33', naics := '31']
  dd[naics=='44-45', naics := '44']
  dd[naics=='48-49', naics := '48']
  dd[, naics := as.integer(as.character(naics))]
  dd <- melt(dd,id.vars = c('fips','naics'))
  dd[, year := as.integer(substr(variable,2,5))][,variable := NULL]
  dd[, state_fips := as.integer(as.character(substr(fips,1,3)))]
  dd[, county_fips := as.integer(as.character(substr(fips,4,6)))]
  dd[, value := as.numeric(as.character(value))]
  dd[, fips := NULL]
  cz <- setDT(read.csv('~/github/EconData/DataRepo/Miscellaneous/cz_crosswalk_2000.csv'))
  dd <- merge(dd,cz,by=c('state_fips','county_fips'))
  dd <- dd[,list(log_gdp = log(sum(value,na.rm=T))),list(year,industry=naics,location=cz)][!is.infinite(log_gdp) & !is.na(log_gdp)]
  
  write.csv(dd,file=sprintf("%sBEA_GDP.csv",output_path),row.names=F)
  
}


