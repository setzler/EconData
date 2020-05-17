
#' Download and prepare CBP data
#' @params years (integer) any integer between 2000 and 2017 is supported.
#' @params aggregation (character) options include: "national_total", "national_industry", "state_total", "state_industry", "county_total", "county_industry".
#' @params LFO (character) legal form of organization.
#' @export
getCBP <- function(years = 2017, aggregation = "state_total", LFO = '-') {
  
  ## check inputs
  if (!(aggregation %in% c("national_total", "national_industry", "state_total", "state_industry", "county_total", "county_industry"))) {
    stop(sprintf("aggregation=%s is not available through the EconData package. Must be one of: national_total, national_industry, state_total, state_industry, county_total, county_industry", aggregation))
  }
  for (year in years) {
    if (year < 2001 | year > 2017) {
      stop(sprintf("year=%s is not yet available through the EconData package.", year))
    }
  }
  
  ## global parameters
  path <- tempdir()
  if (grepl("state", aggregation)) {
    agg <- "st"
    main_varnames <- c("fipstate", "naics", "emp", "est", "qp1")
    uppercase_years <- 2015
    LFO_years <- 2010:2017
    upperC_years <- 0
  }
  if (grepl("county", aggregation)) {
    agg <- "co"
    main_varnames <- c("fipstate", "fipscty", "naics", "emp", "est", "qp1")
    uppercase_years <- 2015
    LFO_years <- 0
    upperC_years <- c(2002, 2007:2008)
  }
  if (grepl("national", aggregation)) {
    agg <- "us"
    main_varnames <- c("naics", "emp", "est", "qp1")
    uppercase_years <- c(2006, 2015)
    upperC_years <- 2002:2009
    LFO_years <- 2008:2017
  }
  
  if(LFO != '-'){
    for (year in years) {
      if (!(year %in% LFO_years)) {
        print("Valid LFO years are:")
        print(LFO_years)
        stop(sprintf("year=%s does not have LFO information.", year))
      }
    }
  }
  
  ## loop over years
  dd_output <- data.table()
  for (year in years) {
    ## set year-specific parameters
    year_sub <- substr(year, 3, 4)
    varnames <- copy(main_varnames)
    if(year %in% LFO_years){
      varnames <- c(varnames,'lfo')
    }
    if(year %in% uppercase_years){
      varnames <- toupper(varnames)
    }
    if(year %in% upperC_years){
      extractfile <- sprintf("%s/Cbp%s%s.txt", path, year_sub, agg)
    } else {
      extractfile <- sprintf("%s/cbp%s%s.txt", path, year_sub, agg)
    }
    
    ## set up download from CBP website
    url <- sprintf("https://www2.census.gov/programs-surveys/cbp/datasets/%s/cbp%s%s.zip", year, year_sub, agg)
    destfile <- sprintf("%s/CBP_%s.zip", path, year)
    if (grepl("national", aggregation) & year <= 2007) {
      url <- sprintf("https://www2.census.gov/programs-surveys/cbp/datasets/%s/cbp%s%s.txt", year, year_sub, agg)
      destfile <- sprintf("%s/CBP_%s.txt", path, year)
    }

    ## download
    flog.info("downloading CBP for year %s aggregated by %s.", year, aggregation)
    download.file(url, destfile)

    ## extract
    if (grepl("national", aggregation) & year <= 2007) {
      ddin <- setDT(fread(destfile, select = varnames))
      file.remove(destfile)
    } else {
      unzip(zipfile = destfile, exdir = path)
      file.remove(destfile)
      ddin <- setDT(fread(extractfile, select = varnames))
      file.remove(extractfile)
    }
    unlink(path)

    ## clean data
    setnames(ddin, tolower(names(ddin)))
    ddin[, qp1 := qp1 * 1e3]
    setnames(ddin, c("emp", "qp1", "est"), c("employment_march", "payroll_quarter1", "establishments"))
    if ("fipstate" %in% names(ddin)) {
      setnames(ddin, "fipstate", "state_fips")
    }
    if ("fipscty" %in% names(ddin)) {
      setnames(ddin, "fipscty", "county_fips")
    }
    if ("lfo" %in% names(ddin)) {
      ddin <- ddin[lfo == LFO]
      ddin[, lfo := NULL]
    }
    if (grepl("total", aggregation)) {
      ddin <- ddin[naics == "------"]
      ddin[, naics := NULL]
    }
    gc()

    ## verify uniqueness
    if (aggregation == "county_total") {
      uniques <- ddin[, .N, list(state_fips, county_fips)]
    }
    if (aggregation == "county_industry") {
      uniques <- ddin[, .N, list(state_fips, county_fips, naics)]
    }
    if (aggregation == "state_total") {
      uniques <- ddin[, .N, list(state_fips)]
    }
    if (aggregation == "state_industry") {
      uniques <- ddin[, .N, list(state_fips, naics)]
    }
    if (aggregation == "national_total") {
      uniques <- data.table(N = nrow(ddin))
    }
    if (aggregation == "national_industry") {
      uniques <- ddin[, .N, list(naics)]
    }

    if (max(uniques$N) > 1) {
      stop("rows are not unique at the provided level of aggregation")
    }

    ddin$year <- year
    dd_output <- rbind(dd_output, ddin)
  }

  return(dd_output)
}
