
#' Download and prepare CBP data
#' @params years (integer) any integer between 2000 and 2017 is supported.
#' @params location (character) options are "county", "state", "national".
#' @params industry (integer) options are 0, 2, 3, 4, 6.
#' @params LFO (character) legal form of organization.
#' @export
getCBP <- function(years = 2017, location = "national", industry = 0, LFO = "-") {

  ## check inputs
  if (!(location %in% c("national", "state", "county"))) {
    stop(sprintf("location=%s is not available.", location))
  }
  if (!(industry %in% c(0, 2, 3, 4, 6))) {
    stop(sprintf("location=%s is not available.", location))
  }
  for (year in years) {
    if (year < 2001 | year > 2017) {
      stop(sprintf("year=%s is not yet available through the EconData package.", year))
    }
  }

  ## global parameters
  path <- tempdir()
  if (location == "state") {
    agg <- "st"
    main_varnames <- c("fipstate", "naics", "emp", "est", "qp1")
    uppercase_years <- 2015
    LFO_years <- 2010:2017
    upperC_years <- 0
  }
  if (location == "county") {
    agg <- "co"
    main_varnames <- c("fipstate", "fipscty", "naics", "emp", "est", "qp1")
    uppercase_years <- 2015
    LFO_years <- 0
    upperC_years <- c(2002, 2007:2008)
  }
  if (location == "national") {
    agg <- "us"
    main_varnames <- c("naics", "emp", "est", "qp1")
    uppercase_years <- c(2006, 2015)
    upperC_years <- 2002:2009
    LFO_years <- 2008:2017
  }

  if (LFO != "-") {
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
    if (year %in% LFO_years) {
      varnames <- c(varnames, "lfo")
    }
    if (year %in% uppercase_years) {
      varnames <- toupper(varnames)
    }
    if (year %in% upperC_years) {
      extractfile <- sprintf("%s/Cbp%s%s.txt", path, year_sub, agg)
    } else {
      extractfile <- sprintf("%s/cbp%s%s.txt", path, year_sub, agg)
    }

    ## set up download from CBP website
    url <- sprintf("https://www2.census.gov/programs-surveys/cbp/datasets/%s/cbp%s%s.zip", year, year_sub, agg)
    destfile <- sprintf("%s/CBP_%s.zip", path, year)
    if (location == "national" & year <= 2007) {
      url <- sprintf("https://www2.census.gov/programs-surveys/cbp/datasets/%s/cbp%s%s.txt", year, year_sub, agg)
      destfile <- sprintf("%s/CBP_%s.txt", path, year)
    }

    ## download
    flog.info("downloading CBP for year %s aggregated by location='%s' and industry digits=%s.", year, location, industry)
    download.file(url, destfile)

    ## extract
    if (location == "national" & year <= 2007) {
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
    if (industry == 0) {
      ddin <- ddin[naics == "------"]
      ddin[, naics := NULL]
    }
    if (industry == 2) {
      search_for <- paste(rep("-", 4), collapse = "")
      ddin <- ddin[grepl(search_for, naics) & naics != "------"]
      ddin[, naics := substr(naics, 1, 2)]
    }
    if (industry %in% c(2,3,4)) {
      search_for <- paste(rep("/", 6 - industry), collapse = "")
      no_search_for <- paste(rep("/", 6 - industry + 1), collapse = "")
      ddin <- ddin[grepl(search_for, naics) & !grepl(no_search_for, naics)]
      ddin[, naics := substr(naics, 1, industry)]
    }
    if (industry == 6) {
      ddin <- ddin[!grepl("/", naics) & !grepl("-", naics)]
      ddin[, naics := substr(naics, 1, industry)]
    }
    gc()

    ## verify uniqueness
    if (location == "county" & industry == 0) {
      uniques <- ddin[, .N, list(state_fips, county_fips)]
    }
    if (location == "county" & industry > 0) {
      uniques <- ddin[, .N, list(state_fips, county_fips, naics)]
    }
    if (location == "state" & industry == 0) {
      uniques <- ddin[, .N, list(state_fips)]
    }
    if (location == "state" & industry > 0) {
      uniques <- ddin[, .N, list(state_fips, naics)]
    }
    if (location == "national" & industry == 0) {
      uniques <- data.table(N = nrow(ddin))
    }
    if (location == "national" & industry > 0) {
      uniques <- ddin[, .N, list(naics)]
    }

    if (max(uniques$N) > 1) {
      stop("rows are not unique at the provided level of aggregation")
    }

    ddin[employment_march == 0, employment_march := NA]
    ddin[payroll_quarter1 == 0, payroll_quarter1 := NA]
    ddin$year <- year
    dd_output <- rbind(dd_output, ddin)
  }

  return(dd_output)
}
