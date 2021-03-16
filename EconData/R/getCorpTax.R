

#' Download and manually correct corporate tax rate data
#' @param source_file (character)
#' @export
getStateCorpTax <- function(source_file){
  
  dd2 <- data.table()
  for(year in c(2002:2008,2010:2020)){
    if(year %in% c(2005,2006)){
      dd <- setDT(read.xlsx(source_file,sheet=as.character(year),colNames=T)[2:52,c(1,3)])
      setnames(dd,c('state','cit'))
      dd[,state := trimws(state)]
      dd[str_detect(cit,"\\r\\n"), cit := gsub("\\r\\n" , "," ,cit)]
      dd[str_detect(cit,","), cit := gsub("\\r\\n" , "," ,cit)]
      dd[,  cit := sub(",([^,]*)$", " -\\1", cit)]
      dd[, start := 1]
      dd[grepl("-",cit), start := str_locate(cit,"-")[,2] + 1]
      dd[, cit:= parse_number(str_sub(cit,start,start+4))]
    }
    if(year %in% c(2002:2004,2007:2008,2010:2020)){
      dd <- setDT(read.xlsx(source_file,sheet=as.character(year),colNames=T)[2:60,1:2])
      setnames(dd,c('state','cit'))
      if(year %in% c(2007)){
        dd <- dd[5:50]
      }
      if(year %in% c(2010)){
        dd <- dd[5:52]
      }
      if(year %in% c(2011:2012)){
        dd <- dd[6:56]
      }
      if(year %in% c(2013:2015)){
        dd <- dd[4:54]
      }
      if(year %in% 2016:2018){
        dd <- dd[3:53]
        dd[ grep("[*]",cit) , cit := 0]
        dd[, cit := str_replace(cit, "to", "-")]
      }
      if(year %in% 2019:2020){
        dd <- dd[4:54]
        dd[ grep("No Corporate",cit) , cit := 0]
        dd[, cit := str_replace(cit, "to", "-")]
      }
      if(year==2004){
        CapStr <- function(y) {
          c <- strsplit(y, " ")[[1]]
          paste(toupper(substring(c, 1,1)), substring(c, 2), sep="", collapse=" ")
        }
        dd$state <- sapply(dd$state, tolower)
        dd$state <- sapply(dd$state, CapStr)
        dd[state=='Dist. Of Columbia', state := 'District of Columbia']
      }
      if(year==2002){
        dd <- rbind(dd,data.table(state="Michigan",cit=2)) # Michigan is missing from file
      }
      if(year %in% c(2003,2004,2007)){
        dd <- rbind(dd,data.table(state="Michigan",cit=1.9)) # Michigan is missing from file
      }
      if(year < 2011){
        for(zero_state in c('Nevada','Washington','Wyoming')){
          dd <- rbind(dd,data.table(state=zero_state,cit=0)) # zero states missing from file
        }
      }
      if(year %in% c(2002:2004)){
        dd <- rbind(dd,data.table(state='Texas',cit=0)) # Texas is missing from file
      }
      if(year==2007){
        dd <- rbind(dd,data.table(state='Texas',cit=4.5)) # Texas is missing from file
      }
    }
    dd[cit=='---',cit := 0]
    dd <- dd[nchar(state) < 30 & !grepl('note|tate',state) & !is.na(state)]
    dd[, start := 1]
    dd[grepl("-",cit), start := str_locate(cit," - ")[,2] + 1]
    dd[, cit_clean := parse_number(str_sub(cit,start,start+4))]
    dd[, year := (year)]
    dd[, cit := copy(cit_clean)][, cit_clean := NULL][, start := NULL]
    dd <- dd[order(state)]
    dd2 <- rbind(dd2,dd)
  }
  dd2[,state := trimws(state)]
  dd2[, cit := round(cit,3)]
  dd2[state=='Delaware (3)', state := 'Delaware']
  
  
  ## manual perfections based on footnotes etc.
  for(zero_state in c('Nevada','South Dakota','Washington','Wyoming')){
    dd2[state==zero_state, cit := 0] # states with no taxes
  }
  dd2[year %in% 2012:2016 & state=='Connecticut', cit := 9.0] # 20% surtax, 7.5% * (1 + .2) = 9%
  dd2[year %in% 2018:2020 & state=='Connecticut', cit := 8.25] # 10% surtax, 7.5% * (1 + .1) = 8.25%
  dd2[state=='Delaware', cit := 8.7] # in footnote for each year
  dd2[year %in% c(2005,2006) & state=='Illinois', cit := 7.3] # add in personal property replacement tax of 2.5pp
  dd2[year==2012 & state=='Indiana', cit := 8.25] # mid-year phase in of reduction from 8.5% to 8.0%
  dd2[year==2015 & state=='Indiana', cit := 6.75] # mid-year phase in of reduction from 7% to 6.5%
  dd2[year==2019 & state=='Indiana', cit := 5.75] # mid-year phase in 
  dd2[year==2020 & state=='Indiana', cit := 5.5] # mid-year phase in 
  dd2[year %in% 2002:2008 & state=='Kansas', cit := 7.35] # 3.35pp surtax
  dd2[year %in% 2010 & state=='Kansas', cit := 7.05] # 3.05pp surtax
  dd2[year %in% 2011:2020 & state=='Kansas', cit := 7.00] # 3.00pp surtax
  dd2[year==2005 & state=='Kentucky', cit := 8.25] # year of tax change differs between the two Tax Foundation sources
  dd2[year %in% c(2005,2006) & state=='Massachusetts', cit := 9.5] # did not parse correctly
  dd2[year == 2015 & state=='New Mexico', cit := 6.9] # did not parse correctly
  dd2[year %in% c(2005,2006) & state=='New York', cit := 7.5] # did not parse correctly
  dd2[year==2008 & state=='New York', cit := 7.1] # differs between Tax Foundation sources
  dd2[year==2006 & state=='Ohio', cit := 8.5] # did not parse correctly
  dd2[year >= 2010 & state=='Ohio', cit := 0.26] # see footnotes, this is a gross receipts tax
  dd2[year >= 2008 & year < 2015 & state=='Texas', cit := 1.0] # gross receipts tax in footnotes
  dd2[year >= 2015 & state=='Texas', cit := 0.5] # on Texas govt website
  dd2[year==2007 & state=='Vermont', cit := 8.9] # differs across Tax Foundation files
  dd2[year==2007 & state=='West Virginia', cit := 9.0] # differs between Tax Foundation sources
  dd2[year==2008 & state=='West Virginia', cit := 8.75] # differs between Tax Foundation sources
  dd2[year==2012 & state=='West Virginia', cit := 7.75] # differs between Tax Foundation sources
  # New Jersey is complicated by a surcharge, unclear how to count it
  
  
  ## add 2009 manually since it is not in main Tax Foundation file, manually set the handful of changes relative to 2008
  dd2 <- rbind(dd2,copy(dd2[year==2008])[, year := 2009])
  dd2[year==2009 & state=='Ohio', cit := 0.26]
  dd2[year==2009 & state=='Oregon', cit := 7.9]
  dd2[year==2009 & state=='Kansas', cit := 7.05]
  dd2[year==2009 & state=='West Virginia', cit := 8.5] # differs between Tax Foundation sources
  
  ## add 2001, manually set the handful of changes relative to 2002
  dd2 <- rbind(dd2,copy(dd2[year==2002])[, year := 2001])
  dd2[year==2001 & state=='Idaho', cit := 8.0]
  dd2[year==2001 & state=='Michigan', cit := 2.1]
  dd2[year==2001 & state=='New York', cit := 8.0]
  
  ## add 2000, manually set the handful of changes relative to 2001
  dd2 <- rbind(dd2,copy(dd2[year==2001])[, year := 2000])
  dd2[year==2000 & state=='Alabama', cit := 5.0]
  dd2[year==2000 & state=='Arizona', cit := 7.968]
  dd2[year==2000 & state=='Colorado', cit := 4.75]
  dd2[year==2000 & state=='Michigan', cit := 2.2]
  dd2[year==2000 & state=='New Hampshire', cit := 7.25]
  dd2[year==2000 & state=='New York', cit := 8.5] # mid-year phase in

  ## finish
  dd2 <- dd2[order(state,year)]
  setnames(dd2,'state','state_name')
  return(dd2)

}


#' Download corporate tax rate data from various sources
#' @param source_path (character)
#' @export
getCorpTaxSources <- function(source_path){

  temp_path <- tempdir()
  
  # Giroud & Rauh (2020, JPE) data
  Giroud_Rauh <- 'http://www.columbia.edu/~xg2285/JPE_Data.zip'
  destfile = sprintf("%s/Giroud_Rauh.zip",temp_path)
  if(!file.exists(destfile)){
    download.file(Giroud_Rauh,destfile)
    unzip(zipfile=destfile,exdir=temp_path)
    GR <- setDT(read.dta13(sprintf("%s/JPE_Data_Appendix/stata_tax_data.dta",temp_path)))
    GR <- GR[,list(state_fips=fips,state_name,year,cit,cit_flag)][order(state_name,year)]
    write.csv(GR, file = sprintf("%sGiroudRauh_1976_2012.csv",source_path), row.names = F)
  }
  
  # download Tax Foundation 2000-2014 data
  TF_legacy <- 'https://files.taxfoundation.org/legacy/docs/State%20Corporate%20Income%20Tax%20Rates%2C%202000-2014.xlsx'
  destfile = sprintf("%sTaxFoundation_2000_2014.xlsx",source_path)
  if(!file.exists(destfile)){
    download.file(TF_legacy,destfile)
  }
  
  TF_newer <- 'https://www.taxpolicycenter.org/sites/default/files/statistics/spreadsheet/state_corporate_income_tax_3.xlsx'
  dest_file= sprintf("%sTaxFoundation_2002_2020.xlsx",source_path)
  if(!file.exists(dest_file)){
    download.file(TF_newer,dest_file)
  }
  
  # my version
  # download Tax Foundation 2002-2020 data
  # secondary source: 'https://files.taxfoundation.org/legacy/docs/State%20Corporate%20Income%20Tax%20Rates%2C%202000-2014.xlsx'
  TF_newer <- 'https://www.taxpolicycenter.org/sites/default/files/statistics/spreadsheet/state_corporate_income_tax_3.xlsx'
  tax_file= sprintf("%sTaxFoundation_2002_2020.xlsx",source_path)
  if(!file.exists(tax_file)){
    download.file(TF_newer,tax_file)
  }
  
  
  destfile <- sprintf("%sStateCorpTax_without_fips.csv",source_path)
  if(!file.exists(destfile)){
    rates <- getStateCorpTax(source_file = tax_file)
    write.csv(rates, file=destfile, row.names=F)
  }
  
}

