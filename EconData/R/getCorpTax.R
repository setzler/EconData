

#' Download and manually correct corporate tax rate data
#' @export
getCorrectedCorpTax <- function(){
  
  # secondary source: 'https://files.taxfoundation.org/legacy/docs/State%20Corporate%20Income%20Tax%20Rates%2C%202000-2014.xlsx'
  tax_file <- 'https://www.taxpolicycenter.org/file/178481/download?token=rYZdmxwX' # primary source
  dd2 <- data.table()
  for(year in c(2002:2008,2010:2015)){
    if(year %in% c(2005,2006)){
      dd <- setDT(read.xlsx(tax_file,sheet=as.character(year),colNames=T)[2:52,c(1,3)])
      setnames(dd,c('state','cit'))
      dd[,state := trimws(state)]
      dd[str_detect(cit,"\\r\\n"), cit := gsub("\\r\\n" , "," ,cit)]
      dd[str_detect(cit,","), cit := gsub("\\r\\n" , "," ,cit)]
      dd[,  cit := sub(",([^,]*)$", " -\\1", cit)]
      dd[, start := 1]
      dd[grepl("-",cit), start := str_locate(cit,"-")[,2] + 1]
      dd[, cit_clean := parse_number(str_sub(cit,start,start+4))]
      dd[state=='Massachusetts', cit_clean := 9.5]
      dd[state=='New York', cit_clean := 7.5]
      dd[state=='Illinois', cit_clean := 7.3]
      if(year==2006){
        dd[state=='Ohio', cit_clean := 8.5]
      }
      if(year==2005){
        dd[state=='Kentucky', cit_clean := 8.25] # year of tax change differs between the two Tax Foundation sources
      }
      for(zero_state in c('Nevada','South Dakota','Washington','Wyoming')){
        dd[state==zero_state, cit_clean := 0]
      }
      dd[, cit := copy(cit_clean)][, cit_clean := NULL]
    }
    if(year %in% c(2002:2004,2007:2008,2010:2015)){
      dd <- setDT(read.xlsx(tax_file,sheet=as.character(year),colNames=T)[2:60,1:2])
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
      if(year==2004){
        CapStr <- function(y) {
          c <- strsplit(y, " ")[[1]]
          paste(toupper(substring(c, 1,1)), substring(c, 2), sep="", collapse=" ")
        }
        dd$state <- sapply(dd$state, tolower)
        dd$state <- sapply(dd$state, CapStr)
        dd[state=='Dist. Of Columbia', state := 'District of Columbia']
        dd[state=='South Dakota', cit := 0.0]
      }
      if(year==2002){
        dd <- rbind(dd,data.table(state="Michigan",cit=2))
      }
      if(year %in% c(2003,2004,2007)){
        dd <- rbind(dd,data.table(state="Michigan",cit=1.9))
      }
      if(year %in% 2011:2015){
        for(zero_state in c('Nevada','Washington','Wyoming')){
          dd[state==zero_state, cit := 0]
        }
      } else {
        for(zero_state in c('Nevada','Washington','Wyoming')){
          dd <- rbind(dd,data.table(state=zero_state,cit=0))
        }
      }
      if(year %in% c(2002:2004)){
        for(zero_state in c('Texas')){
          dd <- rbind(dd,data.table(state=zero_state,cit=0))
        }
      }
      if(year %in% c(2007)){
        dd <- rbind(dd,data.table(state='Texas',cit=4.5))
        dd[state=='Vermont', cit := 8.9] # differs across Tax Foundation files
      }
      if(year %in% c(2008,2010:2015)){
        dd[state=='Texas', cit := 0.0]
        dd[state=='South Dakota', cit := 0.0]
      }
      if(year >= 2010){
        dd[state=='Ohio', cit := 0.26]
      }
      if(year==2015){
        dd[state=='New Mexico', cit := 6.9]
      }
      if(year==2012){
        dd[state=='Indiana', cit := 8.25] # mid-year tax reduction
      }
      if(year %in% c(2012,2013,2014,2015)){
        dd[state=='Connecticut', cit := 9.0]
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
  
  ## add 2000, 2001, 2009
  dd2 <- rbind(dd2,copy(dd2[year==2002])[, year := 2001])
  dd2 <- rbind(dd2,copy(dd2[year==2008])[, year := 2009])
  dd2[year==2001 & state=='Michigan', cit := 2.1]
  dd2[year==2001 & state=='Idaho', cit := 8.0]
  dd2[year==2001 & state=='New York', cit := 8.0]
  dd2[year==2009 & state=='Ohio', cit := 0.26]
  dd2[year==2009 & state=='Oregon', cit := 7.9]
  dd2[year %in% 2001:2008 & state=='Kansas', cit := 7.35]
  dd2[year %in% 2009:2010 & state=='Kansas', cit := 7.05]
  dd2[year %in% 2011:2015 & state=='Kansas', cit := 7.00]
  dd2 <- rbind(dd2,copy(dd2[year==2001])[, year := 2000])
  dd2[year==2000 & state=='Alabama', cit := 5.0]
  dd2[year==2000 & state=='Arizona', cit := 7.968]
  dd2[year==2000 & state=='Colorado', cit := 4.75]
  dd2[year==2000 & state=='Michigan', cit := 2.2]
  dd2[year==2000 & state=='New Hampshire', cit := 7.25]
  dd2[year==2000 & state=='New York', cit := 8.5] # mid-year phase in
  dd2[year %in% c(2008,2009) & state=='New York', cit := 7.1] # differs between Tax Foundation sources
  dd2[year==2007 & state=='West Virginia', cit := 9.0] # differs between Tax Foundation sources
  dd2[year==2008 & state=='West Virginia', cit := 8.75] # differs between Tax Foundation sources
  dd2[year==2012 & state=='West Virginia', cit := 7.75] # differs between Tax Foundation sources
  dd2[year==2012 & state=='Indiana', cit := 8.25] # mid-year phase in
  dd2 <- dd2[order(state,year)]
  

  return(dd2)

}


#' Download and prepare corporate tax rate data from various sources
#' @params output_path (character)
#' @export
getCorpTax <- function(output_path = "~/github/EconData/DataRepo/StateCorpTax/"){
  
  temp_path <- tempdir()
  
  # my version
  write.csv(getCorrectedCorpTax(), file=sprintf("%sCorrected_2000_2015.csv",output_path), row.names=F)
  
  # Giroud & Rauh (2020, JPE) data
  Giroud_Rauh <- 'http://www.columbia.edu/~xg2285/JPE_Data.zip'
  destfile = sprintf("%s/Giroud_Rauh.zip",temp_path)
  download.file(Giroud_Rauh,destfile)
  unzip(zipfile=destfile,exdir=temp_path)
  GR <- setDT(read.dta13(sprintf("%s/JPE_Data_Appendix/stata_tax_data.dta",temp_path)))
  GR <- GR[,list(state=state_name,state_fips=fips,year,cit,cit_flag)][order(state,year)]
  write.csv(GR, file = sprintf("%sGiroudRauh_1976_2012.csv",output_path), row.names = F)
  
  # download Tax Foundation 2000-2014 data
  TF_legacy <- 'https://files.taxfoundation.org/legacy/docs/State%20Corporate%20Income%20Tax%20Rates%2C%202000-2014.xlsx'
  destfile = sprintf("%sTaxFoundation_2000_2014.xlsx",output_path)
  download.file(TF_legacy,destfile)
  
  # download Tax Foundation 2002-2020 data
  TF_newer <- 'https://www.taxpolicycenter.org/file/178481/download?token=rYZdmxwX'
  destfile = sprintf("%sTaxFoundation_2002_2020.xlsx",output_path)
  download.file(TF_newer,destfile)
  
}

