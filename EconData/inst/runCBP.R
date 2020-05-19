
library(EconData)
library(futile.logger)
for (ind in c(0, 2, 3, 4, 6)) {
  for (loc in c("national", "state", "county")) { 
    if (!(loc == "county" & ind > 2) & !(loc == "state" & ind == 6)) {
      flog.info("location %s, industry %s", loc, ind)
      CBP <- getCBP(years = 2001:2017, location = loc, industry = ind)
      if (ind > 0) {
        write.csv(CBP, file = sprintf("~/github/EconData/DataRepo/CensusCBP/CBP_%s_industry%s.csv", loc, ind), row.names = F)
      } else {
        write.csv(CBP, file = sprintf("~/github/EconData/DataRepo/CensusCBP/CBP_%s_total.csv", loc), row.names = F)
      }
    }
  }
}
