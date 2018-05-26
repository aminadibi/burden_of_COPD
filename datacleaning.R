options(java.parameters = "- Xmx1024m") #prevents Java memory issue with XLConnect

library(XLConnect)
library(tidyverse)

tidyCost <- function (province, type) {
  wb <- loadWorkbook("./burden_of_COPD/Burden_of_COPD_BC_ProvidenceAPR04.xlsx", create=F)
  excelName <- paste0(province, "_cost_", type)
  data <- readNamedRegion(wb, name = excelName)
  data$province <- province
  data$type <- type
  data %>% select (-total) %>% gather("genderage", "value", 2:9) %>% separate (genderage, into=c("gender", "age"))
}

#hosp
cost <- tidyCost("BC", "hosp")
cost <- rbind(cost, tidyCost("ON", "hosp"))
cost <- rbind(cost, tidyCost("AB", "hosp"))
cost <- rbind(cost, tidyCost("MB", "hosp"))
cost <- rbind(cost, tidyCost("NB", "hosp"))
cost <- rbind(cost, tidyCost("NL", "hosp"))
cost <- rbind(cost, tidyCost("NS", "hosp"))
cost <- rbind(cost, tidyCost("PE", "hosp"))
cost <- rbind(cost, tidyCost("QC", "hosp"))
cost <- rbind(cost, tidyCost("SK", "hosp"))

#MSP
cost <- rbind(cost, tidyCost("BC", "MSP"))
cost <- rbind(cost, tidyCost("ON", "MSP"))
cost <- rbind(cost, tidyCost("AB", "MSP"))
cost <- rbind(cost, tidyCost("MB", "MSP"))
cost <- rbind(cost, tidyCost("NB", "MSP"))
cost <- rbind(cost, tidyCost("NL", "MSP"))
cost <- rbind(cost, tidyCost("NS", "MSP"))
cost <- rbind(cost, tidyCost("PE", "MSP"))
cost <- rbind(cost, tidyCost("QC", "MSP"))
cost <- rbind(cost, tidyCost("SK", "MSP"))

#Pharm
cost <- rbind(cost, tidyCost("BC", "pharm"))
cost <- rbind(cost, tidyCost("ON", "pharm"))
cost <- rbind(cost, tidyCost("AB", "pharm"))
cost <- rbind(cost, tidyCost("MB", "pharm"))
cost <- rbind(cost, tidyCost("NB", "pharm"))
cost <- rbind(cost, tidyCost("NL", "pharm"))
cost <- rbind(cost, tidyCost("NS", "pharm"))
cost <- rbind(cost, tidyCost("PE", "pharm"))
cost <- rbind(cost, tidyCost("QC", "pharm"))
cost <- rbind(cost, tidyCost("SK", "pharm"))