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

#BC_cost_hosp
cost <- tidyCost("BC", "hosp")
cost <- rbind(cost, tidyCost("ON", "hosp"))