library(XLConnect)
library(tidyverse)

tidyCost <- function (excelName, province) {
  wb <- loadWorkbook("./burden_of_COPD/Burden_of_COPD_BC_ProvidenceAPR04.xlsx", create=F)
  data <- readNamedRegion(wb, name = "excelName")
  data$province <- province
  data %>% select (-total) %>% gather("genderage", "value", 2:9) %>% separate (genderage, into=c("gender", "age"))
}

wb <- loadWorkbook("./burden_of_COPD/Burden_of_COPD_BC_ProvidenceAPR04.xlsx", create=F)
BC_cost_hosp <- readNamedRegion(wb, name = "BC_cost_hosp")

data <- BC_cost_hosp

data$province <- 'bc'


data %>% select (-total) %>% gather("genderage", "value", 2:9) %>% separate (genderage, into=c("gender", "age"))