source("R/MetaData.R")
choices_gender <- list("female" = "Female",
                       "male" = "Male",
                       "all" = "all genders")
choices_age <- list("35-54" = "35",
                    "55-64" = "55",
                    "65-74" = "65",
                    "75 and older" = "75",
                    "all" = "all ages")
choices_prov <- list(
  "Alberta" = "AB",
  "British Columbia" = "BC",
  "Manitoba" = "MB",
  "New Brunswick" = "NB",
  "Newfoundland and Labrador" = "NL",
  "Nova Scotia" = "NS",
  "Ontario" = "ON",
  "Prince Edward Island" = "PE",
  "Quebec" = "QC",
  "Saskatchewan" = "SK",
  "all" = "Canada")
choices_year <- list(
  "2020" = "2020",
  "2025" = "2025",
  "2030" = "2030",
  "all" = "all years")
choices_cost <- list("Total" = "sum",
                     "Inpatient" = "hosp",
                     "Outpatient" = "MSP",
                     "Pharma" = "pharm")
tab1 <- c("selectInput", "leafletOutput", "sliderInput")
tab2 <- c("plotlyOutput", "download")
tab3 <- c("selectInput", "plotlyOutput", "download")
tab4 <- c("markdown")
tab5 <- c("markdown", "image")
tab1id <- list("label" = c("costTypeMap", "map", "sliderYear"),
               "title" = c("Cost Map", "", "Year"),
               "choices" = c(choices_cost),
               "selected" = c("sum"),
               "sliderSettings" = list("min"=2015,
                                    "max"=2030,
                                    "value"=2015,
                                    "step"=NULL,
                                    "round"=FALSE,
                                    "ticks"=TRUE,
                                    "sep"="",
                                    "animate_interval"= 300,
                                    "animate_loop"= FALSE))
tab2id <- list("label" = c("plot_n_COPD", "download_plot_n"),
                   "title" = c("", "Download Plot"))
tab3id <- list("label" = c("costType", "plot_cost", "download_plot_cost"),
               "title" = c("Cost Type", "", "Download Plot"),
               "choices" = c(choices_cost),
               "selected" = c("sum"))
tab4id <- list("markdownFile"="disclaimer.rmd")
tab5id <- list("markdownFile"="about.Rmd","imFile"="logos")

metaData = new("MetaData")
metaData@app_title = "Burden of COPD in Canada"
metaData@tabs = 5
metaData@tab_titles <- c("Map", "Number of Cases", "Cost", "Terms", "About")
metaData@sidebar = 4
metaData@sidebar_titles = c("Gender", "Age Group","Province", "Year") 
metaData@sidebar_labels = c("Gender", "AgeGroup","Provinces", "Year") 
metaData@sidebar_choices_long = list(choices_gender, choices_age, choices_prov, choices_year)
metaData@sidebar_choices_short = list(list("all" = "All","select" = "Select"),
                                      list("all" = "All","select" = "Select"),
                                      list("all" = "All","select" = "Select"),
                                      list("all" = "All","select" = "Select"))
metaData@tab_inout = list(tab1, tab2, tab3, tab4, tab5)
metaData@tab_settings = list(tab1id, tab2id, tab3id, tab4id, tab5id)
save(metaData, file="data/metaData.RData")
