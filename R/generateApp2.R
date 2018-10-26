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
choices_cost <- list("Total" = "sum",
                     "Inpatient" = "hosp",
                     "Outpatient" = "MSP",
                     "Pharma" = "pharm")

tab1 <- c("plotlyOutput", "download")
tab4 <- c("selectInput", "leafletOutput", "sliderInput")
tab3 <- c("selectInput", "plotlyOutput", "download")
tab2 <- c("leafletOutput", "sliderInput")
tab6 <- c("markdown")
tab5 <- c("markdown", "image")
tab4input <- c("costTypeMap", "sliderYear")
tab2input <- c()
tab1input <- c("plot_n_COPD")
tab3input <- c()
tab5input <- c()
tab6input <- c()
tab4id <- list("label" = c("costTypeMap", "map", "sliderYear"),
               "title" = c("Cost Map", "", "Year"),
               "choices" = list(choices_cost),
               "selected" = c("sum"),
               "sliderSettings" = list("min"=2015,
                                       "max"=2030,
                                       "value"=2015,
                                       "step"=NULL,
                                       "round"=FALSE,
                                       "ticks"=TRUE,
                                       "sep"="",
                                       "animate" = animationOptions(interval = 300,
                                                                    loop = FALSE)),
               "functions"=c("getMapData"))
tab1id <- list("label" = c("plot_n_COPD", "download_plot_n"),
               "title" = c("", "Download Plot"),
               "png_name"="COPD_Projected_Prevalence_",
               "functions"=c("n_copd_plot"))
tab3id <- list("label" = c("costType", "plot_cost", "download_plot_cost", "map"),
               "title" = c("Cost Type", "", "Download Plot"),
               "choices" = list(choices_cost),
               "selected" = c("sum"),
               "png_name"="COPD_Projected_cost_",
               "functions"=c("cost_plot"))
tab2id <- list("label" = c("map2", "sliderYear2"),
               "title" = c("Case Map", "", "Year"),
               "choices" = list(choices_cost),
               "selected" = c("sum"),
               "sliderSettings" = list("min"=2015,
                                       "max"=2030,
                                       "value"=2015,
                                       "step"=NULL,
                                       "round"=FALSE,
                                       "ticks"=TRUE,
                                       "sep"="",
                                       "animate" = animationOptions(interval = 300,
                                                                    loop = FALSE)),
               "functions"=c("getMapData"))
tab6id <- list("markdownFile"="disclaimer.rmd")
tab5id <- list("markdownFile"="about.Rmd","imFile"="logos2.png", "label"=c("","logos"))

metaData = new("MetaData")
metaData@app_title = "Burden of COPD in Canada"
metaData@tabs = 6
metaData@tab_titles <- c("Map", "Number of Cases", "Cost", "About", "Terms")
metaData@sidebar = 3
metaData@sidebar_titles = c("Gender", "Age Group","Province") 
metaData@sidebar_labels = c("Gender", "AgeGroup","Provinces") 
metaData@sidebar_choices_long = list(choices_gender, choices_age, choices_prov)
metaData@sidebar_choices_short = list(list("all" = "All","select" = "Select"),
                                      list("all" = "All","select" = "Select"),
                                      list("all" = "All","select" = "Select"))
metaData@sidebar_skip = c(1)
metaData@tab_inout = list(tab1, tab2, tab3, tab4, tab5, tab6)
metaData@tab_settings = list(tab1id, tab2id, tab3id, tab4id, tab5id, tab6id)
metaData@tab_input = list(tab1input, tab2input, tab3input, tab4input, tab5input, tab6input)
save(metaData, file="data/metaData.RData")
