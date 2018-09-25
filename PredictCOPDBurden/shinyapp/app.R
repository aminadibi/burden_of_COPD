#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(scales)
library(readr)
library(rmarkdown) #for markdown file
library(knitr) #for markdown file
library(htmltools)
library(maps) # interactive map
library(mapproj)
library(leaflet)
#source("R/map_plots.R")
#source("R/Cost.R")
#source("R/Census.R")
#source("R/Map.R")
#source("R/initialize.R")
#source("R/settings.R")

# Left Sidebar

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
     "Canada" = "Canada")
choices_year <- list(
     "2020" = "2020",
     "2025" = "2025",
     "2030" = "2030",
     "all" = "all years")
choices_cost <- list("Total" = "sum",
                     "Inpatient" = "hosp",
                     "Outpatient" = "MSP",
                     "Pharma" = "pharm")
ids <- c("showGender", "showAgeGroup", "showProvinces", "showYear")
rids <- c("radioGender", "radioAgeGroup", "radioProvinces", "radioYear")
tab_titles <- c("Number of Cases", "Cost", "Map", "Terms", "About")
num_inputs <- length(ids)
initialize = TRUE

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("simplex"),
  shinyjs::useShinyjs(),

   # Application title
   titlePanel("Burden of COPD in Canada"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      # SideBar
      sidebarPanel(
        # Gender
        radioButtons("radioGender", "Gender", c("all" = "All","select" = "Select")),

        shinyjs::hidden(div(id = ids[1],
          checkboxGroupInput("gender", label = NA,choices = choices_gender,selected = choices_gender$all))),

        # Age Group
        radioButtons("radioAgeGroup", "Age Group", c("all" = "All","select" = "Select")),

        shinyjs::hidden(div(id = ids[2],
          checkboxGroupInput("ageGroup", NA, choices = choices_age, selected = choices_age$all))),

        # Provinces
        radioButtons("radioProvinces", "Province",c("overall Canada" = "All", "select" = "Select")),

        shinyjs::hidden(div(id = ids[3],
          checkboxGroupInput("province", NA,
                            choices = choices_prov,
                            selected = choices_prov$Canada))),
        # Year
        radioButtons("radioYear", "Year",c("all" = "All","select" = "Select")),

        shinyjs::hidden(div(id = ids[4],
          checkboxGroupInput("year", label = NA, choices = choices_year, selected = choices_year$all)))),
      # Center
      mainPanel(

        tabsetPanel(
          id="selectedTab",
          type="tabs",
          tabPanel(tab_titles[3],
                   selectInput("costTypeMap",
                               h5("Cost Type"),
                               choices = choices_cost,
                               selected = "sum"),
                   leafletOutput("map"),br(),
                   sliderInput(inputId="sliderYear", label="Year",
                               min=2015, max=2030, value=2015, step = NULL, round = FALSE,
                               ticks = TRUE, animate = animationOptions(interval = 300, loop = FALSE),
                               sep="")),
          tabPanel(tab_titles[1],plotlyOutput("plot_n_COPD"),br(),#tableOutput("table_n_COPD"),
                    br(), br(),div(id = "SaveLoad",downloadButton("download_plot_n", "Download Plot"))),

          tabPanel(tab_titles[2], selectInput("costType",
                                        h5("Cost Type"),
                                        choices = choices_cost,
                                        selected = "sum"),
                                        plotlyOutput("plot_cost"),
                                        br(),br(), br(),
                                        div(id = "SaveLoad",
                                            downloadButton("download_plot_cost", "Download Plot"))),

          tabPanel(tab_titles[4],  includeMarkdown("../data/disclaimer.rmd")),
          tabPanel(tab_titles[5],  includeMarkdown("../data/about.Rmd"), imageOutput("logos"))


        )
     )

))

server <- function(input, output, session) {
   cost_data <- new("costData")
   cost_data <- readRDS(cost_data, "../data/cost.rds")
   cost <- cost_data@data
   copdNumber <- read_rds("../data/copdNumber.rds")
   buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
   dataList <- list("Cost"=cost, "copdNumber"=copdNumber)
   canMap <- new("canadaMap", filename=mapSettings$filename, initialize=FALSE)

   observe({
     inputRadio <- c(input$radioGender, input$radioAgeGroup, input$radioProvinces, input$radioYear)
     for(i in 1:num_inputs){
     if (inputRadio[i] == "Select") {
       shinyjs::show (id = ids[i], anim = TRUE)
       }
       else {shinyjs::hide (id = ids[i], anim = TRUE)
       }

     }})

   output$download_plot_n = downloadHandler(
     filename = function() {
       paste("COPD_Projected_Prevalence_", Sys.Date(), ".png", sep="")
     },    content = function(file) {
       ggsave(file, device = "png", width=11, height=8.5)

     }
   )

   output$download_plot_cost = downloadHandler(
     filename = function() {
       paste("COPD_Projected_cost_", Sys.Date(), ".png", sep="")
     },    content = function(file) {
       ggsave(file, device = "png", width=11, height=8.5)

     }
   )

  output$plot_cost <- renderPlotly({
    cost_plot()
  })

  cost_plot <- reactive ({

    if (input$radioGender == "All") {
      genderCheck <- "all genders"
    } else {
      genderCheck <- input$gender
    }


    if (input$radioAgeGroup == "All") {
      ageGroupCheck <- "all ages"
    } else {
      ageGroupCheck <- input$ageGroup
    }

    if (input$radioProvinces == "All") {
      provinceCheck <- "Canada"
    } else {
      provinceCheck <- input$province
    }
   cost$Legend <- interaction(cost$province, cost$gender, cost$age, sep=" ")
   p <- ggplot(subset (cost, ((gender %in% genderCheck) & (age %in% ageGroupCheck) & (province %in% provinceCheck) & (type %in% input$costType))), aes(x = Year, y=value/1000000, fill = Legend)) +
        geom_bar(stat = "identity", position = "dodge")  + labs(x="Year", y="") + scale_y_continuous(label=scales::dollar_format(suffix = "M")) + theme_bw()

   ggplotly (p) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))

  })

   output$plot_n_COPD <- renderPlotly({
     n_copd_plot()
   })

  n_copd_plot <- reactive ({
    if (input$radioGender == "All") {
      genderCheck <- "all genders"
    } else {
      genderCheck <- input$gender
    }

    if (input$radioAgeGroup == "All") {
      ageGroupCheck <- "all ages"
    } else {
      ageGroupCheck <- input$ageGroup
    }

    if (input$radioProvinces == "All") {
      provinceCheck <- "Canada"
    } else {
      provinceCheck <- input$province
    }
    copdNumber$Legend <- interaction(copdNumber$province, copdNumber$gender, copdNumber$age, sep=" ")
    p <- ggplot(subset (copdNumber, ((gender %in% genderCheck) & (age %in% ageGroupCheck) & (province %in% provinceCheck))), aes(x = Year, y=value, color = Legend)) +
         geom_point() + geom_line() + theme_bw() + labs(x="Year", y="") +
      scale_y_continuous("\n", labels = comma)

    ggplotly (p) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))

  })

  output$logos <- renderImage({
    width  <- session$clientData$output_logos_width
    height <- session$clientData$output_logos_height
    # Return a list containing the filename
    list(src = "./logos2.png",
         contentType = 'image/png',
         width = width,
         alt = "This is alternate text")
  }, deleteFile = FALSE)


  getMapData <- reactive({
    if (input$radioGender == "All") {
      genderCheck <- "all genders"
    } else {
      genderCheck <- input$gender
    }

    if (input$radioAgeGroup == "All") {
      ageGroupCheck <- "all ages"
    } else {
      ageGroupCheck <- input$ageGroup
    }
    if (input$radioYear == "All") {
      yearCheck <- seq(from=min(cost$Year), to=max(cost$Year), by=1)
    } else {
      yearCheck <- as.numeric(input$year)
    }
    yearCheck <- input$sliderYear

    mapDataList <- mapSettings$mapDataList
    for(i in 1:mapSettings$layers){
      data <- dataList[[i]]
      mapData <- new("mapData", canMap=canMap,digits=mapSettings$digits[i],
                      group=mapSettings$groups[i],
                     plotLabel=mapSettings$plotLabels[i], palette=mapSettings$palette[i])
      costAll  <- subset(data, ((gender %in% genderCheck) & (age %in% ageGroupCheck) &(province!="Canada")))
      costYear  <- subset(data, ((gender %in% genderCheck) & (age %in% ageGroupCheck) & (Year %in% yearCheck)))
      if("type" %in% colnames(data)){
        mapData@costAll <- subset(costAll, ((type %in% input$costTypeMap)))
        mapData@costYear <- subset(costYear, ((type %in% input$costTypeMap)))
      } else {
        mapData@costAll <- costAll
        mapData@costYear <- costYear
      }
      mapData <- getCostDensity(mapData)
      mapDataList[[i]] <- mapData

    }
    return(mapDataList)
    })

  output$map <- renderLeaflet({
      mapDataList <- getMapData()
      map <- new("createMap", layers=mapSettings$layers,
                 groups = mapSettings$groups, mapDataList=mapDataList)
      map <- drawMap(map)
      map
  })

}

# Run the application
shinyApp(ui = ui, server = server)

