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

source("./R/Cost.R")
source("./R/Census.R")
source("./R/Map.R")
source("./R/initialize.R")
source("./R/settings.R")
source("./R/MetaData.R")
source("./R/helper_functions.R")

load(file="./data/metaData.RData")
print(metaData)

# Left Sidebar

ids <- c("showGender", "showAgeGroup", "showProvinces", "showYear")
rids <- c("radioGender", "radioAgeGroup", "radioProvinces", "radioYear")
tab_titles = metaData@tab_titles
num_inputs <- length(ids)
initialize = TRUE

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("simplex"),
  shinyjs::useShinyjs(),

   # Application title
   titlePanel(metaData@app_title),
   # Sidebar with a slider input for number of bins
   sidebarLayout(
      # SideBar
      sidebarPanel(
        lapply(1:metaData@sidebar, function(i){
          radioButtons(inputId=paste0("radio", metaData@sidebar_labels[i]), 
                       label=metaData@sidebar_titles[i],
                       choices=metaData@sidebar_choices_short[[i]],
                       selected=metaData@sidebar_choices_short[[i]]$all)}),
        lapply(1:metaData@sidebar, function(i){
          shinyjs::hidden(div(id = ids[i],
                              checkboxGroupInput(metaData@sidebar_labels[i], 
                                                 label = NA,
                                                 choices = metaData@sidebar_choices_long[[i]],
                                                 selected = metaData@sidebar_choices_long[[i]]$all)))})
        ),

      
      # Center
      mainPanel(
        
        do.call(tabsetPanel, c(id="selectedTab",type="tabs",

          lapply(1:metaData@tabs, function(i){
            settings = metaData@tab_settings[[i]]
            tab_inout = metaData@tab_inout[[i]]
            tabPanel(metaData@tab_titles[i],
                   lapply(1:length(tab_inout), function(k){

                     if(tab_inout[k]=="selectInput"){
                        selectInput(settings$label[k],
                                    h5(settings$title[k]),
                                    choices = settings$choices[k],
                                    selected = settings$selected[k])}
                     else if(tab_inout[k]=="leafletOutput"){
                       leafletOutput(settings$label[k])
                     }
                     else if(tab_inout[k]=="sliderInput"){
                       sliderInput(inputId=settings$label[k],
                                   label=settings$title[k],
                                   min=settings$sliderSettings$min,
                                   max=settings$sliderSettings$max,
                                   value=settings$sliderSettings$value,
                                   step = settings$sliderSettings$step,
                                   round = settings$sliderSettings$round,
                                   ticks = settings$sliderSettings$ticks,
                                   animate = animationOptions(
                                     interval=settings$sliderSettings$animate_interval,
                                     loop=settings$sliderSettings$animate_loop),
                                   sep=settings$sliderSettings$sep)
                     }
                     else if(tab_inout[k]=="plotlyOutput"){
                       plotlyOutput(settings$label[k])

                     } else if(tab_inout[k]=="download"){
                       div(id = "SaveLoad",downloadButton(settings$label[k], settings$title[k]))
                     } else if(tab_inout[k]=="markdown"){
                         includeMarkdown(paste0("./static_data/", settings$markdownFile))

                     } else if(tab_inout[k]=="image"){
                       imageOutput(settings$imFile)
                       }


}))
            }))))

))

server <- function(input, output, session) {
   cost_data <- new("costData")
   cost_data <- readRDS(cost_data, "./data/cost.rds")
   cost <- cost_data@data
   copdNumber <- read_rds("./data/copdNumber.rds")
   buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
   dataList <- list("Cost"=cost, "copdNumber"=copdNumber)
   canMap <- new("canadaMap", filename=mapSettings$filename, initialize=FALSE)
   observe({
     inputRadio <- c(input$radioGender, input$radioAgeGroup, input$radioProvinces, input$radioYear)
     print("Check")
     for(i in 1:num_inputs){
       print("Check")
       print(input$radioAgeGroup)
       print(input[['radioGender']])
         print(input$Gender)
   
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

