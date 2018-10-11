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
#library(shinyjs)
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

ids <- c("showGender", "showAgeGroup", "showProvinces")
rids <- c("radioGender", "radioAgeGroup", "radioProvinces")
s_tabs = c(2,3)
tab_titles = metaData@tab_titles
i=1
num_inputs <- length(ids)
initialize = TRUE

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("simplex"),
  shinyjs::useShinyjs(),
  
  # Use the Google webfont "Source Sans Pro"
  tags$link(
    href=paste0("http://fonts.googleapis.com/css?",
                "family=Source+Sans+Pro:300,600,300italic"),
    rel="stylesheet", type="text/css"),
  tags$style(type="text/css",
             "body {font-family: 'Source Sans Pro'}"
  ),

   # Application title
   titlePanel(metaData@app_title),
   do.call(tabsetPanel, c(id="selectedTab",type="tabs",
                         
                         lapply(1:metaData@tabs, function(i){
                           z = i
                           settings = metaData@tab_settings[[i]]
                           tab_inout = metaData@tab_inout[[i]]
                          
                           tabPanel(metaData@tab_titles[i],
                                    lapply(1:1, function(j){
                                      l=1
                                      if(i %in% s_tabs){
                                        do.call(sidebarLayout, list(
                                          # SideBar
                                          sidebarPanel(lapply(1:(metaData@sidebar*2), function(k){
                                            if(k%%2==0){
                                              f = shinyjs::hidden
                                              i = k/2
                                              arguments = list(div(id = paste0(ids[i],z),
                                                                   checkboxGroupInput(paste0(metaData@sidebar_labels[i], z),
                                                                                      label = NA,choices = metaData@sidebar_choices_long[[i]],
                                                                                      selected = metaData@sidebar_choices_long[[i]]$all)))
                                              
                                            } else{
                                              i=(k+1)/2
                                              f=radioButtons
                                              print(paste0("radio", metaData@sidebar_labels[i],z))
                                              arguments = list(inputId=paste0("radio", metaData@sidebar_labels[i],z), 
                                                               label=metaData@sidebar_titles[i],choices=metaData@sidebar_choices_short[[i]],
                                                               selected=metaData@sidebar_choices_short[[i]]$all)
                                              
                                            }
                                            do.call(f, arguments)
                                            
                                            
                                          })),
                                          
                                          
                                          
                                          # Center
                                          mainPanel(lapply(1:length(tab_inout),function(k){
                                            if(tab_inout[k]=="selectInput"){
                                              print(settings$choices[k])
                                              selectInput(settings$label[k],
                                                          h5(settings$title[k]),
                                                          choices = settings$choices[[l]],
                                                          selected = settings$selected[k])}
                                            else if(tab_inout[k]=="leafletOutput"){
                                              do.call(leafletOutput, list(outputId=settings$label[k]))
                                            }
                                            else if(tab_inout[k]=="plotlyOutput"){
                                              plotlyOutput(settings$label[k])
                                              
                                            }else if(tab_inout[k]=="download"){
                                              div(id = "SaveLoad",downloadButton(settings$label[k], settings$title[k]))
                                            }
                                          })
                                            
                                          )
                                          
                                        ))
                                        
                                      } else{
                                        lapply(1:length(tab_inout), function(k){
                                      
                                      if(tab_inout[k]=="selectInput"){
                                        print(settings$choices[k])
                                        selectInput(settings$label[k],
                                                    h5(settings$title[k]),
                                                    choices = settings$choices[[l]],
                                                    selected = settings$selected[k])}
                                      else if(tab_inout[k]=="leafletOutput"){
                                        do.call(leafletOutput, list(outputId=settings$label[k]))
                                      }
                                      else if(tab_inout[k]=="sliderInput"){
                                        sliderSettings = settings$sliderSettings
                                        sliderSettings[["inputId"]] = settings$label[k]
                                        sliderSettings[["label"]] = settings$title[k]
                                        do.call(sliderInput, sliderSettings)
                                      }
                                      else if(tab_inout[k]=="plotlyOutput"){
                                        plotlyOutput(settings$label[k])
                                        
                                      } else if(tab_inout[k]=="download"){
                                        div(id = "SaveLoad",downloadButton(settings$label[k], settings$title[k]))
                                      } else if(tab_inout[k]=="markdown"){
                                        includeMarkdown(paste0("./static_data/", settings$markdownFile))
                                        
                                      } else if(tab_inout[k]=="image"){
                                        imageOutput(settings$label[k])
                                      }})}
                                      
                                      
                                    }))
                         })))
)

server <- function(input, output, session) {
   cost_data <- new("costData")
   cost_data <- readRDS(cost_data, "./data/cost.rds")
   cost <- cost_data@data
   copdNumber <- read_rds("./data/copdNumber.rds")
   buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
   dataList <- list("Cost"=cost, "copdNumber"=copdNumber)
   canMap <- new("canadaMap", filename=mapSettings$filename, initialize=FALSE)
   observe({
     #inputRadio <- c(input$radioGender, input$radioAgeGroup, input$radioProvinces)
     print("Check")
     if(input$selectedTab %in% c("Number of Cases","Cost")){
       print(input$selectedTab)
       z = which(c("Number of Cases","Cost")==input$selectedTab)+1
       print(z)
       inputRadio <- c(input[[paste0("radioGender",z)]], input[[paste0("radioAgeGroup",z)]],
                       input[[paste0("radioProvinces",z)]])
     for(i in 1:num_inputs){
 
         print("Printing input radio")
         print(inputRadio)
       if(inputRadio[i]=="Select"){
       
       shinyjs::show (id = paste0(ids[i],z), anim = TRUE)
       }
       else {shinyjs::hide (id = paste0(ids[i],z), anim = TRUE)
       }

     }}})
    print("still working")

  lapply(1:metaData@tabs, function(i){
    settings = metaData@tab_settings[[i]]
    tab_inout = metaData@tab_inout[[i]]
    lapply(1:length(tab_inout), function(k){
      l=1
      if(tab_inout[k]=="plotlyOutput"){
        output[[settings$label[k]]]<- renderPlotly({
          #inputRadio <- c(input$radioGender, input$radioAgeGroup, input$radioProvinces)
          p <- reactive({do.call(settings$functions[l], args=list())})
          
          p()
        })
      } else if(tab_inout[k]=="download"){
        output[[settings$label[k]]] <- downloadHandler(
          filename = function(){
            paste(settings$png_name, Sys.Date(), ".png", sep="")},
          content = function(file) {
            ggsave(file, device="png", width=11, height=8.5)})
      } else if(tab_inout[k]=="image"){
        print(settings$imFile)
        output[[settings$label[k]]] <- renderImage({
          width  <- session$clientData$output_logos_width
          height <- session$clientData$output_logos_height
          # Return a list containing the filename
          list(src = paste0("./static_data/",settings$imFile),
               contentType = 'image/png',
               width = width,
               alt = "Logos")
        }, deleteFile = FALSE)
      } else if(tab_inout[k]=="leafletOutput"){
        print("Working...")
        output[[settings$label[k]]] <- renderLeaflet({
          p <- reactive({do.call(settings$functions[l], args=list())})
          mapDataList <- p()
          print("Good still")
          map <- new("createMap", layers=mapSettings$layers,
                     groups = mapSettings$groups, mapDataList=mapDataList)
          map <- drawMap(map)
          print("Still okay")
          map
        })
      }
          }
        )
      })
  
  cost_plot <- function(){

    if (input$radioGender3 == "All") {
      genderCheck <- "all genders"
    } else {
      genderCheck <- input$Gender3
    }


    if (input$radioAgeGroup3 == "All") {
      ageGroupCheck <- "all ages"
    } else {
      ageGroupCheck <- input$AgeGroup3
    }

    if (input$radioProvinces3 == "All") {
      provinceCheck <- "Canada"
    } else {
      provinceCheck <- input$Provinces3
    }
    print(input$radioProvinces3)
   cost$Legend <- interaction(cost$province, cost$gender, cost$age, sep=" ")
   p <- ggplot(subset (cost, ((gender %in% genderCheck) & (age %in% ageGroupCheck) & (province %in% provinceCheck) & (type %in% input$costType))), aes(x = Year, y=value/1000000, fill = Legend)) +
        geom_bar(stat = "identity", position = "dodge")  + labs(x="Year", y="") + scale_y_continuous(label=scales::dollar_format(suffix = "M")) + theme_bw()
   print(class(p))
   ggplotly (p) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F,
                           modeBarButtonsToRemove=buttonremove) %>%
     layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))

  }

  n_copd_plot <- function(){
    if (input$radioGender2 == "All") {
      genderCheck <- "all genders"
    } else {
      genderCheck <- input$Gender2
    }

    if (input$radioAgeGroup2 == "All") {
      ageGroupCheck <- "all ages"
    } else {
      ageGroupCheck <- input$AgeGroup2
    }

    if (input$radioProvinces2 == "All") {
      provinceCheck <- "Canada"
    } else {
      provinceCheck <- input$Provinces2
    }
    copdNumber$Legend <- interaction(copdNumber$province, copdNumber$gender, copdNumber$age, sep=" ")
    p <- ggplot(subset (copdNumber, ((gender %in% genderCheck) & (age %in% ageGroupCheck) & (province %in% provinceCheck))), aes(x = Year, y=value, color = Legend)) +
         geom_point() + geom_line() + theme_bw() + labs(x="Year", y="") +
      scale_y_continuous("\n", labels = comma)

    ggplotly (p) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))

  }

  getMapData <- function(){

    genderCheck <- "all genders"
    ageGroupCheck <- "all ages"
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
      print("Working in getmapdata")

    }
    return(mapDataList)
    }



}

# Run the application
shinyApp(ui = ui, server = server)

