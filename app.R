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
library(shinydashboard2)
library(devtools)
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
source("./R/MapData.R")
source("./R/CreateMap.R")
source("./R/initialize.R")
source("./R/settings.R")
source("./R/MetaData.R")
source("./R/helper_functions.R")
source("./R/AppLayout.R")

load(file="./data/metaData.RData")
print(metaData)

# Left Sidebar

ids <- c("showGender", "showAgeGroup", "showProvinces")
rids <- c("radioGender", "radioAgeGroup", "radioProvinces")
choices_cost <- list("Total" = "sum",
                     "Inpatient" = "hosp",
                     "Outpatient" = "MSP",
                     "Pharma" = "pharm")
s_tabs = c(2,3)
# colors <- c("olive", "purple", "maroon", "aqua")
# colors <- c("ink", "posy", "embers", "black")

iconBox <- list(icon("user", lib="font-awesome"), icon("usd", lib="font-awesome"))
tab_titles = metaData@tab_titles
i=1
num_inputs <- length(ids)
appLayout <- AppLayout$new(6, "burdenOfCOPD")
initialize = TRUE
cat("~~~ Starting UI ~~~", fill=T)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin=appLayout$dashboardColour,
  
  # header
  dashboardHeader(title=metaData@app_title, titleWidth=320),
  # sidebar
  dashboardSidebar(
    sidebarMenu(id="selectedTab",
      menuItem("Cost", tabName = "costTab", icon = icon("dollar sign", lib="font-awesome"),
               menuSubItem("Map", tabName="costSubTabMap",icon=icon("globe americas", lib="font-awesome")),
               menuSubItem("Graph", tabName="costSubTabGraph", icon=icon("bar-chart", lib="font-awesome"))),
               
      menuItem("Prevalence", tabName = "casesTab", icon = icon("sort numeric up", lib="font-awesome"),
               menuSubItem("Map", tabName="casesSubTabMap",icon=icon("globe americas", lib="font-awesome")),
               menuSubItem("Graph", tabName="casesSubTabGraph",icon=icon("bar-chart", lib="font-awesome"))),
               

      menuItem("About", tabName = "aboutTab", icon = icon("address-book", lib="font-awesome")),
      menuItem("Terms", tabName = "termsTab", icon = icon("balance-scale", lib="font-awesome"))
    )
  ),
  # body
  dashboardBody(
    shinyjs::useShinyjs(),
    
    tabItems(
    

            # Tab tab1
            tabItem(tabName="casesSubTabMap",
                    # TabBox --> Box
                    box(solidHeader=FALSE, 
                        status="info",
                        # TabBox --> ValueBoxOutput
                        valueBoxOutput("box01", width=4),
                        valueBoxOutput("box02", width=8),
                        # AppBrick --> MapBrick
                        leafletOutput(outputId="map2",
                                                    width="100%"), 
                        # AppBrick --> SliderInput
                        sliderInput(inputId="sliderYear2",label="Year", 
                                    width="100%",
                                    min=2015,
                                    max=2030,
                                    value=2015,
                                    step=5,
                                    round=FALSE,
                                    ticks=TRUE,
                                    sep="",
                                    animate = animationOptions(interval = 300,
                                                               loop = FALSE)),
                        width=12, height=6)),
            # Tab tab2
            tabItem(tabName="costSubTabGraph",
                    
                    # TabBox --> Box
                    box(width=12,status="info",
                        # AppBrick --> SideBarLayout
                        sidebarLayout(
                          
                          sidebarPanel(
                            radioButtons(inputId=paste0("radio", metaData@sidebar_labels[1],"3"), 
                                         label=metaData@sidebar_titles[1],
                                         choices=metaData@sidebar_choices_short[[1]],
                                         selected=metaData@sidebar_choices_short[[1]]$all),
                            shinyjs::hidden(div(id=paste0(ids[1],"3"),
                                                checkboxGroupInput(paste0(metaData@sidebar_labels[1], "3"),
                                                                   label = NA,
                                                                   choices = metaData@sidebar_choices_long[[1]],
                                                                   selected = metaData@sidebar_choices_long[[1]]$all))),
                            radioButtons(inputId=paste0("radio", metaData@sidebar_labels[2],"3"), 
                                         label=metaData@sidebar_titles[2],
                                         choices=metaData@sidebar_choices_short[[2]],
                                         selected=metaData@sidebar_choices_short[[2]]$all),
                            shinyjs::hidden(div(id=paste0(ids[2],"3"),
                                                checkboxGroupInput(paste0(metaData@sidebar_labels[2], "3"),
                                                                   label = NA,
                                                                   choices = metaData@sidebar_choices_long[[2]],
                                                                   selected = metaData@sidebar_choices_long[[2]]$all))),
                            radioButtons(inputId=paste0("radio", metaData@sidebar_labels[3],"3"), 
                                         label=metaData@sidebar_titles[3],
                                         choices=metaData@sidebar_choices_short[[3]],
                                         selected=metaData@sidebar_choices_short[[3]]$all),
                            shinyjs::hidden(div(id=paste0(ids[3],"3"),
                                                checkboxGroupInput(paste0(metaData@sidebar_labels[3], "3"),
                                                                   label = NA,
                                                                   choices = metaData@sidebar_choices_long[[3]],
                                                                   selected = metaData@sidebar_choices_long[[3]]$all)))
                            
                          ),
                          mainPanel(
                            selectizeInput(inputId="costType",
                                           label="",
                                           options = list(style="z-index:100;"),
                                           choices = choices_cost,
                                           selected = c("sum")),
                            plotlyOutput(metaData@tab_settings[[1]]$label[2]),
                            
                            div(id = "SaveLoad",downloadButton(metaData@tab_settings[[1]]$label[3], 
                                                               metaData@tab_settings[[1]]$title[3]))))
                    )),
            # Tab
             tabItem(tabName="casesSubTabGraph",
                     # TabBox --> Box
                     box(width=12,status="info",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(inputId=paste0("radio", metaData@sidebar_labels[1],"2"), 
                                       label=metaData@sidebar_titles[1],
                                       choices=metaData@sidebar_choices_short[[1]],
                                       selected=metaData@sidebar_choices_short[[1]]$all),
                          shinyjs::hidden(div(id=paste0(ids[1],"2"),
                                          checkboxGroupInput(paste0(metaData@sidebar_labels[1], "2"),
                                                             label = NA,
                                                             choices = metaData@sidebar_choices_long[[1]],
                                                             selected = metaData@sidebar_choices_long[[1]]$all))),
                          radioButtons(inputId=paste0("radio", metaData@sidebar_labels[2],"2"), 
                                       label=metaData@sidebar_titles[2],
                                       choices=metaData@sidebar_choices_short[[2]],
                                       selected=metaData@sidebar_choices_short[[2]]$all),
                          shinyjs::hidden(div(id=paste0(ids[2],"2"),
                                          checkboxGroupInput(paste0(metaData@sidebar_labels[2], "2"),
                                                             label = NA,
                                                             choices = metaData@sidebar_choices_long[[2]],
                                                             selected = metaData@sidebar_choices_long[[2]]$all))),
                          radioButtons(inputId=paste0("radio", metaData@sidebar_labels[3],"2"), 
                                       label=metaData@sidebar_titles[3],
                                       choices=metaData@sidebar_choices_short[[3]],
                                       selected=metaData@sidebar_choices_short[[3]]$all),
                          shinyjs::hidden(div(id=paste0(ids[3],"2"),
                                          checkboxGroupInput(paste0(metaData@sidebar_labels[3], "2"),
                                                             label = NA,
                                                             choices = metaData@sidebar_choices_long[[3]],
                                                             selected = metaData@sidebar_choices_long[[3]]$all)))
                          
                        ),
                        mainPanel(
                          plotlyOutput(metaData@tab_settings[[3]]$label[1]),
                                  div(id = "SaveLoad",downloadButton(metaData@tab_settings[[3]]$label[2], 
                                                                     metaData@tab_settings[[3]]$title[2])))
                      ))),
          tabItem(tabName="costSubTabMap",
                  # TabBox --> ValueBoxOutput
                  valueBoxOutput("box1"),
                  valueBoxOutput("box2"),
                  valueBoxOutput("box3"),
                  valueBoxOutput("box4", width=4),
                  valueBoxOutput("box5", width=8),
            box(status="info",
                # AppBrick --> MapBrick
                leafletOutput(outputId="map",
                              width="100%"),
                # AppBrick --> SliderInput
                sliderInput(inputId="sliderYear",label="Year", 
                            width="100%",
                            min=2015,
                            max=2030,
                            value=2015,
                            step=5,
                            round=FALSE,
                            ticks=TRUE,
                            sep="",
                            animate = animationOptions(interval = 300,
                                                       loop = FALSE)),
                width=12, height=6)),
             tabItem(tabName="aboutTab",
                     # AppBrick --> IncludeMarkDown
                      includeMarkdown(paste0("./static_data/", metaData@tab_settings[[5]]$markdownFile)),
                     # AppBrick --> ImageOutput
                      imageOutput(metaData@tab_settings[[5]]$label[2])),
             tabItem(tabName="termsTab",
                      includeMarkdown(paste0("./static_data/", metaData@tab_settings[[6]]$markdownFile))))))
                      
                       

server <- function(input, output, session) {
  
  options(warn=-1)
  cat("~~~ Starting server ~~~", fill=T)
  cost_data <- new("costData")
  cost_data <- readRDS(cost_data, "./data/cost.rds")
  cost <- cost_data@data
  copdNumber <- read_rds("./data/copdNumber.rds")
  
  buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
  #dataList <- list("CostDensity"=cost,"Cost"=cost, "copdNumber"=copdNumber)
  canMap <- new("canadaMap", filename=mapSettings1$filename, initialize=FALSE)
  group_prev <<- "new"
  group_prev2 <<- "new"
  
  
  observe({
    print(input$selectedTab)
    if(!is.null(input$map_groups)){
      if(grepl("cost",input$selectedTab)){
        mapSettings = mapSettings1
      } else {mapSettings = mapSettings2}
      group = input$map_groups
      sout("test", group)
      sout(input$map_groups_baselayerchange)
      group = group[-which(group=="basemap")]
      print(length(group))
      if(length(group)==1){
        group_prev <<- group
        print(group_prev)
      }else{
        group = c(setdiff(group_prev, group), setdiff(group, group_prev))
        group_prev <<- group
      }
      g = which(mapSettings$groups!=group)
      
      proxy = leafletProxy("map")
      proxy %>% hideGroup(mapSettings$groups[g])
    }
    
    if(grepl("Graph",input$selectedTab)){
      
      z = which(c("casesSubTabGraph","costSubTabGraph")==input$selectedTab)+1
      print(z)
      inputRadio <- c(input[[paste0("radioGender",z)]], input[[paste0("radioAgeGroup",z)]],
                      input[[paste0("radioProvinces",z)]])
      for(i in 1:num_inputs){
        
        print("Printing input radio")
        print(inputRadio)
        print(ids[i])
        print(z)
        if(inputRadio[i]=="Select"){
          print(paste0(ids[i],z))
          shinyjs::show (id = paste0(ids[i],z), anim = TRUE)
        }
        else {
          
          shinyjs::hide (id = paste0(ids[i],z), anim = TRUE)
        }
        
      }}})
  
  lapply(1:metaData@tabs, function(i){
    settings = metaData@tab_settings[[i]]
    tab_inout = metaData@tab_inout[[i]]
    cat(paste0("Setting up tab components for tab:", i), fill=T)
    lapply(1:length(tab_inout), function(k){
      l=1
      if(tab_inout[k]=="plotlyOutput"){
        cat("~~~ Plotly Graph ~~~", fill=T)
        cat(settings$functions[l], fill=T)
        cat(settings$label[[k]], fill=T)
        output[[settings$label[k]]]<- renderPlotly({
          cat("~~~ Making Graph ~~~", fill=T)
          p <- reactive({do.call(settings$functions[l], args=list())})
          
          p()
        })
      } else if(tab_inout[k]=="download"){
        cat("~~~ Download ~~~", fill=T)
        cat(settings$label[[k]], fill=T)
        output[[settings$label[k]]] <- downloadHandler(
          filename = function(){
            paste(settings$png_name, Sys.Date(), ".png", sep="")},
          content = function(file) {
            ggsave(file, device="png", width=11, height=8.5)})
      } else if(tab_inout[k]=="image"){
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
        cat("~~~ Leaflet Map ~~~", fill=T)
        cat(settings$functions[l], fill=T)
        cat(settings$label[[k]])
        mapId = settings$label[[k]]
        if(mapId=="map2"){
          mapSettings = mapSettings2
          dataList <- list("copdNumber"=copdNumber)
        }else{
          mapSettings = mapSettings1
          dataList <- list("CostDensity"=cost,"Cost"=cost)
          
        }
        p <- reactive({do.call(settings$functions[l], args=list(mapSettings, dataList))})
        
        output[[mapId]] <- renderLeaflet({
          
          mapDataList <- p()
          map <- CreateMap$new(layers=mapSettings$layers,
                     groups = mapSettings$groups, legendLabels=mapSettings$legendLabels,
                     mapDataList=mapDataList)
          map$setupMap()
          mapRender <- map$drawMap()
          mapRender <- mapRender %>% htmlwidgets::onRender("function(el, x) {
                                               L.control.zoom({ position: 'topright' }).addTo(this)
        }") 
          mapRender
          
      })
        cat("~~~ Setting up Info Boxes ~~~", fill=T)
        cat(paste0("Number of Boxes = ", settings$numberOfBoxes), fill=T)
        mapShapeClick <- paste0(mapId, "_shape_click")
        changeLayer <- paste0(mapId, "_groups_baselayerchange")
        value <- reactiveValues(default = "Alberta", layer=1)
        observe({
          if(!is.null(input$map_groups)){
            if(grepl("cost",input$selectedTab)){
              mapSettings = mapSettings1
            } else {mapSettings = mapSettings2}
            group2 = input$map_groups
            group2 = group2[-which(group2=="basemap")]
            if(length(group2)!=1){
              group_prev2 <<- group2
            }else{
              group2 = c(setdiff(group_prev2, group2), setdiff(group2, group_prev2))
              group_prev2 <<- group2
            }
            sout("Group", group_prev2)
            g2 = which(mapSettings$groups!=group_prev2)
            value$layer <- g2
            sout("Layer: ",value$layer)
          }
          
          
        })
        lapply(1:settings$numberOfBoxes, function(box){
          boxId <- paste0(settings$boxLabel, box)
          
          observeEvent(input[[mapShapeClick]],{
            value$default <- input[[mapShapeClick]]$id
          })

 
         
        output[[boxId]] <- renderValueBox({
          if(value$default=="Alberta"){
            layer <- 1
            groupid <- "group11"
          } else {
              if(length(value$layer)!=1){
                layer <- 1
              } else{
                layer <- value$layer
              }
          province <- eventReactive(input[[mapShapeClick]], { # update the location selectInput on map clicks
            input[[mapShapeClick]]$id
          })
            groupid <- province()

            
              #layer <- as.numeric(substr(groupid, 6,6))
     
            
          }
          mapDataList <- p()
          map <- CreateMap$new(layers=mapSettings$layers,
                     groups = mapSettings$groups, legendLabels=mapSettings$legendLabels,
                     mapDataList=mapDataList)
          map$setupMap()
          cat("Creating map", fill=T)
          
          sout(groupid)
          
          prov <- as.numeric(substr(groupid,7,9))
          print(prov)
          mapLayer <- map$mapDataList[[layer]]
          print(mapLayer@costYear)
          totalBox = mapSettings$totalBox
          types = mapSettings$types
          colors = mapSettings$colors
          
          print(settings$treatmentType[box])
          print(settings$treatmentType)
          if(box==totalBox+1){
            subBox = totalBox
          } else {
            subBox = box
          }
          typeList <- map$costType(layer, settings$treatmentType[subBox], types, mapSettings$dense[layer])
          print(typeList)
          print(settings$treatmentType[box])
          
          if(box==totalBox){
            # subtitle = paste0(settings$treatmentTypeTitles[box], typeList$provinces[prov], 
            #                   settings$boxSuffix[layer])
            subtitle = paste0(settings$treatmentTypeTitles[box], settings$boxSuffix[layer])
          } else if(box==totalBox+1){
            subtitle = "Province"
            } else {
            subtitle = settings$treatmentTypeTitles[box]
          }
          if(typeList$labels[prov]==0 || typeList$labels[prov]=="No data"){
            value = "No data"
          } else if(box==totalBox+1){
            value = paste0(typeList$provinces[prov])
          } else {
            value = paste0(settings$boxPrefix, typeList$labels[prov])
          }

          valueBox(
            value = value,
            subtitle = subtitle,
            color = colors[box],
            icon = iconBox[[layer]]

          )
        })})
      } else if(tab_inout[k]=="infoBox"){
 
      }
      }
        )
      })

  
  cost_plot <- function(){
    
    cat("~~~ Making Cost Plot ~~~", fill=T)
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
  
  getMapData <- function(mapSettings, dataList){
    
    genderCheck <- "all genders"
    ageGroupCheck <- "all ages"
    yearCheck <- input$sliderYear
    noType <- TRUE
    
    mapDataList <- mapSettings$mapDataList
    for(i in 1:mapSettings$layers){
      data <- dataList[[i]]
      
      mapData <- new("mapData", canMap=canMap,digits=mapSettings$digits[i],
                     group=mapSettings$groups[i],prefix = mapSettings$prefix[i],
                     plotLabel=mapSettings$plotLabels[i], palette=mapSettings$palette[i])
      costAll  <- subset(data, ((gender %in% genderCheck) & (age %in% ageGroupCheck) &(province!="Canada")))
      costYear  <- subset(data, ((gender %in% genderCheck) & (age %in% ageGroupCheck) & (Year %in% yearCheck)))
      
      if("type" %in% colnames(data)){
        mapData@costAll <- subset(costAll, ((type %in% "sum")))
        mapData@costYear <- subset(costYear, ((type %in% "sum")))
        mapData@costYearNoType <- costYear
        mapData@types <- TRUE
        mapData@typesList <- c("sum", "hosp", "MSP", "pharm")
      } else {
        mapData@costAll <- costAll
        mapData@costYear <- costYear
      }
      
      mapData <- getCostDensity(mapData, mapSettings$dense[i])
      
      mapDataList[[i]] <- mapData
    }
    return(mapDataList)
  }
  
  
  
  }

# Run the application
shinyApp(ui = ui, server = server)

