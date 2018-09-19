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
source("map_plots.R")
source("Cost.R")
source("Census.R")
source("Map.R")

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
ids <- c("showGender", "showAgeGroup", "showProvinces", "showYear")
rids <- c("radioGender", "radioAgeGroup", "radioProvinces", "radioYear")
tab_titles <- c("Number of Cases", "Cost", "Map", "Terms", "About")
num_inputs <- length(ids)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
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
        radioButtons("radioAgeGroup", "Age Group", c("all" = "all ages","select" = "Select")),
      
        shinyjs::hidden(div(id = ids[2], 
          checkboxGroupInput("ageGroup", NA, choices = choices_age, selected = choices_age$all))),
        
        # Provinces
        radioButtons("radioProvinces", "Province",c("overall Canada" = "Canada", "select" = "Select")),
                       
        shinyjs::hidden(div(id = ids[3], 
          checkboxGroupInput("province", NA, 
                            choices = choices_prov,
                            selected = choices_prov$Canada))),
        # Year
        radioButtons("radioYear", "Year",c("all" = "all years","select" = "Select")),

        shinyjs::hidden(div(id = ids[4],
          checkboxGroupInput("year", label = NA, choices = choices_year, selected = choices_year$all)))),
      # Center 
      mainPanel(
        
        tabsetPanel(
          id="selectedTab", 
          type="tabs", 
          tabPanel(tab_titles[1],plotlyOutput("plot_n_COPD"),br(),#tableOutput("table_n_COPD"), 
                    br(), br(),div(id = "SaveLoad",downloadButton("download_plot_n", "Download Plot"))),
                    
          tabPanel(tab_titles[2], selectInput("costType", 
                                        h5("Cost Type"), 
                                        choices = list("Total" = "sum",
                                                        "Inpatient" = "hosp", 
                                                        "Outpatient" = "MSP",
                                                        "Pharma" = "pharm"), 
                                        selected = "sum"),
                                        plotlyOutput("plot_cost"),
                                        br(),
                                        #tableOutput("table_cost"), 
                                        br(), br(),
                                        div(id = "SaveLoad",
                                            downloadButton("download_plot_cost", "Download Plot"))),
          tabPanel(tab_titles[3], 
                   selectInput("costTypeMap", 
                               h5("Cost Type"), 
                               choices = list("Total" = "sum",
                                              "Inpatient" = "hosp", 
                                              "Outpatient" = "MSP",
                                              "Pharma" = "pharm"), 
                               selected = "sum"),
                   leafletOutput("map"),
                   sliderInput(inputId="sliderYear", label="Year", 
                               min=2015, max=2030, value=2015, step = NULL, round = FALSE, 
                               ticks = TRUE, animate = FALSE, sep="")),
          tabPanel(tab_titles[4],  includeMarkdown("./disclaimer.rmd")),
          tabPanel(tab_titles[5],  includeMarkdown("./about.Rmd"), imageOutput("logos"))


        )
     )
  
))

server <- function(input, output, session) {
   cost_data <- costData()
   cost_data <- readRDS(cost_data, "./cost.rds")
   cost <- cost_data@data
   copdNumber <- read_rds("./copdNumber.rds")
   buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
   map <- mapData()
   map <- getMap(map)
   
   observe({
     for(i in 1:num_inputs){
     if (input$radioGender == "Select") {
       shinyjs::show (id = ids[i], anim = TRUE)
       }
       else {shinyjs::hide (id = ids[i], anim = TRUE)
       }
     
     if (input$radioAgeGroup == "Select") {
       shinyjs::show (id = ids[2], anim = TRUE)
     }
     else shinyjs::hide (id = ids[2], anim = TRUE)

     if (input$radioProvinces == "Select") {
       shinyjs::show (id = ids[3], anim = TRUE)
     }
     else shinyjs::hide (id = ids[3], anim = TRUE)

     if (input$radioYear == "Select") {
       shinyjs::show (id = ids[4], anim = TRUE)
     }
     else shinyjs::hide (id = ids[4], anim = TRUE)
      }
 
     })  
   
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

    if (input$radioAgeGroup == "all ages") {
      ageGroupCheck <- "all ages"
    } else {
      ageGroupCheck <- input$ageGroup
    }
    
    if (input$radioProvinces == "Canada") {
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
    
    if (input$radioAgeGroup == "all ages") {
      ageGroupCheck <- "all ages"
    } else {
      ageGroupCheck <- input$ageGroup
    }
    
    if (input$radioProvinces == "Canada") {
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
  

  map_plot <- reactive({
    if (input$radioGender == "All") {
      genderCheck <- "all genders"
    } else {
      genderCheck <- input$gender
    }
    
    if (input$radioAgeGroup == "all ages") {
      ageGroupCheck <- "all ages"
    } else {
      ageGroupCheck <- input$ageGroup
    }
    if (input$radioYear == "all years") {
      yearCheck <- seq(from=min(cost$Year), to=max(cost$Year), by=1)
    } else {
      yearCheck <- as.numeric(input$year)
    }
    yearCheck <- input$sliderYear
    
    dollar  <- subset(cost, ((gender %in% genderCheck) & (age %in% ageGroupCheck) 
                           &(type %in% input$costTypeMap) &(province!="Canada")))
    data  <- subset(cost, ((gender %in% genderCheck) & (age %in% ageGroupCheck) & (Year %in% yearCheck)
                           &(type %in% input$costTypeMap)))

    map@costYear <- data
    map@costAll <- dollar
    map <- getCostDensity(map)
    return(map)
    })
  
  output$map <- renderLeaflet({
    map <- map_plot()
    map <- drawMap2(map)
    map
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

