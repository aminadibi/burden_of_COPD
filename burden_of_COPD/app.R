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
source("map_plots.R")


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
      
        shinyjs::hidden(div(id = "showGender",                 
          checkboxGroupInput("gender", label = NA,choices = list("female" = "Female", 
            "male" = "Male","all" = "all genders"),selected = c("all genders")))),
        
        # Age Group
        radioButtons("radioAgeGroup", "Age Group", c("all" = "all ages","select" = "Select")),
      
        shinyjs::hidden(div(id = "showAgeGroup", 
          checkboxGroupInput("ageGroup", NA, choices = list("35-54" = "35", 
                                                            "55-64" = "55", 
                                                            "65-74" = "65",
                                                            "75 and older" = "75",
                                                            "all" = "all ages"),
            selected = "all ages"))),
        
        # Provinces
        radioButtons("radioProvinces", "Province",c("overall Canada" = "Canada", "select" = "Select")),
                       
        shinyjs::hidden(div(id = "showProvinces", 
          checkboxGroupInput("province", NA, 
                            choices = list("Alberta" = "AB", 
                                           "British Columbia" = "BC", 
                                           "Manitoba" = "MB",
                                           "New Brunswick" = "NB",
                                           "Newfoundland and Labrador" = "NL",
                                           "Nova Scotia" = "NS",
                                           "Ontario" = "ON",
                                           "Prince Edward Island" = "PE",
                                           "Quebec" = "QC", 
                                           "Saskatchewan" = "SK",
                                           "Canada" = "Canada"),
                            selected = "Canada"))),
        # Year
        radioButtons("radioYear", "Year",c("all" = "all years","select" = "Select")),

        shinyjs::hidden(div(id = "showYear",
          checkboxGroupInput("year", label = NA, choices = list("2020" = "2020",
                                                                "2025" = "2025",
                                                                "2030" = "2030",
                                                                "all" = "all years"),
            selected = "all years")))),
      # Center 
      mainPanel(
        
        tabsetPanel(
          id="selectedTab", 
          type="tabs", 
          tabPanel("Number of Cases",plotlyOutput("plot_n_COPD"),br(),#tableOutput("table_n_COPD"), 
                    br(), br(),div(id = "SaveLoad",downloadButton("download_plot_n", "Download Plot"))),
                    
          tabPanel("Cost", selectInput("costType", 
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
          tabPanel("Terms",  includeMarkdown("./disclaimer.rmd")),
          tabPanel("About",  includeMarkdown("./about.Rmd"), imageOutput("logos")),
          tabPanel("Map", 
                   selectInput("costTypeMap", 
                                h5("Cost Type"), 
                                choices = list("Total" = "sum",
                                                "Inpatient" = "hosp", 
                                                "Outpatient" = "MSP",
                                                "Pharma" = "pharm"), 
                                selected = "sum"),
                   plotOutput("map"),
                   sliderInput(inputId="sliderYear", label="Year", 
                               min=2015, max=2030, value=2015, step = NULL, round = FALSE, 
                               ticks = TRUE, animate = FALSE, sep=""))

        )
     )
  
))

server <- function(input, output, session) {
   cost <- read_rds("./cost.rds")
   copdNumber <- read_rds("./copdNumber.rds")
   buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
   mapFrame <- getMap()
   
   observe({
     if (input$radioGender == "Select") {
       shinyjs::show (id = "showGender", anim = TRUE)
       }
       else {shinyjs::hide (id = "showGender", anim = TRUE)
       }
     
     if (input$radioAgeGroup == "Select") {
       shinyjs::show (id = "showAgeGroup", anim = TRUE)
     }
     else shinyjs::hide (id = "showAgeGroup", anim = TRUE)
     
     if (input$radioProvinces == "Select") {
       shinyjs::show (id = "showProvinces", anim = TRUE)
     }
     else shinyjs::hide (id = "showProvinces", anim = TRUE)
     
     if (input$radioYear == "Select") {
       shinyjs::show (id = "showYear", anim = TRUE)
     }
     else shinyjs::hide (id = "showYear", anim = TRUE)
     
 
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
         geom_point() + geom_line() + theme_bw() + labs(x="Year", y="") +  scale_y_continuous("\n", labels = comma)



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
    dollarRange <- c(min(dollar$value), max(dollar$value))
    data  <- subset(cost, ((gender %in% genderCheck) & (age %in% ageGroupCheck) & (Year %in% yearCheck)
                           &(type %in% input$costTypeMap)))
    print(cost$Year)
    print(dollar$value)
    print(dollar[168,])
    print(genderCheck)
    print(ageGroupCheck)
    print(input$radioYear)
    print(yearCheck)
    print(max(cost$Year))
    print(min(cost$Year))
    print(data$Year)
    print(input$costTypeMap)
    print(dollarRange)
    drawMap(data, dollarRange, mapFrame[[1]], mapFrame[[2]])
    })
  
  output$map <- renderPlot({
    map_plot()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

