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
library(XLConnect)
library(directlabels)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Burden of COPD"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
#        selectInput("selectPlot", h4("Select Plot"), 
#                    choices = list("Prevalence" = "prev", 
#                                   "Cost" = "cost"), 
#                                   selected = NA),
        
        checkboxGroupInput("gender", 
                           h4("Demographics"), 
                           choices = list("all" = "all", 
                                          "female" = "Female", 
                                          "male" = "Male"),
                           selected = "Female"
      ),
      checkboxGroupInput("ageGroup", 
                         h4("Age Group"), 
                         choices = list("35-54" = "35", 
                                        "55-64" = "55", 
                                        "65-74" = "65",
                                        "75 and older" = "75"),
                         selected = "65"
      ),
      
         checkboxGroupInput("province", 
                            h4("Provinces"), 
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
                                           "Canada - Overall" = "CA"),
                            selected = "BC")
      ),

      
      mainPanel(
        
        tabsetPanel(type="tabs",
                    tabPanel("Number of Cases",
                             plotlyOutput("plot_n_COPD"),
                             br(),
                             tableOutput("table_n_COPD")
                             
                    ),
                    
                    tabPanel("Cost",
                             selectInput("costType", h5("Cost Type"), 
                                         choices = list("Total" = "total",
                                                        "Inpatient" = "hosp", 
                                                        "Outpatient" = "MSP",
                                                        "Pharma" = "pharm"), selected = "hosp"),
                             plotlyOutput("plot_cost"),
                             br(),
                             tableOutput("table_cost")
                             
                            )

                  )
     )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   cost <- read_rds("../cost.rds")
   copdNumber <- read_rds("../copdNumber.rds")
  # wb <- loadWorkbook("./Burden_of_COPD_BC_ProvidenceAPR04.xlsx", create=F)
  # BC_cost_hosp <- readNamedRegion(wb, name = "BC_cost_hosp")
  # 
  
  output$plot_cost <- renderPlotly({
    print (cost_plot())
  })
  
  
  filterdata <- observe ({
    if ("male" %in% input$gender) {
       #filterCols <-  paste0("Male", input$ageGroup)
      dfCost <- filter(cost, gender == "Male")
    } else {
      
    }
       
    if ("female" %in% input$gender) {
         dfCost <- filter(cost, gender == "Female")
    }
    
    if ("35" %in% input$ageGroup) {
      cost <- filter(cost, age == "35")
    }
    
    if ("55" %in% input$ageGroup) {
      cost <- filter(cost, age == "55")
    }
 
    if ("75" %in% input$ageGroup) {
      cost <<- filter(cost, age == "75")
    }
    
    mdfCost <- reshape2::melt(cost, id.var = "Year")
    mdfNumber <- reshape2::melt(copdNumber, id.var = "Year")
    mdfCost <- as.data.frame(mdfCost)
    mdfNumber <- as.data.frame(mdfNumber)
    
    # variableList <- ""
    # mdf <- subset (mdf, variable =)
    
    
  })
  
  cost_plot <- reactive ({ 

   #p <- ggplot(cost, aes(x = Year)) #+ geom_line(aes(y = Male35), color="salmon") 
   p <- ggplot(subset (cost, ((gender %in% input$gender) & (age %in% input$ageGroup) & (province %in% input$province) & (type %in% input$costType))), aes(x = Year, y=value, color = province)) + geom_point() + geom_line() #+ geom_line(aes(y = Male35), color="salmon") 
   p <- p +  labs(x="year", y="COPD Cost") + theme_bw() 
      #direct.label(p, 'last.points')
      
   ggplotly (p) #%>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
  })
    
   output$plot_n_COPD <- renderPlotly({
     print (n_copd_plot())
   })
  
  n_copd_plot <- reactive ({ 
    p <- ggplot(subset (copdNumber, ((gender %in% input$gender) & (age %in% input$ageGroup) & (province %in% input$province))), aes(x = Year, y=value, color = province)) + geom_point() + geom_line() #+ geom_line(aes(y = Male35), color="salmon") 
    p <- p +  labs(x="year", y="COPD Cost") + theme_bw() 
    #direct.label(p, 'last.points')
    ggplotly (p) #%>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

