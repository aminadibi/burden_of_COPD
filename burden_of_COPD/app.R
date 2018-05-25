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
                                          "female" = "female", 
                                          "male" = "male"),
                           selected = NA
      ),
      checkboxGroupInput("ageGroup", 
                         h4("Age Group"), 
                         choices = list("35-54" = 3554, 
                                        "55-64" = 5564, 
                                        "65-74" = 6574,
                                        "75 and older" = 75),
                         selected = NA
      ),
      
         checkboxGroupInput("province", 
                            h4("Provinces"), 
                            choices = list("Alberta" = "AB", 
                                           "British Columbia" = "BC", 
                                           "Manitoba" = "MN",
                                           "New Brunswick" = "NB",
                                           "Newfoundland and Labrador" = "NL",
                                           "Nova Scotia" = "NS",
                                           "Ontario" = "ON",
                                           "Prince Edward Island" = "PEI",
                                           "Quebec" = "QC", 
                                           "Saskatchewan" = "SK",
                                           "Canada - Overall" = "CA"),
                            selected = NA)
      ),

      
      mainPanel(
        
        tabsetPanel(type="tabs",
                    tabPanel("Number of Cases",
                             plotlyOutput("plot_n_COPD"),
                             br(),
                             tableOutput("table_n_COPD")
                             
                    ),
                    
                    tabPanel("Cost",
                             selectInput("select", h5("Cost Type"), 
                                         choices = list("Total" = "total",
                                                        "Inpatient" = "in", 
                                                        "Outpatient" = "out",
                                                        "Pharma" = "pharma"), selected = NA),
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
   
  wb <- loadWorkbook("./Burden_of_COPD_BC_ProvidenceAPR04.xlsx", create=F)
  BC_cost_hosp <- readNamedRegion(wb, name = "BC_cost_hosp")
  
  output$plot_cost <- renderPlotly({
    print (cost_plot())
  })
  
  
  cost_plot <- reactive ({ 
    
   df <- BC_cost_hosp
   mdf <- reshape2::melt(df, id.var = "Year")
   mdf <- as.data.frame(mdf)
   #p <- ggplot(BC_cost_hosp, aes(x = Year)) #+ geom_line(aes(y = Male35), color="salmon") 

   p <- ggplot(mdf, aes(x = Year, y=value, colour = variable)) + geom_point() + geom_line() #+ geom_line(aes(y = Male35), color="salmon") 
   
  if ("male" %in% input$gender) {
     if (3554 %in% input$ageGroup) {p <- p + geom_line(aes(y = Male35), color = 1) }
     if (5564 %in% input$ageGroup) {p <- p + geom_line(aes(y = Male55), color = 2) }
     if (6574 %in% input$ageGroup) {p <- p + geom_line(aes(y = Male65), color = 3) }
     if (75 %in% input$ageGroup) {p <- p + geom_line(aes(y = Male75), color = 4) }
   }
   
   if ("female" %in% input$gender) {
     if (3554 %in% input$ageGroup) {p <- p + geom_line(aes(y = Female35), color = 5) }
     if (5564 %in% input$ageGroup) {p <- p + geom_line(aes(y = Female55), color = 6) }
     if (6574 %in% input$ageGroup) {p <- p + geom_line(aes(y = Female65), color = 7) }
     if (75 %in% input$ageGroup) {p <- p + geom_line(aes(y = Female75), color = 8) }
   }
    
      p <- p +  labs(x="year", y="COPD Cosy") + theme_bw() 
     
     ggplotly (p) #%>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
  })
    
  output$plot_n_COPD <- renderPlotly({
    print (n_copd_plot())
  })
  
  n_copd_plot <- reactive ({ 
    
    #readNamedRegion(wb, name = "BC_cost_hosp")
    
    # p <- ggplot(GLOBAL_prediction_results_fev1, aes(year)) + geom_line(aes(y = percentpred), color=lineColorSmoker, linetype=1) +
    #   geom_ribbon(aes(ymin=percentpred_lowerbound_PI, ymax=percentpred_upperbound_PI), linetype=2, alpha=0.1, fill=lineColorSmoker) +
    #   geom_line(aes(y = percentpred_lowerbound_PI), color=errorLineColorSmoker, linetype=2) +
    #   geom_line(aes(y = percentpred_upperbound_PI), color=errorLineColorSmoker, linetype=2) +
    #   labs(x=xlab, y="Number of COPD") +
    #   theme_bw() 
    # 
    # ggplotly (p) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

