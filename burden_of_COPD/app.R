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
library(readr)
library(rmarkdown) #for markdown file
library(knitr) #for markdown file
library(htmltools)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Burden of COPD"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        checkboxGroupInput("gender", 
                           h4("Demographics"), 
                           choices = list(
                                          "female" = "Female", 
                                          "male" = "Male"),
                           selected = c("Female", "Male")
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
                             tableOutput("table_n_COPD"), 
                             br(), br(),
                             div(id = "SaveLoad",
                                 downloadButton("download_plot_n", "Download Plot")   
                             
                    )),
                    
                    tabPanel("Cost",
                             selectInput("costType", h5("Cost Type"), 
                                         choices = list("Total" = "sum",
                                                        "Inpatient" = "hosp", 
                                                        "Outpatient" = "MSP",
                                                        "Pharma" = "pharm"), selected = "sum"),
                             plotlyOutput("plot_cost"),
                             br(),
                             tableOutput("table_cost"), 
                             br(), br(),
                             div(id = "SaveLoad",
                                 downloadButton("download_plot_cost", "Download Plot")
                             
                            )),
                    tabPanel("Terms",  includeMarkdown("./disclaimer.rmd")),
                    tabPanel("About",  includeMarkdown("./about.rmd"), 
                             imageOutput("logos"))

        )
     )
  )
)

server <- function(input, output, session) {
   cost <- read_rds("./cost.rds")
   copdNumber <- read_rds("./copdNumber.rds")
   buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
   
  # wb <- loadWorkbook("./Burden_of_COPD_BC_ProvidenceAPR04.xlsx", create=F)
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
    print (cost_plot())
  })
  
  
  cost_plot <- reactive ({ 
   cost$Legend <- interaction(cost$province, cost$gender, cost$age)
   p <- ggplot(subset (cost, ((gender %in% input$gender) & (age %in% input$ageGroup) & (province %in% input$province) & (type %in% input$costType))), aes(x = Year, y=value/1000000, fill = Legend)) + 
        geom_bar(stat = "identity")  + labs(x="Year", y="") + scale_y_continuous(label=scales::dollar_format(suffix = "M")) + theme_bw() 
      
   ggplotly (p) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
  })
    
   output$plot_n_COPD <- renderPlotly({
     print (n_copd_plot())
   })
  
  n_copd_plot <- reactive ({ 
    copdNumber$Legend <- interaction(copdNumber$province, copdNumber$gender, copdNumber$age)
    p <- ggplot(subset (copdNumber, ((gender %in% input$gender) & (age %in% input$ageGroup) & (province %in% input$province))), aes(x = Year, y=value, color = Legend)) + 
         geom_point() + geom_line() + theme_bw() + labs(x="Year", y="")


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
  
}

# Run the application 
shinyApp(ui = ui, server = server)

