#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Burden of COPD"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("selectPlot", h4("Select Plot"), 
                    choices = list("Prevalence" = "prev", 
                                   "Cost" = "cost",
                                   "Choice 3" = 3), selected = NA),
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
                                        "74 and older" = 74),
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

      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

