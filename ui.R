library(shiny)
library(shinythemes)
setwd("/Users/craigheard/Documents/Miami Graduate School/Masters Project/ShinyApp/TimeSeriesPL/Data")
load( file = "appData.RData")

# Define UI for application that draws a histogram
shinyUI(navbarPage(" ", theme = shinytheme("united"),
  
  # Apply theme to app
  # theme = "bootstrap.css",
  
  tabPanel("Timeline",
           
           # Application title
           h3("History of the English Premier League", align = "center"),
    
           plotOutput("timePlot"), 
           
           fluidRow(
             
             column(2, img(src="premier-league-logo-header.png", height = 220, width = 290)),
             
             column(width = 3, helpText("Select the Premier League clubs you wish to compare:"),
                    
                    # Select 1st Team to Compare
                    selectInput(inputId = "team1", label = "Club #1:", choices = teams, 
                                selected = "Leicester City", multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL),
                    
                    # Select 2nd Team to Compare
                    selectInput(inputId = "team2", label = "Clubs #2:", choices = teams, 
                                selected = "Manchester United", multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL),
                    # Add slider input for time
                    sliderInput("range", "Select Time Period:",  sep = "",
                                min = 1996,  max = 2017, step = 1, value = c(1996, 2017)),
                    
                    offset = 1
                    
             ),
             
          
             column(width = 5, 
                    helpText("Points distribution for clubs selected:"),
                    tableOutput('summaryTab'),
                    br(),
                    h6(em("Created by Craig Heard (Miami University, Department of Statistics) .  
                          If interested learning more, see Heard, C.A. and Bailer A.J. (2018) 
                          Defying the odds: How likely are we to see another team pull a ‘Leicester’ 
                          and win the EPL?  Chance (09/2018)."))
                    
             )
             
           )
    
  ),
  
  
    
    tabPanel("Data",
            helpText("Initial Position represents where the club finished in the Premier League in the previous season. 
                     Teams with initial position 18, 19 and 20 represent the clubs who were promoted from
                     the Championship in 1st 2nd and Play-Off winner, respectively."),
            dataTableOutput("dataTable"))
    
  

   
  )
)
