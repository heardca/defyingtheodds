library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(xtable)

setwd("/Users/craigheard/Documents/Miami Graduate School/Masters Project/ShinyApp/TimeSeriesPL/Data")
load( file = "appData.RData")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Build time series plot
  output$timePlot <- renderPlot({
    
    # Build a layering plot for all of the clubs - NOISE
    ggplot() +
      geom_line(aes(x = Year.Start, y = Position, group = Team), alpha = 0.25,
                color = "gray",data = timeline) +
      scale_y_continuous(breaks=seq(1,24,1), 
                         labels=c(seq(1,20, by=1), "Championship", "League 1", "League 2", "Non-League"),
                         trans = "reverse") +
      scale_x_continuous(breaks=seq(input$range[1],input$range[2],2), limits = c(input$range[1],input$range[2])) +
      ylab("Finishing Position") +
      xlab("Time") +
      theme_bw() +
      
    # Pick first team
    geom_line(aes(x = Year.Start, y = Position), size = 1, color = timeline[timeline$Team==input$team1,"Team.Colour"][1],
                 data = timeline[timeline$Team==input$team1,]) +
      
    # Pick second team
    geom_line(aes(x = Year.Start, y = Position), color = timeline[timeline$Team==input$team2,"Team.Colour"][1],size = 1, 
                data = timeline[timeline$Team==input$team2,]) +
      
    # Add themes and lines for relegation/qualifications
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      geom_hline(yintercept=1, color = "gold", linetype = 2) +
      geom_hline(yintercept=4, color = "seagreen4", linetype = 2) +
      geom_hline(yintercept=7, color = "lightskyblue", linetype = 2) +
      geom_hline(yintercept=18, color = "red", linetype = 2) +
      geom_hline(yintercept=20, color = "red", linetype = 2) +
      
    # Add labels for relegation/qualifcations regions
      annotate("text", x=input$range[1], y=19, label= "Relegation", size = 2.5, color ="red") +
      annotate("text", x=input$range[1], y=5.5, label= "Europa\n League", size = 2.5, color ="lightskyblue2") +
      annotate("text", x=input$range[1], y=2.5, label= "Champions\n League", size = 2.5, color ="seagreen4") +
      
    # Add Team name at the bottom in the same colour as the line so it's easy to see which team is which.
      annotate("text", x=input$range[1]+2, y=24,
               label = input$team1, size = 4, color =timeline[timeline$Team==input$team1,"Team.Colour"][1]) +
      annotate("text", x=input$range[1]+5, y=24,
               label = input$team2, size = 4, color =timeline[timeline$Team==input$team2,"Team.Colour"][1]) +
      scale_colour_continuous(guide = FALSE)
    
  })
  
  
  # show dataset
  output$dataTable <- renderDataTable(tables, options = list(pageLength = 20))
  
  # create summary table for each team
  output$summaryTab <- renderTable({
    
    # Team 1 stats
    team <- timeline[timeline$Team==input$team1,"Points"]
    meanTeam <-mean(team,na.rm = TRUE)
    quanTeam <-quantile(team, c(0.25,0.50,0.75),na.rm = TRUE)
    maxTeam <-max(team,na.rm = TRUE)
    minTeam <-min(team,na.rm = TRUE)
    naTeam <-sum(is.na(team))
    nameTeam <- input$team1
    
    # Team 2 Stats
    team2 <- timeline[timeline$Team==input$team2,"Points"]
    meanTeam2 <-mean(team2,na.rm = TRUE)
    quanTeam2 <-quantile(team2, c(0.25,0.50,0.75),na.rm = TRUE)
    maxTeam2 <-max(team2,na.rm = TRUE)
    minTeam2 <-min(team2,na.rm = TRUE)
    naTeam2 <-sum(is.na(team2))
    nameTeam2 <- input$team2
    
    # create data frame of summary statistics
    df <- as.table(rbind(c(nameTeam,minTeam,quanTeam[1],quanTeam[2],round(meanTeam,2),quanTeam[3],maxTeam,naTeam),
                         c(nameTeam2,minTeam2,quanTeam2[1],quanTeam2[2],round(meanTeam2,2),quanTeam2[3],maxTeam2,naTeam2)))
    colnames(df) <-c("Club","Minimum", "Q1", "Median","Mean","Q3","Maximum", "Seasons Not in PL")
    
    # convert df into table
    xtable(df) 
    
    })
  
  
})

