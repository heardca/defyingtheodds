# old summary code for app

### server 
# output$stats1 <- renderPrint({
#   summary(timeline[timeline$Team==input$team1,"Points"])
# })
# 
# output$stats2 <- renderPrint({
#   summary(timeline[timeline$Team==input$team2,"Points"])
# })
# 
# output$text1a <- renderUI({
#   HTML(paste("Points Distribution for ", input$team1, sep = "") )
# })
# 
# output$text1b <- renderUI({
#   HTML(paste(input$team1, " have competed in ", timeline[timeline$Team==input$team1,"Seasons"][1],
#         " Premier League Seasons since 1996.\n ", sep = ""))
# })
# 
# output$text2a <- renderUI({
#   HTML(paste("Points Distribution for ", input$team2, sep = ""))
# })
# 
# output$text2b <- renderUI({
#   HTML(paste(input$team2, " have competed in ", timeline[timeline$Team==input$team2,"Seasons"][1],
#         " Premier League Seasons since 1996.\n ", sep = ""))
# })
# 
# output$text3a <- renderUI("Note: NA's represent seasons not in the Premier League")

### ui

# # Summary Stats for Team 1
# column(width = 5,
#        htmlOutput("text1a"),
#        verbatimTextOutput("stats1"),
#        htmlOutput("text1b"),
#        br()
#        ),
# 
# # Summary Stats for Team 2
# column(width = 5,
#        htmlOutput("text2a"),
#        verbatimTextOutput("stats2"),
#        htmlOutput("text2b"),
#        br(),
#        htmlOutput("text3a")
#        ),
