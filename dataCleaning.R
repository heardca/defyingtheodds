# Preliminary Code for Master's Project Code

#libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)

# set wd and grab files needed
setwd("/Users/craigheard/Documents/University/Miami Graduate School/Masters Project/ShinyApp/TimeSeriesPL/Data")
timeline <- read.csv("/Users/craigheard/DocumentsUniversity//Miami Graduate School/Masters Project//Data/PL Timeline.csv")
colors <- read.csv("/Users/craigheard/DocumentsUniversity//Miami Graduate School/Masters Project/S/Data/PL Team Colors.csv")
teams <- factor(colors$Team)

# 21 - Championship (Division 1)
# 22 - League One (Division 2)
# 23 - League Two (Division 3)
# 24 - Not in the football league

timeline$Year.Start <- as.numeric(str_sub(timeline$Season, 1,4))
brks2 <- seq(1:24)

# select only clubs that are outside the premier league and jitter there lines so it is
# easy to see when relegated and not all on one line
nonPrem <- timeline %>% 
  filter(Finishing_Pos > 20) %>%
  group_by(Team) %>%
  summarise()
jitterPrem <- as.vector(seq(-.15, .15, .3/40) )
nonPrem[["Jitter"]] <- jitterPrem

# merge files together so that each club has 20 observations for each season
timeline <- merge(timeline, nonPrem, by = "Team", all.x = TRUE)
timeline$Jitter[is.na(timeline$Jitter)] <- 0
timeline <- timeline %>% mutate(Position = round(Finishing_Pos + Jitter,2))

# merge the color of teams dataset
timeline <- merge(timeline, colors, by = "Team", all.x = TRUE)
timeline$Team.Colour <- as.character(timeline$Team.Colour)

# clean tables data up
tables <- timeline %>% 
  select(Season, Initial_Rank, Team, Finishing_Pos, GF, GA, GD, Points) %>%
  na.omit() %>%
  arrange(Season, Finishing_Pos)
colnames(tables) <- c("Season", "Initial\nPosition", "Club", "Finishing\nPosition", "GF", "GA", "GD", "Points")

# save RData file
save(timeline, tables, teams, file = "appData.RData")

