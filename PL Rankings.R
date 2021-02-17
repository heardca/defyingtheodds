############ Premier League Rankings ###############

# libraries
# library(Hmisc) # needed to get error bars
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(tidyverse)

setwd("/Users/craigheard/Documents/University/Miami Graduate School/Masters Project/Data")
rankings <- read.csv("PL Ranking Data.csv")

# variable needed for charts
brks <-seq(1:20)

###################################
######## Reconstruct tables #######
###################################
# filter out 2015-2016 season
#rankings <- rankings %>%
   # filter(Season <20)

# total points for higher ranked team 
season.hr <- rankings %>% group_by(Season, Rank,Team) %>%
          summarize(total_team_points = sum(Team.Points, na.rm = TRUE))

# total points for lower ranked team 
season.lr <- rankings %>% group_by(Season, Opposition.Rank, Opposition) %>%
  summarize(total_opp_points = sum(Opp.Points, na.rm = TRUE))

colnames(season.lr)[2] <- "Rank"

# total points for each team
season.comb <- merge(season.hr, season.lr, by=c("Season","Rank"), all= TRUE) %>%
                rowwise() %>%
                mutate(total_points = sum(total_team_points, total_opp_points, na.rm = TRUE))

# this code replaces NA in Team name to the Opp name.
season.comb$Club <- season.comb$Team
my.na <- is.na(season.comb$Team)
season.comb$Club[my.na] <- season.comb$Opposition[my.na]

# combine tables and sort by total points so it can be compared to original tables
season.comb <- season.comb %>% select(Season, Rank, Club, total_points) %>%
               arrange(Season, desc(total_points))

# seasons match up now :)

########################################
######### Home and Away Points #########
########################################

season.hr.loc <- rankings %>% group_by(Season, Rank, Location) %>%
  summarize(total_team_points = sum(Team.Points, na.rm = TRUE))

# total points for lower ranked team 
season.lr.loc <- rankings %>% group_by(Season, Opposition.Rank, Location) %>%
  summarize(total_opp_points = sum(Opp.Points, na.rm = TRUE))

colnames(season.lr.loc)[2] <- "Rank"

# total points for each team by location
season.comb.loc.tp <- merge(season.hr.loc, season.lr.loc, by=c("Season","Rank", "Location"), all= TRUE) %>%
  rowwise() %>%
  mutate(total_points = sum(total_team_points, total_opp_points, na.rm = TRUE)) %>%
  select(Rank, Location, total_points) %>% 
  group_by(Rank,Location) %>%
  summarize(Avg_Pts = mean(total_points, na.rm = TRUE)) %>%
  spread(Location, Avg_Pts) %>%
  mutate(Difference = Home - Away, Total = Home + Away) %>%
  select(Rank, Home, Away, Total) %>%
  gather(Status, Points, -Rank)

# std err for each team by location
season.comb.loc.se <- merge(season.hr.loc, season.lr.loc, by=c("Season","Rank", "Location"), all= TRUE) %>%
  rowwise() %>%
  mutate(total_points = sum(total_team_points, total_opp_points, na.rm = TRUE)) %>%
  select(Rank, Location, total_points) %>% 
  group_by(Rank,Location) %>%
  summarize(stdErr = sd(total_points, na.rm = TRUE)/sqrt(20)) %>%
  spread(Location, stdErr) %>%
  mutate(Total = (Home + Away)/2) %>%
  select(Rank, Home, Away, Total) %>%
  gather(Status, Points, -Rank)

season.comb.loc <- merge(season.comb.loc.tp, season.comb.loc.se, by=c("Rank", "Status") ) %>%
                   mutate(LWR = Points.x - 2*Points.y, UPR = Points.x + 2*Points.y) %>%
                   select(Rank, Status, Points.x, LWR, UPR)


colnames(season.comb.loc) <- c("Rank", "Status", "Points", "Lower", "Upper")
season.comb.loc$Status <- as.factor(season.comb.loc$Status)
season.comb.loc$Status <- factor(season.comb.loc$Status, c("Total", "Home", "Away"))

write_csv(season.comb.loc, "PL Avg Points Home and Away.csv")

homeAway <- season.comb.loc %>% filter(Status != "Total")

# plot showing average points by location for rankings
ggplot(aes(x = Rank, y = Points, color = Status),data=season.comb.loc) +
  geom_point(size = 3.5) +
  geom_errorbar(aes(ymax = Upper, ymin=Lower), width =0.4) +
  xlab("Ranking at the Begining of the Season") +
  ylab("Points") +
  theme_bw() +
  scale_size(guide = 'none') +
  scale_colour_manual(values = c("steelblue"," seagreen", "indianred")) +
  scale_x_continuous(breaks=brks) +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,82)) +
  ggtitle("Average Points for \nEPL Clubs from 1996-2016") +
  theme(plot.title=element_text(hjust=0.5))

# plot showing average points by location for rankings
ggplot(aes(x = Rank, y = Points, color = Status),data=homeAway) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymax = Upper, ymin=Lower), width =0.4) +
  xlab("Initial Ranking") +
  ylab("Average Points") +
  theme_bw() +
  scale_colour_manual(values = c("seagreen", "indianred")) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks=brks) +
  scale_y_continuous(breaks = seq(0,50,5), limits = c(0,50)) +
  #ggtitle("Average Home and Away Points for \nPremier League Clubs from 1996-2016") +
  theme(plot.title=element_text(hjust=0.5),
        panel.grid.minor = element_blank())



###################################
######### Expected Points #########
###################################

n <- 20 # number of seasons
# calculates the expected points for each game
exp_points <- rankings %>% group_by(Rank, Opposition.Rank, Location) %>%
                  summarize(exp_pts_team = sum(Team.Points)/n) %>%
                  mutate(exp_pts_opp = 3 - exp_pts_team)

# expected points for higher rank
exp.hr <- exp_points %>% group_by(Rank) %>%
  summarize(exp_hr_pts = sum(exp_pts_team, na.rm = TRUE), exp_hr_sd = sd(exp_pts_team, na.rm = TRUE))
# expected points for lower rank
exp.lr <- exp_points %>% group_by(Opposition.Rank) %>%
  summarize(exp_lr_pts = sum(exp_pts_opp, na.rm = TRUE), exp_lr_sd = sd(exp_pts_team, na.rm = TRUE))

colnames(exp.lr)[1] <- "Rank"
# merge team and opp expected points together
exp.comb <- merge(exp.hr, exp.lr, by=c("Rank"), all= TRUE) %>%
  rowwise() %>%
  mutate(total_exp_pts = sum(exp_hr_pts, exp_lr_pts, na.rm = TRUE),
         exp_pts_sd = mean(exp_hr_sd, exp_lr_sd, na.rm = TRUE) )

# plot of expected points for each rank
ggplot() +
  geom_point(aes(x = Rank, y = total_exp_pts, size = 0.75), data = exp.comb) +
  scale_x_continuous( breaks = brks) +
  theme_bw() +
  geom_errorbar(aes(ymin =  total_exp_pts, ymax = total_exp_pts), data = exp.comb) +
  xlab("Ranking at the Begining of the Season") +
  ylab("Season Expected Points") +
  scale_size(guide = 'none')

# add 95th percentile error bars
# look at box plots

#######################################################
######### Expected Points - Split Home & Away #########
#######################################################

# expected points for higher rank
exp.hr. <- exp_points %>% group_by(Rank) %>%
  summarize(exp_hr_pts = sum(exp_pts_team, na.rm = TRUE), exp_hr_sd = sd(exp_pts_team, na.rm = TRUE))
# expected points for lower rank
exp.lr <- exp_points %>% group_by(Opposition.Rank) %>%
  summarize(exp_lr_pts = sum(exp_pts_opp, na.rm = TRUE), exp_lr_sd = sd(exp_pts_team, na.rm = TRUE))

colnames(exp.lr)[1] <- "Rank"
# merge team and opp expected points together
exp.comb <- merge(exp.hr, exp.lr, by=c("Rank"), all= TRUE) %>%
  rowwise() %>%
  mutate(total_exp_pts = sum(exp_hr_pts, exp_lr_pts, na.rm = TRUE))

# plot of expected points for each rank
ggplot() +
  geom_point(aes(x = Rank, y = total_exp_pts, size = 0.75), data = exp.comb) +
  scale_x_continuous( breaks = brks) +
  theme_bw() +
  xlab("Ranking at the Begining of the Season") +
  ylab("Season Expected Points") +
  scale_size(guide = 'none')
  # geom_errorbar(aes(ymin =  total_exp_pts - , ymax = total_exp_pts + ), data = exp.comb) 





###################################
######## Home & Away Value ########
###################################

# expected points for team rank split by location
exp.hr.loc <- exp_points %>% group_by(Rank, Location) %>%
  summarize(exp_team_pts = sum(exp_pts_team, na.rm = TRUE))

# expected points for opp rank split by location
exp.lr.loc <- exp_points %>% group_by(Opposition.Rank, Location) %>%
  summarize(exp_opp_pts = sum(exp_pts_opp, na.rm = TRUE))

colnames(exp.lr.loc)[1] <- "Rank"
# merge team and opp expected points together split by location
# difference between home and away variable created
exp.comb.loc <- merge(exp.hr.loc, exp.lr.loc, by=c("Rank", "Location"), all= TRUE) %>%
  rowwise() %>%
  mutate(total_exp_pts = sum(exp_team_pts, exp_opp_pts, na.rm = TRUE)) %>%
  select(Rank, Location, total_exp_pts) %>% 
  spread(Location, total_exp_pts) %>%
  mutate(Difference = Home - Away, Total = Home + Away)

# look at home influence for different rankings
brks <-seq(1:20)
ggplot() +
  geom_point(aes(x = Rank, y = Difference, size = .75), stat="identity", data=exp.comb.loc) +
  xlab("Ranking at the Begining of the Season") +
  ylab("Difference (Home - Away)") +
  theme_bw() +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks=brks) +
  ggtitle("Differences between Home and Away Expected Points for \nPremier League Clubs from 1996-2016")

# look at stadium capacity as possible covariate

exp.comb.loc2 <- exp.comb.loc %>%
                 select(Rank, Home, Away, Total) %>%
                 gather(Status, Points, -Rank)

# plot showing expected points by location for rankings
ggplot() +
  geom_point(aes(x = Rank, y = Points, color = Status,size = .75), stat="identity",data=exp.comb.loc2) +
  xlab("Ranking at the Begining of the Season") +
  ylab("Points") +
  theme_bw() +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks=brks) +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  ggtitle("Expected Points for \nPremier League Clubs from 1996-2016")


