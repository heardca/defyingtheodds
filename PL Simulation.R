############### Simulation #################

# Simulation for every premier league season going back to 1996-97 season.

# libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

setwd("/Users/craigheard/Documents/Miami Graduate School/Masters Project/")
rankings <- read.csv("PL Ranking Data.csv")
tables <- read.csv("PL Tables.csv")

### create a table which has initial rank and different GD
gd.table <- tables %>% 
  select(Initial_Rank, GD)

#splits the gd into seasons
gd.table2 <- split(gd.table,gd.table$Initial_Rank)
gd.tab <-cbind(gd.table2$`1`$GD, gd.table2$`2`$GD, gd.table2$`3`$GD, gd.table2$`4`$GD,
               gd.table2$`5`$GD, gd.table2$`6`$GD, gd.table2$`7`$GD, gd.table2$`8`$GD,
               gd.table2$`9`$GD, gd.table2$`10`$GD, gd.table2$`11`$GD, gd.table2$`12`$GD,
               gd.table2$`13`$GD, gd.table2$`14`$GD, gd.table2$`15`$GD, gd.table2$`16`$GD,
               gd.table2$`17`$GD, gd.table2$`18`$GD, gd.table2$`19`$GD, gd.table2$`20`$GD )
gd.tab <- t(gd.tab)

apply(gd.tab, 1, mean)
apply(gd.tab, 1, sd)

# creates rankings
rankings <- rankings %>% select(Season,Year, Rank, Team, Opposition.Rank, Opposition, Game.., 
                                Location, Team.Points, Opp.Points)

# build 380 matrix with probabilities for each match
rankings$Location <- as.integer(ifelse(rankings$Location == "Home", 1, 0)) # used to identify Home = 1 and Away =0
results_table <- rankings %>% 
                 group_by(Rank, Opposition.Rank, Location, Team.Points) %>%
                 tally() %>% 
                 spread(Team.Points, n)

results_table[is.na(results_table)] <- 0

colnames(results_table)[4] <- "Losses"
colnames(results_table)[5] <- "Draws"
colnames(results_table)[6] <- "Wins"

prob_table <- results_table %>% 
              mutate( Games = Losses + Draws + Wins, pi_Loss = Losses/Games, 
                                     pi_Draw = Draws/Games, pi_Win = Wins/Games) %>% 
              select(Rank, Opposition.Rank, Location, pi_Loss, pi_Draw, pi_Win)

p.table <- as.matrix(prob_table)

# simulation

nseasons <-1
season <- matrix(nrow=nseasons*380,ncol=6)
colnames(season) <- c("i", "j", "k", "u", "pts_i", "pts_j")

nsim <- nseasons*380

# loop over games
for (igame in 1:nsim) {
  team_i <- p.table[igame,1]
  season[igame,1] <- team_i # put team i in matrix
  
  team_j <- p.table[igame,2]
  season[igame,2] <- team_j # put team j in matrix
  
  home  <- p.table[igame,3]
  season[igame,3] <- home # put location in matrix
  
  pt.mat <- as.matrix(p.table[igame,4:6])
  
  cut1 <- pt.mat[1,]
  cut2 <- cut1 + pt.mat[2,]
  
  u <- runif(1,0,1)
  season[igame,4] <- u # put u in matrix
  
  season[igame,5] <-  ifelse(u < cut1, 0,  ifelse( u < cut2, 1, 3))
  
  season[igame,6] <-  ifelse(season[igame,5] == 0, 3,  ifelse(season[igame,5] == 1, 1, 0))
  
}

###########################################################################
############################  Multiple Seasons ############################
###########################################################################
set.seed(8374625)
nseasons <- 10000 # number of seasons we want to simulate
seasons <- matrix(nrow=nseasons*380,ncol=7)
colnames(seasons) <- c("season", "i", "j","k","u","pts_i","pts_j")

nsim <- nseasons*380 # total number of simulated games (380 games per season)

# loop over games
for (igame in 1:nsim) {
  
  # this creates a value for season number. eg. season 2 game 1 vs 2 would be observation 381.
  season <- ceiling(igame/380)
  seasons[igame,1] <- season # sends season number to table.
  
  # this variable is created to refer the observation to correct row in the probability table
  # 381 mod 380 = 1
  mod_repeat <- ifelse(igame %% 380 == 0, 380, igame %% 380)
  
  # creates higher ranked team i in the table
  team_i <- p.table[mod_repeat,1]
  seasons[igame,2] <- team_i # put team i in matrix
  
  # creates lower ranked team j in the table
  team_j <- p.table[mod_repeat,2]
  seasons[igame,3] <- team_j # put team j in matrix
  
  # creates status of the game in the table (0 - Away or 1 - Home)
  home  <- p.table[mod_repeat,3]
  seasons[igame,4] <- home # put location in matrix
  
  # subsets the probabity table to just the probability columns 
  pt.mat <- as.matrix(p.table[mod_repeat, 4:6])
  
  # cut1 represents the probability of losing 
  cut1 <- pt.mat[1,]
  # cut2 represents the probability of drawing 
  cut2 <- cut1 + pt.mat[2,]
  
  # use the random uniform distribution to simulate a random number from 0 to 1
  u <- runif(1,0,1)
  
  seasons[igame,5] <- u # put u in matrix
  
  # first conditional statement if u < probability of losing then, team i gets 0 points,
  #                             if probability of losing < u < probability of losing + drawing, 
  #                             then team i gets 1 point
  #                             else team i gets 3 points (u > probability of losing + drawing)
  seasons[igame,6] <-  ifelse(u < cut1, 0,  ifelse( u < cut2, 1, 3))
  
  # calculates points for the lower ranked team.
  seasons[igame,7] <-  ifelse(seasons[igame,6] == 0, 3,  ifelse(seasons[igame,6] == 1, 1, 0))
  
}

seasons <- as.data.frame(seasons)

# total points for higher ranked team 
sim.hr <- seasons %>% group_by(season, i) %>%
  summarize(total_hr_points = sum(pts_i, na.rm = TRUE))

# total points for lower ranked team 
sim.lr <- seasons %>% group_by(season, j) %>%
  summarize(total_lr_points = sum(pts_j, na.rm = TRUE))

colnames(sim.lr)[2] <- "i"

# total points for each team
season.table <- merge(sim.hr, sim.lr, by=c("season","i"), all= TRUE) %>%
  rowwise() %>%
  mutate(total_points = sum(total_hr_points, total_lr_points, na.rm = TRUE))


# adding goal difference to rankings because some teams might end on the same points
teams <- 20 # no. of teams in the league
iterations <- teams*nseasons
for(diff in 1:iterations){
  
  # this variable is created to refer the observation to correct row in the gd table
  mod_repeat <- ifelse(diff %% 20 == 0, 20, diff %% 20)
  season.table$mod <- mod_repeat
  
  season.table$GD[diff] <- sample(gd.tab[mod_repeat,], 1) 
  
}

# creates the final rankings of teams by season, ranking on total points first and then GD
sorted <- season.table %>%
  select(season, i, total_points, GD) %>%
  arrange(season, -total_points, -GD) %>%
  group_by(season) %>%
  mutate(final = row_number())

save(sorted, file = "Simulated Season Tables.RData")

# contingency table to show
cont <- table(sorted$i, sorted$final)

cont.df <-as.data.frame.matrix(cont)
cont.df.prop <- as.data.frame.matrix(round(prop.table(cont, 1)*100,2) )

save(cont.df,file="Simulation Contingency Table.Rda")
save(cont.df.prop,file="Simulation Contingency Table (Proportions)2.Rda")

# run 20 trials of 10,000 seasons.

# display the first simulation results as a data frame
cont.df11 <-as.data.frame(as.table(as.matrix(cont.df)))
# display second sim results
cont.df12 <-as.data.frame(as.table(as.matrix(cont.df)))
# display third sim results
cont.df13 <-as.data.frame(as.table(as.matrix(cont.df)))
# display fourth sim results
cont.df14 <-as.data.frame(as.table(as.matrix(cont.df)))
# display fifth sim results
cont.df15 <-as.data.frame(as.table(as.matrix(cont.df)))

save(cont.df11,cont.df12,cont.df13,cont.df14,cont.df15, file="Simulation Cont Table DF Results 11-15.Rda")

# display sixth sim results
cont.df16 <-as.data.frame(as.table(as.matrix(cont.df)))
# display seventh sim results
cont.df17 <-as.data.frame(as.table(as.matrix(cont.df)))
# display eigth sim results
cont.df18 <-as.data.frame(as.table(as.matrix(cont.df)))
# display ninth sim results
cont.df19 <-as.data.frame(as.table(as.matrix(cont.df)))
# display tenth sim results
cont.df20 <-as.data.frame(as.table(as.matrix(cont.df)))

save(cont.df11,cont.df12,cont.df13,cont.df14,cont.df15,cont.df16,
     cont.df17,cont.df18,cont.df19,cont.df20, file="Simulation Cont Table DF Results 11-20.Rda")


