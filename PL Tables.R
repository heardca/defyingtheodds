############ Premier League Tables ###############

#### libraries ####
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(reshape)
library(reshape2)

# files to be read in 
setwd("/Users/craigheard/Documents/University/Miami Graduate School/Masters Project/Data")
tables <- read.csv("PL Tables.csv")
colors <- read.csv("PL Team Colors.csv")
timeline <- read.csv("PL Timeline.csv")

# load simulation results for all 20 seasons
load("/Users/craigheard/Documents/Miami Graduate School/Masters Project/Simulation Results/Simulation Contingency Table.Rda")
load("/Users/craigheard/Documents/Miami Graduate School/Masters Project/Simulation Results/Simulation Contingency Table (Proportions).Rda")
sim.table <- cont.df
sim.prop.table <- cont.df.prop

# load simulation results for all seasons minus Leicesters title winning year
load("/Users/craigheard/Documents/Miami Graduate School/Masters Project/Simulation Results/Simulation Contingency Table (without 2015-16).Rda")
load("/Users/craigheard/Documents/Miami Graduate School/Masters Project/Simulation Results/Simulation Contingency Table (Proportions without 2015-16).Rda")
sim.table2 <- cont.df
sim.prop.table2 <- cont.df.prop

remove(cont.df)
remove(cont.df.prop)

# variable needed for charts
brks <-seq(1:20)

# original contingency table of initial ranking vs. final ranking
cont.original <- as.data.frame.matrix(prop.table(table(tables$Initial_Rank, tables$Finishing_Pos),1)*100)

##### new variables ####
tables$Outcome <- ifelse(tables$Finishing_Pos == 1, "Champions", 
                         ifelse(tables$Finishing_Pos == 2 | tables$Finishing_Pos == 3 |
                                  tables$Finishing_Pos == 4, "CL Qualification", 
                                ifelse(tables$Finishing_Pos == 5 | tables$Finishing_Pos == 6 |
                                         tables$Finishing_Pos == 7, "EL Qualification", 
                                       ifelse(tables$Finishing_Pos == 18 | tables$Finishing_Pos == 19 |
                                                tables$Finishing_Pos == 20, "Relegated", "Survived"))))
tables$Outcome <- factor(tables$Outcome)
tables$Year.Start <- as.numeric(str_sub(tables$Season, 1,4))
tables$Season.Code  <- rep(1:20, each = 20) # creates a variable where 1996/97 = 1, 1997/98 = 2 etc.


# summarize total points per team
total_points <- tables %>% 
                group_by(Team) %>%
                summarize(total_points = sum(Points, na.rm = TRUE), no_seasons= n(),
                          mean_points = round(mean(Points),2)) %>%
                arrange(desc(total_points))

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


##################################################
########### Scatterplot and Jitter plot ##########
##################################################

# Graph showing the difference between initial and final position for PL Clubs
# scatterplot
ggplot() +
  geom_point(aes(x = Initial_Rank, y = Difference, color = Outcome, alpha=.01, size = 1.5), 
             stat="identity", data=tables) +
  xlab("Ranking at the Begining of the Season") +
  scale_color_manual(values=c("dodgerblue", "seagreen4", "yellow","red","slategray")) +
  theme_bw() +
  scale_alpha(guide = 'none') +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks=brks) +
  ggtitle("Differences between Intitial and Final Positions for \nPremier League Clubs from 1996-2016")

# jitter plot
ggplot() +
  geom_jitter(aes(x = Initial_Rank, y = Difference, color = Outcome, alpha = 0.5, size = 1.25), 
             stat="identity", height =NULL, width = 0.5, data=tables) +
  xlab("Ranking at the Begining of the Season") +
  scale_color_manual(values=c("dodgerblue", "seagreen4", "yellow","red","slategray")) +
  theme_bw() +
  scale_alpha(guide = 'none') +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks=brks, limits = c(1,20)) +
  scale_y_continuous(breaks=seq(-14,16,2)) +
  ggtitle("Differences between Intitial and Final Positions \nfor Premier League Clubs from 1996-2016")+
  annotate("text", x=11.7, y=13.25, label= "Leicester City 2015/16", size = 2.5) +
  annotate("text", x=2.9, y=-13.4, label= "Ipswich Town 2001/02", size = 2.5) +
  annotate("text", x=8.3, y=-12.95, label= "Blackburn Rovers 1998/99", size = 2.5) + 
  annotate("text", x=11.5, y=-10.9, label= "Nottingham Foreset 1996/97", size = 2.5) +
  annotate("text", x=17.9, y=15.1, label= "Ipswish Town 2000/01", size = 2.5) +
  annotate("text", x=18.6, y=13, label= "Everton 2004/05", size = 2.5) +
  annotate("text", x=2.6, y=-8.9, label= "Chelsea 2015/06", size = 2.5) +
  annotate("text", x=3.6, y=-11.8, label= "Newcastle 1997/98", size = 2.5) +
  theme(plot.title=element_text(hjust=0.5))

ggsave(filename = "jitted.png", width =8.5, height =7.49 ,dpi = 600)

#### Initial vs. Final position
ggplot() +
  geom_jitter(aes(x = Initial_Rank, y = Finishing_Pos, color = Outcome, alpha = 0.5, size = 1.25), 
              stat="identity", height = 0, width = 0.2, data=tables) +
  xlab("Initial Ranking") +
  ylab("Finishing Ranking") +
  scale_color_manual(values=c("dodgerblue", "seagreen4", "yellow","red","slategray")) +
  theme_bw() +
  scale_alpha(guide = 'none') +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks=brks) +
  scale_y_reverse(breaks=brks ) +
  ggtitle("Intitial and Final Rankings for\n Premier League Clubs from 1996-2016") +
  annotate("text", x=14, y=1.7, label= "Leicester City\n2015/16", size = 2.5) +
  annotate("text", x=5, y=18.7, label= "Ipswich Town\n 2001/02", size = 2.5) +
  annotate("text", x=20, y=5.7, label= "Ipswish Town\n 2000/01", size = 2.5) +
  annotate("text", x=17, y=4.7, label= "Everton\n 2004/05", size = 2.5) +
  annotate("text", x=1, y=10.7, label= "Chelsea\n 2015/06", size = 2.5) +
  annotate("text", x=2, y=13.7, label= "Newcastle\n 1997/98", size = 2.5) +
  geom_hline(yintercept=1, color = "dodgerblue", linetype = 2, alpha=0.5) +
  geom_hline(yintercept=4, color = "seagreen4", linetype = 2, alpha=0.5) +
  geom_hline(yintercept=7, color = "yellow", linetype = 2, alpha=0.5) +
  geom_hline(yintercept=18, color = "red", linetype = 2, alpha=0.5) +
  geom_hline(yintercept=20, color = "red", linetype = 2, alpha=0.5) +
  theme(plot.title=element_text(hjust=0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())


# graph showing iniital positions vs. final position
ggplot() +
  geom_point(aes(x=Initial_Rank, y=Finishing_Pos, color=GD, size = 1.5), data = tables) +
  scale_color_gradient2(low="darkred" , mid ="gray", high="seagreen", name = "GD") +
  theme_bw() +
  scale_size(guide = 'none') +
  xlab("Ranking at the Begining of the Season") +
  scale_x_reverse( lim=c(20,0)) +
  ylab("Ranking at the End of the Season") +
  scale_y_reverse( lim=c(20,0))

##################################################
################ Time Series Plots ###############
##################################################
# 21 - Championship (Division 1)
# 22 - League One (Division 2)
# 23 - League Two (Division 3)
# 24 - Not in the football league
timeline$Year.Start <- as.numeric(str_sub(timeline$Season, 1,4))
brks2 <- seq(1:24)
# filter by teams and positions
top_clubs <- timeline %>% filter(Team == "Manchester United" | Team == "Manchester City" |
                                 Team == "Chelsea" | Team == "Arsenal" | Team == "Liverpool" |
                                 Team == "Everton" | Team == "Tottenham Hotspur")

# Time Series Plot showing team performance over 20 year span
ggplot() +
  geom_line(aes(x = Year.Start, y = Finishing_Pos, color = Team), data = top_clubs) +
  geom_point(aes(x = Year.Start, y = Finishing_Pos, color = Team), data = top_clubs) +
  scale_y_continuous(breaks=brks2, 
                     labels=c(seq(1,20, by=1), "Championship", "League 1", "League 2", "Non-League"),
                     trans = "reverse") +
  scale_x_continuous(breaks=seq(1996,2016,2)) +
  ylab("Finishing Position") +
  xlab("Time") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_hline(yintercept=2, color = "blue", linetype = 2) +
  geom_hline(yintercept=4, color = "green", linetype = 2) +
  geom_hline(yintercept=7, color = "yellow", linetype = 2) +
  geom_hline(yintercept=18, color = "red", linetype = 2) +
  geom_hline(yintercept=20, color = "red", linetype = 2)

bottom_clubs <- timeline %>% filter(Team == "Wimbledon" | Team == "Portsmouth" |
                                 Team == "Norwich City" | Team == "Leeds United" | Team == "Ipswich Town" |
                                 Team == "Leicester City" | Team == "Bolton Wanderers")

# Time Series Plot showing team performance over 20 year span
ggplot() +
  geom_line(aes(x = Year.Start, y = Finishing_Pos, color = Team), data = bottom_clubs) +
  geom_point(aes(x = Year.Start, y = Finishing_Pos, color = Team), data = bottom_clubs) +
  scale_y_continuous(breaks=brks2, labels=c(seq(1,20, by=1), 
                                            "Championship", "League 1", "League 2", "Non-League"),trans = "reverse") +
  scale_x_continuous(breaks=seq(1996,2015,2)) +
  ylab("Finishing Position") +
  xlab("Time") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_hline(yintercept=2, color = "blue", linetype = 2) +
  geom_hline(yintercept=4, color = "green", linetype = 2) +
  geom_hline(yintercept=7, color = "yellow", linetype = 2) +
  geom_hline(yintercept=18, color = "red", linetype = 2) +
  geom_hline(yintercept=20, color = "red", linetype = 2)

##################################################
#################### Box Plots ###################
##################################################

# box plots showing initial positions (x-axis) and final position (y-axis)
ggplot() +
  geom_boxplot(aes(x = Initial_Rank, y = Finishing_Pos, group = Initial_Rank), data = tables) +
  scale_x_reverse( lim=c(21,0)) +
  scale_y_reverse( lim=c(22,1)) +
  theme_bw()

# box plots showing initial positions (x-axis) and points (y-axis)
median_points_initial <- tables %>% group_by(Initial_Rank) %>% 
                summarize( median_points = median(Points))

ggplot() +
  geom_boxplot(aes(x = Initial_Rank, y = Points, group = Initial_Rank), data = tables) +
  scale_x_continuous(breaks = brks) +
  xlab("Initial Ranking") +
  scale_y_continuous(breaks = seq(0,100,10), lim=c(0,100)) +
  theme_bw() +
  geom_text(data = median_points_initial, aes(x = Initial_Rank, y = median_points, label = median_points), 
            size = 2.75, vjust = -0.5) +
  # ggtitle("Initial Ranks End of Season Points for \nPremier League Clubs from 1996-2016") +
  annotate("text", x=1, y=47, label= "Chelsea \n 2015/16", size = 2.5) +
  annotate("text", x=4, y=47, label= "Everton \n 2005/06", size = 2.5) +
  annotate("text", x=10, y=71, label= "Man City \n 2009/10", size = 2.5) +
  annotate("text", x=11, y=75, label= "Newcastle \n 2001/02", size = 2.5) +
  annotate("text", x=11, y=30, label= "Norwich City \n 2013/14", size = 2.5) +
  annotate("text", x=14, y=85, label= "Leicester City \n 2015/16", size = 2.5) +
  annotate("text", x=14, y=16, label= "Portsmouth \n 2013/14", size = 2.5) +
  annotate("text", x=16, y=60, label= "Aston Villa \n 2003/04", size = 2.5) +
  annotate("text", x=20, y=8, label= "Derby County \n 2007/08", size = 2.5) +
  theme(plot.title=element_text(hjust=0.5))

# box plots showing final positions (x-axis) and points (y-axis)
median_points_finish <- tables %>% group_by(Finishing_Pos) %>% 
  summarize( median_points = median(Points))

ggplot() +
  geom_boxplot(aes(x = Finishing_Pos, y = Points, group = Finishing_Pos), data = tables) +
  scale_x_continuous(breaks = brks) +
  xlab("Ranking at the Begining of the Season") +
  scale_y_continuous(breaks = seq(0,100,10), lim=c(0,100)) +
  theme_bw() +
  geom_text(data = median_points_finish, aes(x = Finishing_Pos, y = median_points, label = median_points), 
            size = 2.75, vjust = -0.5) +
  ggtitle("Final Ranks End of Season Points for \nPremier League Clubs from 1996-2016")


# box plots showing initial positions (x-axis) and GD (y-axis)
median_GD <- tables %>% group_by(Initial_Rank) %>% 
  summarize( median_GD = median(GD))

ggplot() +
  geom_boxplot(aes(x = Initial_Rank, y = GD, group = Initial_Rank), data = tables) +
  scale_x_continuous(breaks = brks) +
  xlab("Ranking at the Begining of the Season") +
  ylab("Goal Difference") +
  scale_y_continuous(breaks = seq(-80,80,20), lim=c(-80,80)) +
  theme_bw() +
  geom_text(data = median_GD, aes(x = Initial_Rank, y = median_GD, label = median_GD), 
            size = 2.75, vjust = -0.5) +
  # ggtitle("Initial Ranks and End of Season GD for \nPremier League Clubs from 1996-2016") +
  theme(plot.title=element_text(hjust=0.5))


#################################################
############### Markov Chaines ###################
#################################################

# firstly group initial rankings and final rankings into groups
attach(tables)
tables$Initial_Rank_Group[Initial_Rank <= 3] <- "a1-3"
tables$Initial_Rank_Group[Initial_Rank > 3  & Initial_Rank <= 10] <- "b4-10"
tables$Initial_Rank_Group[Initial_Rank > 10  & Initial_Rank <= 17] <- "c11-17"
tables$Initial_Rank_Group[Initial_Rank >18] <- "d18-20"
tables$Final_Rank_Group[Finishing_Pos <= 3] <- "a1-3"
tables$Final_Rank_Group[Finishing_Pos > 3  & Finishing_Pos <= 10] <- "b4-10"
tables$Final_Rank_Group[Finishing_Pos > 10  & Finishing_Pos <= 17] <- "c11-17"
tables$Final_Rank_Group[Finishing_Pos >18] <- "d18-20"
detach(tables)

trans <- as.matrix(table(tables$Initial_Rank_Group, tables$Final_Rank_Group))
trans.p <- sweep(trans[,-1], 1, rowSums(trans[,-1]), FUN="/")
trans.p <- prop.table(trans,1)

x <- c(1,0,0,0)
trans.p%*%x
trans.p2%*%x


trans.p2 <- trans.p %*% trans.p; trans.p2
trans.p3 <- trans.p2 %*% trans.p; trans.p3

#################################################
################## Heat Map #####################
#################################################

###############################################
# creates a heat map from data
cont.original$Initial<-seq.int(nrow(cont.original))
# turn cont table into format so that a heat map can be created.
cont.original2 <- cont.original %>%
  gather(Final, Prop, 1:20)
cont.original2$Final <- as.numeric(cont.original2$Final)

# heat map of data collected of initial vs final rankings - Empirical Probability
ggplot() +
  geom_tile(aes(x = Initial, y = Final, fill = Prop), data = cont.original2) +
  scale_fill_gradient2(low = "red", mid = "white", high = "forestgreen", "%") +
  xlab("Ranking at the Begining of the Season") +
  scale_x_continuous(breaks=brks) +
  ylab("Ranking at the End of the Season") +
  scale_y_reverse( lim=c(20,0), breaks=brks) +
  theme_bw()

###############################################
# creates a heat map for simulated results
sim.prop.table$Initial<-seq.int(nrow(sim.prop.table))
# turn sim table into format so that a heat map can be created.
sim.prop.table2 <- sim.prop.table %>%
  gather(Final, Prop, 1:20)
sim.prop.table2$Final <- as.numeric(sim.prop.table2$Final)

# heat map of data collected of initial vs final rankings
ggplot() +
  geom_tile(aes(x = Initial, y = Final, fill = Prop), data = sim.prop.table2) +
  scale_fill_gradient2(low = "red", mid = "white", high = "forestgreen", "%") +
  xlab("Ranking at the Begining of the Season") +
  scale_x_continuous(breaks=brks) +
  ylab("Ranking at the End of the Season") +
  scale_y_reverse( lim=c(20,0), breaks=brks) +
  theme_bw()


###############################################
# creates a heat map looking at the difference between original contingency proportions (cont.original)
# and the simulated contingency table (cont.df.prop) of initial vs final rank
diff.table <- as.data.frame(cont.original - sim.prop.table)
diff.table$Initial<-seq.int(nrow(diff.table))

# turn difference table into format so that a heat map can be created.
diff.table2 <- diff.table %>%
              gather(Final, Prop, 1:20)
diff.table2$Final <- as.numeric(diff.table2$Final)

# heat map of difference in proportions
ggplot() +
  geom_tile(aes(x = Initial, y = Final, fill = Prop), data = diff.table2) +
  scale_fill_gradient2(low = "red", mid = "white", high = "forestgreen", "Difference (%)") +
  xlab("Ranking at the Begining of the Season") +
  scale_x_continuous(breaks=brks) +
  ylab("Ranking at the End of the Season") +
  scale_y_reverse( lim=c(20,0), breaks=brks) +
  theme_bw()

#combine proportions
cont.comb <- merge(cont.original2, sim.prop.table2, by = c("Initial", "Final"))
cont.comb <- merge(cont.comb, diff.table2, by = c("Initial", "Final"))
colnames(cont.comb) <- c("Initial", "Final","Empirical", "Simulation", "Difference")

cont.comb2 <- cont.comb %>%
             gather(Status, Percent, Empirical:Difference)

cont.comb2$Status_f = factor(cont.comb2$Status, levels=c('Empirical','Simulation','Difference'))

ggplot() +
  geom_tile(aes(x = Initial, y = Final, fill = Percent), data = cont.comb2) +
  scale_fill_gradient2(low = "red", mid = "white", high = "forestgreen", "Difference (%)") +
  facet_grid(. ~ Status_f, scales = "free_y") +
  xlab("Ranking at the Begining of the Season") +
  scale_x_continuous(breaks=seq(1,20,1)) +
  ylab("Ranking at the End of the Season") +
  scale_y_reverse( lim=c(20,0), breaks=brks) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  


###########################################################################
################## Average Points Per Initial Ranking #####################
###########################################################################

initial.avg <- tables %>%
               group_by(Initial_Rank) %>%
               summarize(avg_pts = mean(Points))

ggplot() +
  geom_point(aes(x = Initial_Rank, y = avg_pts), data = initial.avg)


tables %>%
  filter(Initial_Rank < 7) %>%
  ggplot() +
  geom_smooth(aes(x = Year.Start, y = Finishing_Pos, color = factor(Initial_Rank))) +
  scale_y_continuous(breaks=brks2, 
                     labels=c(seq(1,20, by=1), "Championship", "League 1", "League 2", "Non-League"),
                     trans = "reverse") +
  scale_x_continuous(breaks=seq(1996,2016,2)) +
  ylab("Finishing Position") +
  xlab("Time") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



