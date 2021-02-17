## Probability Charts
# libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(gridExtra)
library(tidyverse)

setwd("/Users/craigheard/Documents/University/Miami Graduate School/Masters Project/Data")
rankings <- read.csv("PL Ranking Data.csv")

# creates rankings
rankings <- rankings %>% 
  select(Season,Year, Rank, Team, Opposition.Rank, Opposition, Game.., 
                                Location, Team.Points, Opp.Points) %>%
  filter(Season <20)
                          

# variable needed for charts
brks <-seq(1:19)

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
          pi_Draw = Draws/Games, pi_Win = Wins/Games,
          Strata = cut(Rank, breaks = c(0,4,6,20))) %>% 
  select(Strata, Rank, Opposition.Rank, Location, pi_Loss, pi_Draw, pi_Win)
p.table <- as.matrix(prob_table)

# join prob and results table and export
probresults_table <- results_table %>% 
  mutate( Games = Losses + Draws + Wins, pi_Loss = Losses/Games, 
          pi_Draw = Draws/Games, pi_Win = Wins/Games,
          Strata = cut(Rank, breaks = c(0,4,6,20)))
write_csv(probresults_table, "PL Probability + Results.csv")

# create difference columns
prob_table$Difference <- prob_table$Opposition.Rank - prob_table$Rank
prob_table$NoLose <- 1 - prob_table$pi_Loss
prob_table$Location <- factor(prob_table$Location)
levels(prob_table$Location) <- c("Away", "Home")

ggplot() +
  geom_jitter(aes(x = Difference, y = pi_Win, alpha = 0.5, size = 1.25), color="indianred",
              stat="identity", height =NULL, width = 0.25, data=prob_table) +
  xlab("Difference between team Initial Ranks ") +
  theme_bw() +
  scale_alpha(guide = 'none') +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks=brks) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits = c(0,1)) +
  ggtitle("Probability of Winning for \nDifferences between Initially Ranked Teams ")+
  theme(plot.title=element_text(hjust=0.5)) +
  facet_grid(Location ~ .)

ggplot() +
  geom_jitter(aes(x = Difference, y = pi_Win, alpha = 0.5, size = 1.25, color=Location), 
              stat="identity", height =NULL, width = 0.25, data=prob_table) +
  geom_hline(yintercept = 0.5, color = "gray") +
  xlab("Difference between team Initial Ranks ") +
  ylab(expression(pi[w])) +
  theme_bw() +
  scale_alpha(guide = 'none') +
  scale_size(guide = 'none') +
  scale_color_manual(values=c("indianred", "seagreen")) +
  scale_x_continuous(breaks=brks) +
  scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
  ggtitle("Probability of Winning for \nDifferences between Initially Ranked Teams")+
  theme(plot.title=element_text(hjust=0.5),
        panel.grid.major = element_blank()) 

# probability of drawing
ggplot() +
  geom_jitter(aes(x = Difference, y = pi_Draw, alpha = 0.5, size = 1.25, color = Location), 
              stat="identity", height =NULL, width = 0.25, data=prob_table) +
  geom_hline(yintercept = 0.5, color = "gray") +
  xlab("Difference between team Initial Ranks ") +
  ylab(expression(pi[d])) +
  theme_bw() +
  scale_alpha(guide = 'none') +
  scale_size(guide = 'none') +
  scale_color_manual(values=c("indianred", "seagreen")) +
  scale_x_continuous(breaks=brks) +
  scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
  ggtitle("Probability of Drawing for \nDifferences between Initially Ranked Teams")+
  theme(plot.title=element_text(hjust=0.5))

# probability of losing
ggplot() +
  geom_jitter(aes(x = Difference, y = pi_Loss, alpha = 0.5, size = 1.25, color = Location), 
              stat="identity", height =NULL, width = 0.25, data=prob_table) +
  geom_hline(yintercept = 0.5, color = "gray") +
  xlab("Difference between team Initial Ranks ") +
  ylab(expression(pi[l])) +
  theme_bw() +
  scale_alpha(guide = 'none') +
  scale_size(guide = 'none') +
  scale_color_manual(values=c("indianred", "seagreen")) +
  scale_x_continuous(breaks=brks) +
  scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
  ggtitle("Probability of Losing for \nDifferences between Initially Ranked Teams ")+
  theme(plot.title=element_text(hjust=0.5))


# probability of not losing
ggplot() +
  geom_jitter(aes(x = Difference, y = NoLose, alpha = 0.5, size = 1.25, color = Location), 
              stat="identity", height =NULL, width = 0.25, data=prob_table) +
  geom_hline(yintercept = 0.5, color = "gray") +
  xlab("Difference between team Initial Ranks ") +
  ylab(expression(pi)) +
  theme_bw() +
  scale_alpha(guide = 'none') +
  scale_size(guide = 'none') +
  scale_color_manual(values=c("indianred", "seagreen")) +
  scale_x_continuous(breaks=brks) +
  scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
  ggtitle("Probability of Not Losing for \nDifferences between Initially Ranked Teams ")+
  theme(plot.title=element_text(hjust=0.5))

levels(prob_table$Strata) <- c("Ranks 1 - 4", "Ranks 5 - 6", "Ranks 7 - 20")
probSum <- prob_table %>%
           group_by(Strata, Difference, Location) %>%
           summarize( NoLoseAvg = mean(NoLose), NoLoseSD = sd(NoLose),
                      winAvg = mean(pi_Win), winSD = sd(pi_Win),
                      drawAvg = mean(pi_Draw), drawSD = sd(pi_Draw))


# creates plot for probability of winning or drawing
noLose <- ggplot() +
  geom_jitter(aes(x = Difference, y = NoLose, color = Location),  alpha = 0.4, size = 3,
              stat="identity", height =NULL, width = 0.25, data=prob_table) +
  geom_smooth(aes(x = Difference, y = NoLoseAvg, color = Location), alpha =1, size = 1, se=FALSE,
             data=probSum) +
  geom_hline(yintercept = 0.5, color = "gray") +
  xlab("Difference between team Initial Ranks ") +
  ylab(expression(pi)) +
  theme_bw() +
  facet_wrap(~Strata, nrow = 1, scales = "free_x") +
  scale_alpha(guide = 'none') +
  scale_size(guide = 'none') +
  scale_color_manual(values=c("indianred", "seagreen")) +
  scale_x_continuous(breaks=brks) +
  scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
  ggtitle("Probability of Winning or Drawing for \nDifferences between Initially Ranked Teams ")+
  theme(plot.title=element_text(hjust=0.5),
        strip.background = element_blank())
noLose

prob_table$Location <- relevel(prob_table$Location, "Home")

win <- ggplot() +
  geom_jitter(aes(x = Difference, y = pi_Win, color = Location),  alpha = 0.4, size = 3,
              stat="identity", height =NULL, width = 0.25, data=prob_table) +
  geom_smooth(aes(x = Difference, y = winAvg, color = Location), alpha =1, size = 1, se=FALSE,
              data=probSum) +
  geom_hline(yintercept = 0.5, color = "gray") +
  xlab("Difference between team Initial Ranks ") +
  ylab("Probability of Winning") +
  theme_bw() +
  facet_wrap(~Strata, nrow = 1, scales = "free_x") +
  scale_alpha(guide = 'none') +
  scale_size(guide = 'none') +
  scale_color_manual(values=c("seagreen", "indianred")) +
  scale_x_continuous(breaks=seq(1,20,2)) +
  scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
  #ggtitle("Probability of Winning or Drawing for Differences between\n Initially Ranked Teams by Initial Ranking Strata")+
  theme(plot.title=element_text(hjust=0.5),
        strip.background = element_blank())
win

prob_table$Location <- relevel(prob_table$Location, "Home")
draw <- ggplot() +
  geom_jitter(aes(x = Difference, y = pi_Draw, color = Location),  alpha = 0.4, size = 3,
              stat="identity", height =NULL, width = 0.25, data=prob_table) +
  geom_smooth(aes(x = Difference, y = drawAvg, color = Location), alpha =1, size = 1, se=FALSE,
               data=probSum) +
  geom_hline(yintercept = 0.5, color = "gray") +
  xlab("Difference between team Initial Ranks ") +
  ylab("Probability of Drawing") +
  theme_bw() +
  facet_wrap(~Strata, nrow = 1, scales = "free_x") +
  scale_alpha(guide = 'none') +
  scale_size(guide = 'none') +
  scale_color_manual(values=c("seagreen","indianred")) +
  scale_x_continuous(breaks=seq(1,20,2)) +
  scale_y_continuous(breaks=seq(0,0.75,0.25), limits = c(0,0.75)) +
  #ggtitle("Probability of Winning or Drawing for \nDifferences between Initially Ranked Teams ")+
  theme(plot.title=element_text(hjust=0.5),
        strip.background = element_blank())
draw

grid.arrange(win, draw, ncol = 1)


# creates plot for probability of winning or drawing
prob1 <- prob_table %>% filter(Rank==1)
prob5 <- prob_table %>% filter(Rank==5)
prob10 <- prob_table %>% filter(Rank==10)

ggplot() +
  geom_line(aes(x = Difference, y = NoLose, group=factor(Rank)), 
            color = "gray", alpha = 0.5, data=prob_table) +
  geom_line(aes(x = Difference, y = NoLose, group=factor(Rank)),
            color = "seagreen", size = 1, data=prob1) +
  # geom_line(aes(x = Difference, y = NoLose, group=factor(Rank)), 
  #           color = "gold", size = 1, data=prob5) +
  # geom_line(aes(x = Difference, y = NoLose, group=factor(Rank)), 
  #           color = "indianred", size = 1, data=prob10) +
  xlab("Difference between team Initial Ranks ") +
  ylab(expression(pi)) +
  theme_bw() +
  scale_alpha(guide = 'none') +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks=brks) +
  scale_y_continuous(breaks=seq(0,1,0.25), limits = c(0,1)) +
  ggtitle("Probability of Winning or Drawing for \nDifferences between Initially Ranked Teams ")+
  theme(plot.title=element_text(hjust=0.5))



# playaround
rankings$colGame <- cut(rankings$Game.., breaks = c(0,6,12,19,26,32,38))

 


