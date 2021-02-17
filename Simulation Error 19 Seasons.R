#libraries
library(tidyverse)

# Simulation Results for 20 runs of 10,000
setwd("~/Documents/University/Miami Graduate School/Masters Project/Simulation Results (wo 2015-16)")
load("Simulation Contingency Table (without 2015-16)1-20.Rda")


cont.df <- countsToCases(as.data.frame(cont.df))


main <- cbind(cont.df1,cont.df2[,3],cont.df3[,3],cont.df4[,3],cont.df5[,3],
              cont.df6[,3],cont.df7[,3],cont.df8[,3],cont.df9[,3],cont.df10[,3],
              cont.df11[,3],cont.df12[,3],cont.df13[,3],cont.df14[,3],cont.df15[,3],
              cont.df16[,3],cont.df17[,3],cont.df18[,3],cont.df19[,3],cont.df20[,3])


colnames(main) <- c("Initial", "Final", "T1", "T2", "T3", "T4", "T5",
                    "T6", "T7", "T8", "T9", "T10", "T11", "T12", "T13", "T14", "T15",
                    "T16", "T17", "T18", "T19", "T20")
main$Mean <- rowMeans(main[,3:22], na.rm=TRUE)
main$StdDev <- apply(main[,3:22], 1, sd )
main$StdErr <- main$StdDev/sqrt(20)
main$Sum <- rowSums(main[,3:22], na.rm=TRUE)

view <- main %>% select(Initial, Final, Mean, StdDev)
view$Stat <- paste(as.character(round(view$Mean/100,2)), " (", as.character(round(view$StdDev/100,2)), ")", sep = "")
view <- view %>% select(Initial, Final, Stat)
results2 <- as.data.frame.matrix(xtabs(Stat ~ Initial + Final, view))
results3 <- as.data.frame.matrix(view)

avgResults <- main %>% select(Initial, Final, Mean)
results2 <- as.data.frame.matrix(xtabs(Mean ~ Initial + Final, avgResults))

save(results2, file="Simulation Results (SD Include).RData")

write.csv(view, file="Mean & SD.csv")

# confidence intervals across all 20 simulations
pLC <- main[14,23]
lower_lcEstimate <- main[14,23] - 1.96*main[14,25]
upper_lcEstimate <- main[14,23] + 1.96*main[14,25]
c(lower_lcEstimate,upper_lcEstimate)

lower_chelsEstimate <- main[181,23] - 1.96*main[181,25]
upper_chelsEstimate <- main[181,23] + 1.96*main[181,25]
c(lower_chelsEstimate,upper_chelsEstimate)

lower_714Estimate <- sum(main[7:20,23]) - 1.96*mean(main[7:20,25])
upper_714cEstimate <- sum(main[7:20,23]) + 1.96*mean(main[7:20,25])
c(lower_714Estimate,upper_714cEstimate)

res <- main %>% select(Initial, Final, Mean)

results<- round(as.data.frame.matrix(xtabs(Mean ~ Initial + Final, res)) /100,2)
save(results, file="Mean Simulation Results (All).RData")

# save as CSV file
write.csv(results, file = "simResults.csv")


#################################################
################## Heat Map #####################
#################################################

tables <- read.csv("~/Documents/University/Miami Graduate School/Masters Project/Data/PL Tables.csv")
brks <-seq(1:20)

# original contingency table of initial ranking vs. final ranking
cont.original <- as.data.frame.matrix(prop.table(table(tables$Initial_Rank, tables$Finishing_Pos),1)*100)


# cont table for simulation results
sim.prop.table <- as.data.frame.matrix(xtabs(Mean ~ Initial + Final, res)/100)

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
  scale_fill_gradient2(low = "darkred", mid = "white", high = "steelblue", "Difference (%)") +
  xlab("Inital Ranking") +
  scale_x_continuous(breaks=brks) +
  ylab("Final Ranking") +
  scale_y_reverse( lim=c(20,0), breaks=brks) +
  theme_bw()

#######################################################
################## Tableau Export #####################
#######################################################
tables.rmlei <- tables %>% filter(Season != "2016/2017" 
                                  #& Season != "2015/2016" 
                                  )


#Empiral Data
empirical.raw <- as.data.frame.matrix(table(tables.rmlei$Initial_Rank, tables.rmlei$Finishing_Pos),1)
empirical.prob <- as.data.frame.matrix(prop.table(table(tables.rmlei$Initial_Rank, tables.rmlei$Finishing_Pos),1)*100)

empirical.raw$Initial<-seq.int(nrow(empirical.raw))
# turn cont table into format so that a heat map can be created.
empirical.raw.final <- empirical.raw %>%
  gather(Final, EmpRaw, 1:20)
empirical.raw.final$Final <- as.numeric(cont.original2$Final)

empirical.prob$Initial<-seq.int(nrow(empirical.prob))
# turn cont table into format so that a heat map can be created.
empirical.prob.final <- empirical.prob %>%
  gather(Final, EmpProb, 1:20)
empirical.prob.final$Final <- as.numeric(cont.original2$Final)

#join raw and prob data
emprical.final <- merge(x = empirical.raw.final, y = empirical.prob.final, by=c("Initial","Final"), all.x = TRUE)

# Add Simulation Data
tableau.final <- merge(x = emprical.final, y = main, by=c("Initial","Final"), all.x = TRUE)

write_csv(tableau.final, "Empirical + Simulation Results.csv")
