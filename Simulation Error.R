library(dplyr)
library(tidyr)

# Simulation Results for 20 runs of 10,000
setwd("/Users/craigheard/Documents/Miami Graduate School/Masters Project/Simulation Results")
load("Simulation Cont Table DF Results 1-10.Rda")
load("Simulation Cont Table DF Results 11-20.Rda")

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

view <- main %>% select(Initial, Final, Mean, StdDev)
view$Stat <- paste(as.character(round(view$Mean/100,2)), " (", as.character(round(view$StdDev/100,2)), ")", sep = "")
view <- view %>% select(Initial, Final, Stat)
results2<- as.data.frame.matrix(xtabs(Stat ~ Initial + Final, view))
save(results2, file="Simulation Results (SD Include).RData")

write.csv(view, file="Mean & SD.csv")

res <- main %>% select(Initial, Final, Mean)


results<- round(as.data.frame.matrix(xtabs(Mean ~ Initial + Final, res)) /100,2)
save(results, file="Simulation Results (All).RData")

# save as CSV file
write.csv(results, file = "simResults.csv")

