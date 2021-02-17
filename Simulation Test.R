# PL Sim

# create test table
p.table <- rbind( c(1,2,1,0.2,0.4,0.4),
                  c(1,2,2,0.3,0.5,0.2),
                  c(1,3,1,0.1,0.2,0.7),
                  c(1,3,2,0.2,0.4,0.3),
                  c(2,3,1,0.2,0.5,0.3),
                  c(2,3,2,0.4,0.4,0.2))

gd.tab <- rbind(c(1, 10, 5, 8 , 4),
                c(2, 0, 3, -3, -1),
                c(3, -10, -8, -5, -3))

# change col names
colnames(p.table) <- c("i", "j", "k", "pi_loss", "pi_draw", "pi_win")

# outcome table
sim_table <- as.data.frame(p.table) %>% select(i,j,k)
sim_table$points <- NA

# simulation

nseasons <-1
season <- matrix(nrow=nseasons*6,ncol=6)
colnames(season) <- c("i", "j", "k", "u", "pts_i", "pts_j")

nsim <- nseasons*6

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

## Multiple seasons
nseasons <-4
seasons <- matrix(nrow=nseasons*6,ncol=7)
colnames(seasons) <- c("season", "i", "j","k","u","pts_i","pts_j")

nsim <- nseasons*6 # 12 total observations

# loop over games
for (igame in 1:nsim) {
  
  season <- ceiling(igame/6)
  seasons[igame,1] <- season
  
  mod_repeat <- ifelse(igame %% 6 == 0, 6, igame %% 6)
  team_i <- p.table[mod_repeat,1]
  seasons[igame,2] <- team_i # put team i in matrix
  
  team_j <- p.table[mod_repeat,2]
  seasons[igame,3] <- team_j # put team j in matrix
  
  home  <- p.table[mod_repeat,3]
  seasons[igame,4] <- home # put location in matrix
  
  # pt.row <- ifelse(igame %% 6 == 0, 6, igame %% 6) needed to repeaet multiple seasons
  pt.mat <- as.matrix(p.table[mod_repeat, 4:6])
  
  cut1 <- pt.mat[1,]
  cut2 <- cut1 + pt.mat[2,]
  
  u <- runif(1,0,1)
  seasons[igame,5] <- u # put u in matrix
  
  seasons[igame,6] <-  ifelse(u < cut1, 0,  ifelse( u < cut2, 1, 3))
  
  seasons[igame,7] <-  ifelse(seasons[igame,6] == 0, 3,  ifelse(seasons[igame,6] == 1, 1, 0))
  
}

seasons <- as.data.frame(seasons)
# total points for higher ranked team 
test.hr <- seasons %>% group_by(season, i) %>%
  summarize(total_hr_points = sum(pts_i, na.rm = TRUE))

# total points for lower ranked team 
test.lr <- seasons %>% group_by(season, j) %>%
  summarize(total_lr_points = sum(pts_j, na.rm = TRUE))

colnames(test.lr)[2] <- "i"

# total points for each team
season.table <- merge(test.hr, test.lr, by=c("season","i"), all= TRUE) %>%
  rowwise() %>%
  mutate(total_points = sum(total_hr_points, total_lr_points, na.rm = TRUE))


# assign goal difference to teams.
mat.season <- as.matrix(season.table)
teams <- 3
iterations <- teams*nseasons
for(diff in 1:iterations){
  
  # this variable is created to refer the observation to correct row in the gd table
  mod_repeat <- ifelse(diff %% 3 == 0, 3, diff %% 3)
  season.table$mod <- mod_repeat
  # subsets only the values for goal diffence
  gd.values <- gd.tab[,-1]
  
  season.table$GD[diff] <- sample(gd.values[mod_repeat,], 1) 
  
}

# creates the final rankings of teams by season, ranking on total points first and then GD
sorted <- season.table %>%
          select(season, i, total_points, GD) %>%
          arrange(season, -total_points, -GD) %>%
          group_by(season) %>%
          mutate(final = row_number())

# contingency table to show
cont <- table(sorted$i, sorted$final)

prop.table(cont, 1) 
  
