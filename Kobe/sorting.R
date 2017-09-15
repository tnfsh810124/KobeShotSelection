library(MASS)

Kobe <- read.csv("Kobe.csv", header = T, sep = ",")
Kobe$matchup <- as.character(Kobe$matchup)
Kobe$matchup <- sapply(1:length(Kobe$matchup), 
       function(x) strsplit(Kobe$matchup[x], " ")[[1]][2])
home <- as.factor(sapply(1:length(Kobe$matchup), function(x) ifelse(Kobe$matchup[x]=="vs.", 1, 0)))
attach(Kobe)
OT <- as.factor(sapply(1:length(period), function(x){ifelse(period[x] >4 , 1, 0)}))
time_remaining <- sapply(1:length(period), function(x){
  ifelse(period[x] >4, 
         minutes_remaining[x]*60+seconds_remaining[x],
         ((4-period[x])*12+minutes_remaining[x])*60+seconds_remaining[x]  )
})
three_pt <- as.factor(sapply(1:length(shot_type), function(x)ifelse(shot_type[x]=="3PT Field Goal", 1, 0)))
season <- as.numeric(sapply(1:length(season), function(x) strsplit(as.character(season[x]), "-")[[1]][1]))
game_date <- as.character(Kobe$game_date)


df <- cbind.data.frame(season, as.factor(game_date), as.factor(Kobe$game_id), Kobe$action_type, Kobe$combined_shot_type, Kobe$loc_x, Kobe$loc_y, Kobe$shot_distance, three_pt, 
              time_remaining, OT, home, as.factor(Kobe$playoffs), as.factor(Kobe$opponent), as.factor(Kobe$shot_made_flag)
              )
names(df) <- c("season", "game_date", "game_id","action_type", "combined_shot_type", "loc_x", "loc_y", "shot_distance", "three_pt", 
               "time_remaining", "ot", "home", "playoffs", "opponent", "shot_made_flag")
df$three_pt[df$loc_y> 400 & df$three_pt==0] <- 1
df$three_pt[df$loc_y %in% c(0, 100) & df$loc_x %in% c(-100, 100) & df$three_pt==1] <- 0
write.csv(df, "df.csv")






# Logistic under 
fit1 <- glm(shot_made_flag ~ combined_shot_type + factor(playoffs) + 
              minutes_remaining + period, family = "binomial")
summary(fit1)
prdct1 <- cut(predict(fit1, df, "response"), c(0,0.5,1), c(0, 1))
prdct.tb1 <- table(shot_made_flag , prdct1)
(prdct.tb1[1, 2] + prdct.tb1[2, 1])/sum(prdct.tb1)


fit2 <- lda(shot_made_flag ~ combined_shot_type + factor(playoffs) + 
              minutes_remaining + period, subset = is.na(Kobe$shot_made_flag)==FALSE)
prdct2 <- predict(fit2, df)$class
prdct.tb2 <- table(df$shot_made_flag , prdct2)
(prdct.tb2[1, 2] + prdct.tb2[2, 1])/sum(prdct.tb2)


fit3 <- qda(shot_made_flag ~ combined_shot_type + factor(playoffs) + 
              minutes_remaining + period, subset = is.na(Kobe$shot_made_flag)==FALSE)
prdct3 <- predict(fit3, df)$class
prdct.tb3 <- table(df$shot_made_flag , prdct3)
(prdct.tb3[1, 2] + prdct.tb3[2, 1])/sum(prdct.tb3)


detach(Kobe)

