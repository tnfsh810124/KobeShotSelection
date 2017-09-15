df <- read.csv("df.csv")
df <- df[, 2:length(df[1, ])]
na.shot <- df[is.na(df$shot_made_flag), ]
df <- na.omit(df)
attach(df)

home_match <- df[df$home == 1,]
away_match <- df[df$home == 0,]
home_smf_per_game = aggregate(home_match$shot_made_flag, by = list("home_match_game_id" = home_match$game_id), FUN = mean)
away_smf_per_game = aggregate(away_match$shot_made_flag, by = list("away_match_game_id" = away_match$game_id), FUN = mean)
var.test(home_smf_per_game$x, away_smf_per_game$x)
t_test <- t.test(home_smf_per_game$x, away_smf_per_game$x, var.equal = TRUE, paired = FALSE)
boxplot(home_smf_per_game$x, away_smf_per_game$x, 
        xaxt="n", main = "Shot-made ratio comparison", 
        sub = paste0("p-value = ", t_test$p.value), 
        col = c( "gold", "darkviolet"), border = gray(0.6), 
        pch = "*", boxwex = 0.5)
axis(1, 1:2, c("Home", "Away"))
detach(df)
