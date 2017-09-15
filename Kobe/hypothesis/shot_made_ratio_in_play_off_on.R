df <- read.csv("df.csv")
df <- df[, 2:length(df[1, ])]
na.shot <- df[is.na(df$shot_made_flag), ]
df <- na.omit(df)
attach(df)

play_off <- df[df$playoffs == 1,]
play_on <- df[df$playoffs == 0,]
play_off_smf_per_game = aggregate(play_off$shot_made_flag, by = list("play_off_game_id" = play_off$game_id), FUN = mean)
play_on_smf_per_game = aggregate(play_on$shot_made_flag, by = list("play_on_game_id" = play_on$game_id), FUN = mean)
var.test(play_off_smf_per_game$x, play_on_smf_per_game$x)
t_test <- t.test(play_off_smf_per_game$x, play_on_smf_per_game$x, var.equal = TRUE, paired = FALSE)
boxplot(play_on_smf_per_game$x,play_off_smf_per_game$x,  
        xaxt="n", main = "Shot-made ratio comparison", 
        sub = paste0("p-value = ", t_test$p.value), 
        col = c( "gold", "darkviolet"), border = gray(0.6), 
        pch = "*", boxwex = 0.5)
axis(1, 1:2, c( "Regular games", "Playoffs"))
