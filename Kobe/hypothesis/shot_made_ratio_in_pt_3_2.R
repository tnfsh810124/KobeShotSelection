df <- read.csv("df.csv")
df <- df[, 2:length(df[1, ])]
na.shot <- df[is.na(df$shot_made_flag), ]
df <- na.omit(df)
attach(df)

pt_3 <- df[df$three_pt == 1,]
pt_2 <- df[df$three_pt == 0,]
pt_3_smf_per_game = aggregate(pt_3$shot_made_flag, by = list("game_id" = pt_3$game_id), FUN = mean)
pt_2_smf_per_game = aggregate(pt_2$shot_made_flag, by = list("game_id" = pt_2$game_id), FUN = mean)
pt_3_2 <- merge(x = pt_3_smf_per_game, y = pt_2_smf_per_game, by.x = 'game_id', by.y = 'game_id')
colnames(pt_3_2) <- c("game_id", "three_point_shot", "two_point_shot")
var.test(pt_3_2$three_point_shot, pt_3_2$two_point_shot)
t_test <- t.test(pt_3_2$three_point_shot, pt_3_2$two_point_shot, var.equal = FALSE, paired = TRUE)
boxplot(pt_3_2$two_point_shot, pt_3_2$three_point_shot, 
        xaxt="n", main = "Shot-made ratio comparison", 
        sub = paste0("p-value = ", t_test$p.value), 
        col = c( "gold", "darkviolet"), border = gray(0.6), 
        pch = "*", boxwex = 0.5)
axis(1, 1:2, c("2 points", "3 points"))
detach(df)
