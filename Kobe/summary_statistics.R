library(MASS)
library(class)
df <- read.csv("df.csv")
df <- df[, 2:length(df[1, ])]
na.shot <- df[is.na(df$shot_made_flag), ]
df <- na.omit(df)
df$season         <- as.factor(df$season)
df$game_date      <- as.factor(df$game_date)
df$game_id        <- as.factor(df$game_id)
df$three_pt       <- as.factor(df$three_pt)
df$ot             <- as.factor(df$ot)
df$home           <- as.factor(df$home)
df$playoffs       <- as.factor(df$playoffs)
df$opponent       <- as.factor(df$opponent)
df$shot_made_flag <- as.factor(df$shot_made_flag)
summary(df)


# draw a picture colored with the pointed type
three_pt_col <- ifelse(df$three_pt==1, "darkviolet", "gold")
par(mfrow = c(1, 3))
three_pt_plot <- plot(df$loc_x, df$loc_y,
                      xlab = "", ylab = "", main = "All the shots",
                      xaxt = "n", yaxt = "n", col = three_pt_col, 
                      xlim = range(df$loc_x), ylim = range(df$loc_y),
                      pch = 16, asp = 1)
three_pt_plot_0 <- plot(df$loc_x[df$shot_made_flag==0], df$loc_y[df$shot_made_flag==0], 
                      xlab = "", ylab = "", main = " All the failure shots ", 
                      xaxt = "n", yaxt = "n", col = three_pt_col[df$shot_made_flag==0], 
                      xlim = range(df$loc_x), ylim = range(df$loc_y), 
                      pch = 16, asp = 1)
three_pt_plot_1 <- plot(df$loc_x[df$shot_made_flag==1], df$loc_y[df$shot_made_flag==1], 
                      xlab = "", ylab = "", main = " All the made shots ", 
                      xaxt = "n", yaxt = "n", col = three_pt_col[df$shot_made_flag==1], 
                      xlim = range(df$loc_x), ylim = range(df$loc_y), 
                      pch = 16, asp = 1)
legend(x =30,y =range(df$loc_y)[2]-10, legend = c("2 points", "3 points"), 
       pch=16, col = c( "gold", "darkviolet"), bg = gray(0.95), box.lty = 0)


# draw the KNN
library(class)
knn.plot.y.k <- 
  function(y, m){
    loc_train  <- cbind(df$loc_x[df$season==y], df$loc_y[df$season==y])
    loc_test   <- cbind(     rep(seq(-250, 250, 5), length(seq(-50 , 800, 5))) ,
                             sort(rep(seq(-50 , 800, 5), length(seq(-250, 250, 5)))) )
    shot_train <- df$shot_made_flag[df$season==y]
    season_shot_loc_knn <- knn(train = loc_train,test = loc_test, cl = shot_train, k = m, prob = TRUE)
    prob <- attr(season_shot_loc_knn, "prob")
    prob <- ifelse(season_shot_loc_knn=="1", prob, 1-prob)
    px_x <- seq(-250, 250, 5)
    px_y <- seq(-50 , 800, 5)
    prob_knn <- matrix(prob, length(px_x), length(px_y))
    contour(px_x, px_y, prob_knn, levels=0.5, labels="",  axes=FALSE, 
            asp =1,
            xlab=paste0(m, "-nearest neighbour"), ylab="", main = y)
    points(loc_train, col=ifelse(df$shot_made_flag[df$season==y]==1, "darkviolet", "gold"), pch = 16)
    gd <- expand.grid(x=px_x, y=px_y)
    points(gd, pch=".", cex=1.2, col=ifelse(prob_knn>0.5, "darkviolet", "gold"))
    box()
  }
par(mfrow = c(1,4))
for(y in c(1996, 2001, 2010, 2015)){
  knn.plot.y.k(y, 21)
}
par(mfrow = c(1,1))


# campare the choise of K
par(mfrow = c(1,4))
for(k in c(1,7,21,49)){
  knn.plot.y.k(2010, k)
}
par(mfrow = c(1,1))



# draw the annual jump shot location
six_type_shoot <- levels(df1$combined_shot_type)
par(mfrow = c(2, 3))
for(s in 1:6){
annual_jumpshot_loc <- sapply(1996:2015, function(y) {
                              cbind(mean(df1$loc_x[df1$season==y & df1$combined_shot_type == six_type_shoot[s]], na.rm = T), 
                                    mean(df1$loc_y[df1$season==y & df1$combined_shot_type == six_type_shoot[s]], na.rm = T) );
                       })
annual_jumpshot_loc <- as.data.frame(t(annual_jumpshot_loc))
names(annual_jumpshot_loc)    <- c("loc_x", "loc_y")
rownames(annual_jumpshot_loc) <- c(1996:2015)
rx <- max(range(na.omit(annual_jumpshot_loc$loc_x)))
ry <- max(range(na.omit(annual_jumpshot_loc$loc_y)))
plot(annual_jumpshot_loc,
     xlim = range(na.omit(df1$loc_x)) ,
     ylim = range(na.omit(df1$loc_y)) , 
     main = "Annual shooting location", xlab =six_type_shoot[s], ylab = "", 
     pch = 16, cex = 1+(0.1)*(1996:2015-1996), col = c(gray((1/30)*(2020 - 1996:2015))), 
     type = "b", asp = 1)
     abline(h = 0, v = 0, col = "gold")
     abline(h = range(na.omit(df1$loc_y)), v = range(na.omit(df1$loc_x)), col = "darkviolet")
}

par(mfrow = c(1, 3))
for(s in c(1,4,5)){
  annual_jumpshot_loc <- sapply(1996:2015, function(y) {
    cbind(mean(df1$loc_x[df1$season==y & df1$combined_shot_type == six_type_shoot[s]], na.rm = T), 
          mean(df1$loc_y[df1$season==y & df1$combined_shot_type == six_type_shoot[s]], na.rm = T) );
  })
  annual_jumpshot_loc <- as.data.frame(t(annual_jumpshot_loc))
  names(annual_jumpshot_loc)    <- c("loc_x", "loc_y")
  rownames(annual_jumpshot_loc) <- c(1996:2015)
  plot(annual_jumpshot_loc,
       main = "Annual shooting location", xlab =six_type_shoot[s], ylab = "", 
       pch = 16, cex = 1+(0.1)*(1996:2015-1996), col = c(gray((1/30)*(2020 - 1996:2015))), 
       type = "b", asp = 1)
  abline(h = 0, v = 0, col = "gold")
  abline(h = range(na.omit(df1$loc_y)), v = range(na.omit(df1$loc_x)), col = "darkviolet")
  
}
par(mfrow = c(1,1))

