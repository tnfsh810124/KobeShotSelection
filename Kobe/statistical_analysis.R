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
df1 <- df[, -c(2, 3, 4)]  # Useless predictors: game_date, game_id, action_type

library(caret)
# cross-validation preparing
k.folds <- createFolds(1:25697, k = 20)
# To avoid that k-folds are not consist in different analysis

##############################################################################
## KNN Classifier ############################################################
##############################################################################
# write a KNN classifier function for a period from year 1 to year 2 #
library(class)
cv.knn.err.TP.FP <- function(y1,y2){
  df2 <- data.frame(df1$loc_x[df1$season %in% c(y1:y2)], 
                    df1$loc_y[df1$season %in% c(y1:y2)])
  twenty.folds <- createFolds(1:sum(df1$season %in% c(y1:y2)), k = 20)
  cv.err.tmp.knn<- 
    sapply(1:20, function(k){
      rsp <- knn(train = df2[-twenty.folds[[k]], ], 
                 test  = df2[ twenty.folds[[k]], ], 
                 cl = df1$shot_made_flag[df1$season %in% c(y1:y2)][-twenty.folds[[k]]],
                 k  = round(sum(df1$season %in% c(y1:y2))/100), prob = TRUE);
      sapply(seq(0,1, 0.05)[-c(1, 21)], function(c) {
        prd <- ifelse(attr(rsp,"prob") >= c, 1, 0)
        tru <- df1$shot_made_flag[df1$season %in% c(y1:y2)][ twenty.folds[[k]]]
        c(mean(prd != tru), sum(prd == 1 & tru == 1)/sum(tru == 1), 
          sum(prd == 1 & tru == 0)/sum(tru == 0))
      })
    })
  cv.err.rate.knn <-  
    sapply(c(0:18)*3+1, function(c) {
      cv.err.tmp.knn[c, ]
    });
  cv.TP.knn <- 
    sapply(c(0:18)*3+2, function(c) {
      cv.err.tmp.knn[c, ]
    });
  cv.FP.knn <- 
    sapply(c(0:18)*3+3, function(c) {
      cv.err.tmp.knn[c, ]
    });
  err.TP.FP <- as.data.frame(cbind(colMeans(cv.err.rate.knn), 
                                   colMeans(cv.TP.knn), 
                                   colMeans(cv.FP.knn)));
  names(err.TP.FP) <- c("err.rate", "TP", "FP");
  row.names(err.TP.FP) <- seq(0,1, 0.05)[-c(1, 21)];
  err.TP.FP.cv <- cbind(err.TP.FP, t(cv.err.rate.knn))
  names(err.TP.FP.cv)[4:23] <- paste("fold", 1:20);
  err.TP.FP.cv
}

par(mfrow = c(2, 4))
for(y in c(1996, 2001, 2010, 2015)){
  cv.knn.year <- cv.knn.err.TP.FP(y, y)
  plot(cv.knn.year$err.rate, col = 0, 
       xlab = "cut-off points", ylab = "error rates", 
       main = paste0("Cut-off Point Selection for KNN (", y, ")"), 
       xaxt = "n", ylim = c(0, 1))
  axis(1, at = 1:19, labels = seq(0,1, 0.05)[-c(1, 21)])
  abline(v = which.min(cv.knn.year$err.rate), lty = 4)
  for(k in 1:20){
    points(1:19, cv.knn.year[, k+3], col = gray(0.66), type = "l", lwd = .8)
  }
  points(1:19, cv.knn.year$err.rate, col = "red", type = "l", lwd = 4)
  text(x = which.min(cv.knn.year$err.rate), y = min(cv.knn.year$err.rate),
       labels = paste0("min = ", round(min(cv.knn.year$err.rate), 3))
       , pos = 2, cex = 1.5, col = "darkblue")
  plot( cv.knn.year$TP ~ cv.knn.year$FP, type = "l", asp = 1, col = "gold", lwd = 2, 
        xlab = "False Positive Rate", ylab = "True Positive Rate", 
        main = paste0("ROC Curve for KNN (", y, ")"))
  abline(0, 1, lty = 3)
}

##############################################################################
## Model Selection ###########################################################
##############################################################################
library(leaps)
library(car)

## Forward Selection #########################################################
mod.selec.fwd <- regsubsets(model.matrix(df1$shot_made_flag ~. , data = df1),
                            df1$shot_made_flag, method = "forward", nvmax = 11,
                            family = "binomial")
mod.summary.fwd <- summary(mod.selec.fwd)
which.min(mod.summary.fwd$cp)
which.min(mod.summary.fwd$bic)
which.max(mod.summary.fwd$adjr2)
# the final foward selection
mod.selec.fwd2=step(glm(df1$shot_made_flag ~. ,data = df1,family = binomial)
                      ,direction = "forward")
vif(mod.selec.fwd2)  # colinearity detection

## Backward Selection ########################################################
mod.selec.bwd <- regsubsets(model.matrix(df1$shot_made_flag ~. , data = df1),
                            df1$shot_made_flag, method = "backward", nvmax = 11,
                            family = "binomial")
mod.summary.bwd <- summary(mod.selec.bwd)
which.min(mod.summary.bwd$cp)
which.min(mod.summary.bwd$bic)
which.max(mod.summary.bwd$adjr2)
# the final backward selection
mod.selec.bwd2 <- step(glm(df1$shot_made_flag ~. , data = df1,family = binomial)
                      ,direction = "backward")
vif(mod.selec.bwd2) # colinearity detection

## Both Selection ############################################################
mod.selec.bth <- regsubsets(model.matrix(df1$shot_made_flag ~. , data = df1),
                            df1$shot_made_flag, method = "seqrep", nvmax = 11,
                            family = "binomial")
mod.summary.bth <- summary(mod.selec.bth)
which.min(mod.summary.bth$cp)
which.min(mod.summary.bth$bic)
which.max(mod.summary.bth$adjr2)
# the final both selection

mod.selec.bth2 <- step(glm(shot_made_flag ~. , data = df1,family = binomial)
                         ,direction = "both")
# colinearity detection
vif(mod.selec.bth2) 
colinear.bth2 <- which(vif(mod.selec.bth2)[, 3] > 1.5)
colinear.in.df1 <- which(names(df1) %in% names(colinear.bth2))

# leave one in the model and choose the lowest AIC
leave.one.in.AIC <- 
sapply(colinear.in.df1, function(v) {
  mod.selec.bth3 <- step(glm(df1$shot_made_flag ~. , 
                               data = df1[, -colinear.in.df1[which(colinear.in.df1 != v)]],
                               family = binomial), direction = "both");
  mod.selec.bth3$aic
})
leave.one.in.AIC <- as.data.frame(t(leave.one.in.AIC))
names(leave.one.in.AIC) <- names(df1)[colinear.in.df1]
names(df1)[which.min(leave.one.in.AIC)]
mod.selec.bth3 <- step(glm(df1$shot_made_flag ~. , 
                             data = df1[, -colinear.in.df1[which(colinear.in.df1 != 5)]],
                             family = binomial), direction = "both")
as.matrix(mod.selec.bth3$coefficients)
fmla.bth3 <- as.formula(shot_made_flag ~ season + combined_shot_type + 
                          shot_distance + time_remaining + home)
vif(mod.selec.bth3)
# choose the best cut off point by cross validation for "both"
cv.err.tmp.bth3 <- 
  sapply(1:20, function(k) {
    train <- df1[-k.folds[[k]], ]
    test  <- df1[ k.folds[[k]], ]
    fit <- glm(formula = fmla.bth3, family = binomial,  data = train)  
    rsp <- predict(fit, newdata = test, type = "response")
    sapply(seq(0,1, 0.05)[-c(1, 21)], function(c) {
      prd <- ifelse(rsp >= c, 1, 0)
      tru <- df1$shot_made_flag[k.folds[[k]]]
      c(mean(prd != tru), sum(prd == 1 & tru == 1)/sum(tru == 1), 
            sum(prd == 1 & tru == 0)/sum(tru == 0))
    })
  })
cv.err.rate.bth3 <-  
  sapply(c(0:18)*3+1, function(c) {
    cv.err.tmp.bth3[c, ]
  })
cv.TP.bth3 <- 
  sapply(c(0:18)*3+2, function(c) {
    cv.err.tmp.bth3[c, ]
  })
cv.FP.bth3 <- 
  sapply(c(0:18)*3+3, function(c) {
    cv.err.tmp.bth3[c, ]
  })

cut.mean.err.bth3 <- colMeans(cv.err.rate.bth3)

par(mfrow = c(1, 2))  
plot(cut.mean.err.bth3, col = "red", type = "l", lwd = 1.5, 
     xlab = "cut-off points", ylab = "error rates", main = "Cut-off Point Selection for Mixed Method", 
     xaxt = "n", ylim = c(0.35, 0.6))
for(k in 1:20){
  points(1:19, cv.err.rate.bth3[k,], col = gray(0.66), type = "l", lwd = .8)
}
axis(1, at = 1:19, labels = seq(0,1, 0.05)[-c(1, 21)])
abline(v = which.min(cut.mean.err.bth3), lty = 4)
points(1:19, cut.mean.err.bth3, col = "red", type = "l", lwd = 4)
text(x = which.min(cut.mean.err.bth3), y = min(cut.mean.err.bth3),
     labels = paste0("min = ", round(min(cut.mean.err.bth3), 3))
     , pos = 1, cex = 1.5, col = "darkblue")

## ROC curve for both
# True Positive Rate
cut.mean.TP.bth3 <- colMeans(cv.TP.bth3)
# False Positive Rate
cut.mean.FP.bth3 <- colMeans(cv.FP.bth3)

plot(cut.mean.TP.bth3 ~ cut.mean.FP.bth3, type = "l", asp = 1, col = "darkviolet", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curve for Mixed Method")
abline(0, 1, lty = 3)

## Lasso Selection ###########################################################
library(glmnet)

# redo lasso with all observation ########
df2 <- df1[, -c(4, 6)]
xmat=model.matrix(shot_made_flag~.,data=df2)[,-1]
mod.lasso=cv.glmnet(xmat,df2$shot_made_flag,alpha=1,family="binomial")
best.lambda=mod.lasso$lambda.min
best.lambda
plot(mod.lasso)
round(coef(mod.lasso), 5)

cv.err.tmp.lasso <- 
  sapply(1:20,function(k){
   train <- df2[-k.folds[[k]],]
   test  <- df2[ k.folds[[k]],]
   xmat  <- model.matrix(train$shot_made_flag ~ 
                           combined_shot_type + shot_distance +
                           time_remaining,data=train)[,-1]
   mod.lasso <- cv.glmnet(xmat,train$shot_made_flag,alpha=1,family="binomial")
   best.lambda <- mod.lasso$lambda.min
   best.model <- glmnet(xmat,train$shot_made_flag,alpha=1,family="binomial")
   test.xmat <- model.matrix(test$shot_made_flag ~ 
                               combined_shot_type + shot_distance +
                               time_remaining,data=test)[,-1]
   lasso.prob <- predict(best.model,newx=test.xmat,s=best.lambda,type="response")
   sapply(seq(0, 1, 0.05)[-c(1, 21)], function(c){
    prd <- ifelse(lasso.prob >= c, 1, 0)
    tru <- test$shot_made_flag
    c(mean(prd != tru), sum(prd == 1 & tru == 1)/sum(tru == 1), 
      sum(prd == 1 & tru == 0)/sum(tru == 0))
    })
  })
cv.err.rate.lasso <-  
  sapply(c(0:18)*3+1, function(c) {
    cv.err.tmp.lasso[c, ]
  })
cv.TP.lasso <- 
  sapply(c(0:18)*3+2, function(c) {
    cv.err.tmp.lasso[c, ]
  })
cv.FP.lasso <- 
  sapply(c(0:18)*3+3, function(c) {
    cv.err.tmp.lasso[c, ]
  })
cut.mean.err.lasso <- sapply(1:19, function(c) mean(cv.err.rate.lasso[, c]))

plot(cut.mean.err.lasso, col = "red", type = "l", lwd = 1.5, 
     xlab = "cut-off points", ylab = "error rates", main = "Cut-off Point Selection for Lasso", 
     xaxt = "n", ylim = c(0.35, 0.6))
for(k in 1:20){
  points(1:19, cv.err.rate.lasso[k,], col = gray(0.66), type = "l", lwd = .8)
}
axis(1, at = 1:19, labels = seq(0,1, 0.05)[-c(1, 21)])
abline(v = which.min(cut.mean.err.lasso), lty = 4)
points(1:19, cut.mean.err.lasso, col = "red", type = "l", lwd = 4)
text(x = which.min(cut.mean.err.lasso), y = min(cut.mean.err.lasso),
     labels = paste0("min = ", round(min(cut.mean.err.lasso), 3))
     , pos = 1, cex = 1.5, col = "darkblue")

## ROC curve for lasso
# True Positive Rate
cut.mean.TP.lasso <- sapply(1:19, function(c) mean(cv.TP.lasso[, c]))
# False Positive Rate
cut.mean.FP.lasso <- sapply(1:19, function(c) mean(cv.FP.lasso[, c]))
plot(cut.mean.TP.lasso ~ cut.mean.FP.lasso, type = "l", asp = 1, col = "gold", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curve for Lasso")
abline(0, 1, lty = 3)

##############################################################################
## Random Forest #############################################################
##############################################################################
library(randomForest)
fit.random.trees <- randomForest(shot_made_flag ~. , data = df1,
                                 mtry = 20, ntree = 500)
mean(predict(fit.random.trees, data = df1) == df1$shot_made_flag)
##  It takes more than one hour to run!  ##
#try.mean.err.RF <- 
#  sapply(1:20, function(k) {
#    train <- df1[-k.folds[[k]], ]
#    test  <- df1[ k.folds[[k]], ]
#    sapply(1:11, function(m) {
#      fit   <- randomForest(shot_made_flag ~. , data = train,
#                            mtry = m, ntree = 500)
#      prd   <- predict(fit, newdata = test)
#      tru   <- test$shot_made_flag
#      mean(prd != tru)
#    })
#  })
##  Result on the server  ##
try.mean.err.RF <- read.csv("try.mean.err.RF.csv", header = T, sep = ",")[, -1]
plot(colMeans(t(try.mean.err.RF)), type = "l", col = "darkviolet", lwd = 4, 
     main = "Cross-validation Error for Random Forest", 
     xlab = "number of the predictors considered for every tree", ylab = "test error rate")
text(which.min(colMeans(t(try.mean.err.RF))), 
               min(colMeans(t(try.mean.err.RF))), 
     labels = min(colMeans(t(try.mean.err.RF))), 
     col = "darkblue", pos = 4)
#try.TP.RF <- 
#  sapply(1:20, function(k) {
#    train <- df1[-k.folds[[k]], ]
#    test  <- df1[ k.folds[[k]], ]
#    sapply(1:11, function(m) {
#      fit   <- randomForest(shot_made_flag ~. , data = train,
#                            mtry = m, ntree = 500)
#      prd   <- predict(fit, newdata = test)
#      tru   <- test$shot_made_flag
#      sum(prd == 1 & tru == 1)/sum(tru == 1)
#    })
#  })
#try.TP.RF <- as.data.frame(try.TP.RF)
try.TP.RF <- read.csv("try.TP.RF.csv")[, -1]


#try.FP.RF <- 
#  sapply(1:20, function(k) {
#    train <- df1[-k.folds[[k]], ]
#    test  <- df1[ k.folds[[k]], ]
#    sapply(1:11, function(m) {
#      fit   <- randomForest(shot_made_flag ~. , data = train,
#                            mtry = m, ntree = 500)
#      prd   <- predict(fit, newdata = test)
#      tru   <- test$shot_made_flag
#      sum(prd == 1 & tru == 0)/sum(tru == 0)
#    })
#  })
#try.FP.RF <- as.data.frame(try.FP.RF)
try.FP.RF <- read.csv("try.FP.RF.csv")[, -1]

## it is not important ##
plot(colMeans(t(try.TP.RF)) ~ colMeans(t(try.FP.RF)), 
     pch = 16, col = colorRampPalette(c("green", "darkgreen"))(11), 
     asp = 1, xlim = c(0, 1), ylim = c(0,1), 
     xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Plot for Random Forest"
     )
abline(0, 1, lty = 3)
legend("bottomright", legend = 1:11, col = colorRampPalette(c("green", "darkgreen"))(11), pch = 16)
text(max(colMeans(t(try.FP.RF))), max(colMeans(t(try.TP.RF))), 
     labels = which.max(colMeans(t(try.TP.RF))), pos = 4)
##############################################################################
## Support Vector Machine ####################################################
##############################################################################
library(e1071)
k.folds <- createFolds(1:25697, k = 5)
cv.svm.linear.tmp <- 
  sapply(1:20, function(k){
    train <- df1[-k.folds[[k]], ]
    test  <- df1[ k.folds[[k]], ]
    sapply(10^c(-5, 0, 5), function(c){
      svm.linear <- svm(shot_made_flag ~. , data = train, kernel = "linear", cost = c) 
      prd <- predict(svm.linear, newdata = test)
      tru <- df1$shot_made_flag[k.folds[[k]]]
      cbind(mean(prd != tru), 
            sum(prd == 1 & tru == 1)/sum(tru == 1), 
            sum(prd == 1 & tru == 0)/sum(tru == 0))
    })
  })
##############################################################################
## Model Selection ROC comparison ############################################
##############################################################################
par(mfrow = c(1,1))
plot(cut.mean.TP.lasso ~ cut.mean.FP.lasso, type = "l", asp = 1, col = "gold", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curves")
knn.col <- colorRampPalette(c("pink", "violet"))(20)
for(y in 1996:2015){
  cv.knn.year <- cv.knn.err.TP.FP(y, y)
  points(cv.knn.year$TP ~ cv.knn.year$FP, col = knn.col[y-1995], type = "l", cex = .7)
}
points(cut.mean.TP.bth3 ~ cut.mean.FP.bth3, type = "l", asp = 1, col = "darkviolet", lwd = 2, lty = 3)
points(colMeans(t(try.TP.RF)) ~ colMeans(t(try.FP.RF)), 
       pch = 16, col = colorRampPalette(c("green", "darkgreen"))(11))
abline(0, 1, lty = 3, lwd = 2)
legend("bottomright", legend = c("Lasso", "Mixed", "KNN", "Random Forest"), lwd = 2, 
       col = c("gold", "darkviolet", "violet", "green3"), lty = c(1, 3, 1, 0), 
       pch =  c(32,32,32,16))

