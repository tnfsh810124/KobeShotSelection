setwd("C:/Users/user/Desktop/統計學習與資料探勘(碩)/作業")
kobe=read.csv(file="df.csv")
kobe=kobe[,2:16]

#將data分成NA一群非NA一群#
na.kobe=kobe[is.na(kobe$shot_made_flag),]
kobe=kobe[!is.na(kobe$shot_made_flag),]

#將資料轉屬為類別型(本身是類別型就不用轉)(判斷方法:貼左是文字，貼右是數字)#
kobe$season         =as.factor(kobe$season)
kobe$game_id        =as.factor(kobe$game_id)
kobe$three_pt       =as.factor(kobe$three_pt)
kobe$ot             =as.factor(kobe$ot)
kobe$home           =as.factor(kobe$home)
kobe$playoffs       =as.factor(kobe$playoffs)
kobe$shot_made_flag =as.factor(kobe$shot_made_flag)
summary(kobe)

#model selection#
#forward#
library(leaps)
mod.selec.fwd=regsubsets(model.matrix(kobe$shot_made_flag~.,data=kobe),kobe$shot_made_flag
                         ,method="forward",nvmax=14,family="binomial")
mod.summary.fwd=summary(mod.selec.fwd)
which.min(mod.summary.fwd$cp)
which.min(mod.summary.fwd$bic)
which.max(mod.summary.fwd$adjr2)

#用另一個forward指令做比對#
mod.summary.fwd2=step(glm(kobe$shot_made_flag~.,data=kobe,family=binomial)
                      ,direction="forward")

#backward#
mod.selec.bwd=regsubsets(model.matrix(kobe$shot_made_flag~.,data=kobe),kobe$shot_made_flag
                         ,method="backward",nvmax=14,family="binomial")
mod.summary.bwd=summary(mod.selec.bwd)
which.min(mod.summary.bwd$cp)
which.min(mod.summary.bwd$bic)
which.max(mod.summary.bwd$adjr2)

#用另一個backward指令做比對#
mod.summary.bwd2=step(glm(kobe$shot_made_flag~.,data=kobe,family=binomial)
                      ,direction="backward")

#best subset#
mod.selec.best=regsubsets(model.matrix(kobe$shot_made_flag~.,data=kobe),kobe$shot_made_flag
                          ,nvmax=14,family="binomial")
mod.summary.best=summary(mod.selec.best)
which.min(mod.summary.best$cp)
which.min(mod.summary.best$bic)
which.max(mod.summary.best$adjr2)

#Lasso#
library(glmnet)

#找能使C-V error最小的爛打#
xmat=model.matrix(kobe$shot_made_flag~.,data=kobe)[,-1]
mod.lasso=cv.glmnet(xmat,kobe$shot_made_flag,alpha=1,family="binomial")
best.lambda=mod.lasso$lambda.min
best.lambda
plot(mod.lasso)

#Next fit the model on entire data using best lambda#
best.model=glmnet(xmat,kobe$shot_made_flag,alpha=1,family="binomial")
predict(best.model,s=best.lambda,type="coefficients")


