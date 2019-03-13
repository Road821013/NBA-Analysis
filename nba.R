
library(dplyr)
library(data.table)
library(glmnet)

# 資料處裡(刪掉重複的場次)
setwd("C:/Users/User/Desktop/")

A <- fread("nba.games.stats.csv")

A %>% head()
A <- A %>% mutate(tmp = ifelse(Home=="Home", paste0(Team, Date, Opponent), paste0(Opponent, Date, Team)))
A$tmp %>% head()
ssn <- 1:9840
ssn_tmp <- A$tmp
B <- data.table(ssn = ssn, ssn_tmp = ssn_tmp, stringsAsFactors = F)
C <- B[order(B$ssn_tmp),]

tt <- ifelse(runif(4920)>0.5, 0, 1)
go <- NULL
for (i in 1:4920) {
  go <- c(go, ifelse(tt[i]==0, 2*i-1, 2*i))
}
D <- C[go, ]

want <- D$ssn
AA <- A[want, ]
# 資料處裡(刪掉重複的場次)


AA <- AA %>% mutate(WIN = ifelse(WINorLOSS=="W",1,0))
AA <- AA %>% mutate(HOME = ifelse(Home=="Home",1,0))

Data_for_model <- AA[,-c(1,2,4,5,6,7,42)]
model1 <- glm(WIN ~.,family = binomial,data=Data_for_model)
summary(model1)

#將資料分為自變數&因變數(跑Lasso)
X <- Data_for_model[,-c(36)]
WIN <- Data_for_model[,c(36)]
#將資料分為自變數&因變數(跑Lasso)
Ridge = glmnet(x = as.matrix(X), 
               y = WIN, 
               alpha = 0,
               family = "binomial")
cv.Ridge = cv.glmnet(x=as.matrix(X),
                     y=WIN,
                     alpha=0,
                     family="binomial"
)
best.lamda.Ridge=cv.Ridge$lambda.min
best.Ridge.model = glmnet(x = as.matrix(X), 
                          y = WIN, 
                          alpha = 0,
                          lambda = best.lamda.Ridge,
                          family = "binomial")
Var_Selected.Ridge = colnames(X)[which(best.Ridge.model$beta!=0)]
coef(best.Ridge.model)

lasso = glmnet(x = as.matrix(X), 
               y = WIN, 
               alpha = 1,
               family = "binomial")
cv.lasso = cv.glmnet(x=as.matrix(X),
                     y=WIN,
                     alpha=1,
                     family="binomial"
                     )
best.lamda.lasso=cv.lasso$lambda.min
best.lasso.model = glmnet(x = as.matrix(X), 
               y = WIN, 
               alpha = 1,
               lambda = best.lamda.lasso,
               family = "binomial")
Var_Selected.lasso = colnames(X)[which(best.lasso.model$beta!=0)]
coef(best.lasso.model)

Data_after_lasso <- Data_for_model[,c(1,2,3,4,6,12,17,19,22,24,25,29,31,32,35,36)]



model_after_lasso <- glm(formula=WIN~.,family = "binomial",data=Data_after_lasso)
summary(model_after_lasso)

#只取得失分
Data_OnlyPoint <-Data_for_model [,c(2,3)]
model_OnlyPoint  <- glm(formula=WIN~.,family = "binomial",data=Data_OnlyPoint)
summary(model_OnlyPoint )
#只取得失分

#只取得失分進失球
Data_OnlyPointFiledGoal<-Data_for_model [,c(2,3,4,20)]
model_OnlyPointFiledGoal  <- glm(formula=WIN~.,family = "binomial",data=Data_OnlyPointFiledGoal)
summary(model_OnlyPointFiledGoal )
#只取得失分進失球

#只取進失球
Data_OnlyFiledGoal<-Data_for_model [,c(4,20)]
model_OnlyFiledGoal  <- glm(formula=WIN~.,family = "binomial",data=Data_OnlyFiledGoal)
summary(model_OnlyFiledGoal )
#只取進失球

#只取進失球命中率
Data_OnlyFiledGoal.<-Data_for_model [,c(6,22)]
model_OnlyFiledGoal.  <- glm(formula=WIN~.,family = "binomial",data=Data_OnlyFiledGoal.)
summary(model_OnlyFiledGoal. )
#只取進失球命中率

#將得失分刪除
Data_minusPoint <-Data_for_model [,-c(2,3)]
model_minusPoint  <- glm(formula=WIN~.,family = "binomial",data=Data_minusPoint)
summary(model_minusPoint )
#將得失分刪除

#將得失分/進失球刪除
Data_adjusted <-Data_for_model [,-c(2,3,4,20)]
model_adjusted  <- glm(formula=WIN~.,family = "binomial",data=Data_adjusted)
summary(model_adjusted )
#將得失分/進失球刪除

#將得失分/進失球命中率刪除
Data_minusPointFiledGoal. <-Data_for_model [,-c(2,3,6,22)]
model_minusPointFiledGoal.  <- glm(formula=WIN~.,family = "binomial",data=Data_minusPointFiledGoal.)
summary(model_minusPointFiledGoal. )
#將得失分/進失球命中率刪除

##將得失分/進失球刪除後跑Lasso)
X2 <- Data_adjusted[,-c(32)]
WIN2 <- Data_adjusted[,c(32)]


lasso2 = glmnet(x = as.matrix(X2), 
               y = WIN2, 
               alpha = 1,
               family = "binomial")
cv.lasso2 = cv.glmnet(x=as.matrix(X2),
                     y=WIN2,
                     alpha=1,
                     family="binomial"
)
best.lamda.lasso2=cv.lasso$lambda.min
best.lasso.model2 = glmnet(x = as.matrix(X2), 
                          y = WIN2, 
                          alpha = 1,
                          lambda = best.lamda.lasso2,
                          family = "binomial")
Var_Selected.lasso2 = colnames(X)[which(best.lasso.model2$beta!=0)]
coef(best.lasso.model2)

Data_after_lasso2 <- Data_adjusted[,-c(5,25,29)]
model_adjusted_after_lasso  <- glm(formula=WIN~.,family = "binomial",data=Data_after_lasso2)
summary(model_adjusted_after_lasso )
##將得失分/進失球刪除後跑Lasso)


#將得失分/進失球/進失球率刪除
Data_adjusted2 <-Data_for_model [,-c(2,3,4,20,6,22)]
model_adjusted2  <- glm(formula=WIN~.,family = "binomial",data=Data_adjusted2)
summary(model_adjusted2 )
#將得失分/進失球/進失球率刪除

#將得失分/進失球/進失球率/出手數刪除
Data_adjusted3 <-Data_for_model [,-c(2,3,4,20,6,22,5,21)]
model_adjusted3  <- glm(formula=WIN~.,family = "binomial",data=Data_adjusted3)
summary(model_adjusted3 )
#將得失分/進失球/進失球率/出手數刪除

#將對手變數刪除/加回出手數
DATA_Halfside <- A[,c(3,5,7,11,13,14,15,16,17,18,19,20,21,22,23,24,25)]
DATA_Halfside <- DATA_Halfside %>% mutate(WIN = ifelse(WINorLOSS=="W",1,0))
DATA_Halfside <- DATA_Halfside %>% mutate(HOME = ifelse(Home=="Home",1,0))
DATA_Halfside <- DATA_Halfside[,-c(2,3)]
model_Halfside   <- glm(formula=WIN~.,family = "binomial",data=DATA_Halfside)
summary(model_Halfside  )
#將對手變數刪除/加回出手數

#將對手變數刪除/加回出手數
Data_adjusted4 <-Data_for_model [,-c(2,3,4,20,6,22,23,24,25,26,27,28,29,30,31,32,33,34,35)]
model_adjusted4  <- glm(formula=WIN~.,family = "binomial",data=Data_adjusted4)
summary(model_adjusted4 )
#將對手變數刪除/加回出手數
