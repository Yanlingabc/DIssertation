library(car)
library(caret)
library(corrplot)
data <- read.csv("data2.csv")
attach(data)
# scale data
newdata <- data[,c(-1,-21)]
train <- newdata[1:40,]
test <- newdata[41:48,]
means <- sapply(train[,2:19],mean)
stdev <- sapply(train[,2:19],sd)
train.scaled <- as.data.frame(scale(train[,2:19],center = means,scale=stdev))
train.scaled$GDP <- train$GDP
test.scaled <- as.data.frame(scale(test[,2:19],center = means,scale=stdev))
test.scaled$GDP <- test$GDP
train.scaled <- cbind(train.scaled$GDP,train.scaled[,-19])
colnames(train.scaled)[1] <- "GDP"
test.scaled <- cbind(test.scaled$GDP,test.scaled[,-19])
colnames(test.scaled)[1] <- "GDP"
# EDA
lm_o <- lm(GDP~.,train.scaled)
cor_matr <- cor(train.scaled)
corrplot(cor_matr,type = "upper")
vif(lm_o)

predict_o <- predict(lm_o,test.scaled)
lmvalues_o <- data.frame(obs=test.scaled$GDP,pred=predict_o)
defaultSummary(lmvalues_o)
# Stepwise regression
library(MASS)
summary(stepAIC(lm_o,direction = "both"))
fit_step <- lm(GDP~  POP + FER + EXP + FDI + CO2 + GREEN + INV + 
                 HEA + VAL + Inflation + HOU + SAV + IPD, data = train.scaled)
predict_step <- predict(fit_step,test.scaled)
lmvalues_step <- data.frame(obs=test.scaled$GDP,pred=predict_step)
defaultSummary(lmvalues_step)
# Best subset regression
library(leaps)
leaps <- regsubsets(GDP~.,train.scaled,nvmax=18)
reg.summary <- summary(leaps)
plot(reg.summary$adjr2,type="l",xlab="numbers of Features",
     ylab="adjr2")
plot(reg.summary$bic,type="l",xlab="numbers of Features",
     ylab="bic")

fit_sub <- lm(GDP~POP+FDI+CO2+INV+VAL+LTI+LWL+HOU+SAV+IPD,train.scaled)
predict_sub <- predict(fit_sub,test.scaled)
lmvalues_sub <- data.frame(obs=test.scaled$GDP,pred=predict_sub)
defaultSummary(lmvalues_sub)

fit_sub1 <- lm(GDP~EXP+FDI+GREEN+VAL+HOU+SAV+IPD,train.scaled)
predict_sub1 <- predict(fit_sub1,test.scaled)
lmvalues_sub1 <- data.frame(obs=test.scaled$GDP,pred=predict_sub1)
defaultSummary(lmvalues_sub1)
# Lasso regression
library(lars)
train_1 <- as.matrix(train.scaled)
lar <- lars(train_1[,-1],train_1[,1])
summary(lar)

fit_las <- lm(GDP~VAL+HOU+EXP+TRADE+SAV+IPD+FDI+GREEN+LWL,data=train.scaled)
predict_las <- predict(fit_las,test.scaled)
lmvalues_las <- data.frame(obs=test.scaled$GDP,pred=predict_las)
defaultSummary(lmvalues_las)
# BMA
library(BMA)
y <- train.scaled[,1]
x <- train.scaled[,-1]
bma_enu <- iBMA.bicreg(x, y, thresProbne0 = 5, verbose = TRUE, maxNvar = 30)
summary(bma_enu)
detach("package:BMA")

fit_bma <- lm(GDP~VAL+HOU+SAV+FDI+EXP+IPD,train.scaled)
predict_bma <- predict(fit_bma,test.scaled)
lmvalues_bma <- data.frame(obs=test.scaled$GDP,pred=predict_bma)
head(lmvalues_bma)
defaultSummary(lmvalues_bma)
# BMS
library(BMS)
bms_enu <- bms(train.scaled, mcmc="enumerate", g="UIP", mprior="uniform", user.int=FALSE)
coef(bms_enu)
detach("package:BMS")

fit_bms <- lm(GDP~VAL+HOU,train.scaled)
predict_bms <- predict(fit_bms,test.scaled)
lmvalues_bms <- data.frame(obs=test.scaled$GDP,pred=predict_bms)
defaultSummary(lmvalues_bms)