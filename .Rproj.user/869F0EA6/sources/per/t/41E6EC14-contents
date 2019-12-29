library(MASS)
library(ggplot2)

load('psub.RData')
str(psub)
dim(psub)
dtrain <- subset(psub,ORIGRANDGROUP>=500)
dtest <- subset(psub,ORIGRANDGROUP<500)
dim(dtest)
model <- lm(log10(PINCP)~AGEP+SEX+COW+SCHL, data=dtrain)
summary(model)

dtrain$predLogPINCP <- predict(model,data=dtrain)
dtest$predLogPINCP <- predict(model,dtest)

ggplot(dtest,aes(x=predLogPINCP,y=log10(PINCP)))+
  geom_point(alpha=0.2)+
  geom_smooth(color='black')+
  geom_line(aes(x=log10(PINCP),y=log10(PINCP)),linetype=2,color='blue')+
  scale_x_continuous(limits = c(4.2,5))+
  scale_y_continuous(limits = c(4,5.5))

ggplot(dtest,aes(x=predLogPINCP,y=(predLogPINCP-log10(PINCP))))+
  geom_point()+
  geom_smooth()

rsq <- function(y,f){
  1-(sum((y-f)^2)/sum((y-mean(y))^2))
}
train_rsq <- rsq(log10(dtrain$PINCP),predict(model,dtrain))
train_rsq
rsq(log10(dtest$PINCP),predict(model,dtest))

rmse <- function(y,f){
  sqrt(mean((y-f)^2))
}

rmse(log10(dtrain$PINCP),predict(model,dtrain))
rmse(log10(dtest$PINCP),predict(model,dtest))

summary(log10(dtrain$PINCP)-predict(model,dtrain))
summary(log10(dtest$PINCP)-predict(model,dtest))

DOF <- dim(dtrain)[[1]]-dim(summary(model)$coefficient)[[1]]
DOF

residuals(model)
Residual_SE <- sqrt(sum(residuals(model)^2)/DOF)
Residual_SE

