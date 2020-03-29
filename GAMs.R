library(mgcv)
library(ggplot2)

set.seed(123)
noise=rnorm(1000,sd=1.5)
x=rnorm(1000)

y=3*sin(2*x)+2*cos(0.75*x)-1.5*(x^2)+noise

select <- runif(1000)
frame <- data.frame(x=x,y=y)

train <- frame[select>0.1,]
test <- frame[select<=0.1,]

l_model <- lm(y~x,data=train)
summary(l_model)

l_resid <- train$y-predict(l_model)
sqrt(mean(l_resid^2))

gam_model <- gam(y~s(x),data=train)
gam_model$converged

summary(gam_model)
gam_resid <- train$y-predict(gam_model)
sqrt(mean(gam_resid^2))

act=test$y
lin_pred <- predict(l_model,test)
gam_pred <- predict(gam_model,test)

resid_l <- act-lin_pred
resid_g <- act-gam_pred

sqrt(mean(resid_l^2))
sqrt(mean(resid_g^2))

cor(act,lin_pred)
cor(act,gam_pred)

# y data points used to make the spline graph or predictions using terms
sx <- predict(gam_model,type='terms')
dim(sx)
summary(sx)
xframe <- cbind(train,sx=sx[,1])
xframe

ggplot(xframe,aes(x=x))+
  geom_point(aes(y=y),alpha=0.4,color='blue')+
  geom_smooth(aes(y=sx),color='green')+
  ylim(-5,6)
  

load('NatalBirthData.rData')
str(sdata)
train <- sdata[sdata$ORIGRANDGROUP<=5,]
test <- sdata[sdata$ORIGRANDGROUP>5,]
form.lin <- as.formula("DBWT ~ PWGT + WTGAIN + MAGER + UPREVIS")
l_model <- lm(form.lin, data=train)
summary(linmodel)

form.glin <- as.formula("DBWT ~ s(PWGT) + s(WTGAIN) +s(MAGER) + s(UPREVIS)")
g_model <- gam(form.glin, data=train)
g_model$converged
summary(g_model)

terms <- predict(g_model, type="terms")
tframe <-cbind(DBWT = train$DBWT, as.data.frame(terms))
colnames(tframe) <- gsub('[()]', '', colnames(tframe))
pframe <- cbind(tframe, train[,c("PWGT", "WTGAIN", "MAGER", "UPREVIS")])

ggplot(pframe, aes(x=PWGT)) +
  geom_point(aes(y=scale(sPWGT, scale=F))) +
  geom_smooth(aes(y=scale(DBWT, scale=F)))

pred.lin <- predict(l_model, newdata=test)
pred.glin <- predict(g_model, newdata=test)
cor(pred.lin, test$DBWT)^2
cor(pred.glin, test$DBWT)^2
