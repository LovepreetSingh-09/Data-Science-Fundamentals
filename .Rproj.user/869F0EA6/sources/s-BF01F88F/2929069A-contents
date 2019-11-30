library(ggplot2)
spamD=read.table('spamD.txt',sep='\t',header=T)
summary(spamD)
str(spamD)

spamtrain=subset(spamD,rgroup>10)
spamtest=subset(spamD,rgroup<=10)
spamVars=setdiff(colnames(spamD),c('rgroup','spam'))
spamFormula=as.formula(paste('spam=="spam"',paste(spamVars,collapse = '+'),sep = '~'))
spamFormula
spamModel=glm(spamFormula,family = binomial(link='logit'),data=spamtrain)
spamtrain$pred=predict(spamModel,newdata = spamtrain,type='response')
spamtest$pred=predict(spamModel,newdata=spamtest,type='response')

spamtest$pred
print(with(spamtest,table(spam,pred>0.5)))
sample=spamtest[c(24,35,1,356,26),c('spam','pred')]
sample

d=data.frame(y=(1:10)^2,x=1:10)
model=lm(y~x, data=d)
d$pred=predict(model,d)
ggplot(d)+geom_point(aes(x=x,y=y))+
  geom_line(aes(x=x,y=pred),color='blue')+
  geom_segment(aes(x=x,y=y,xend=x,yend=pred))

