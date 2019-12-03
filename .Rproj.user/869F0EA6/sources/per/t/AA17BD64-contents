library(ggplot2)
library(ROCR)
library(grDevices)
library(reshape2)


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

ggplot(data=spamtest)+
  geom_density(aes(x=pred,color=spam,linetype=spam))

eval=prediction(spamtest$pred,spamtest$spam)
plot(performance(prediction.obj = eval,measure='tpr',x.measure = 'fpr') )              
attributes(performance(eval,measure = 'auc'))$y.values

sum(ifelse(spamtest$spam=='spam',log(spamtest$pred),log(1-spamtest$pred)))
sum(ifelse(spamtest$spam=='spam',log(spamtest$pred),log(1-spamtest$pred)))/dim(spamtest)[[1]]
sum(spamtest$spam=='spam')
cm=table(spamtest$spam,spamtest$pred>0.5)
cm

pnull=sum(ifelse(spamtest$spam=='spam',1,0))/dim(spamtest)[[1]]
pnull
sum(ifelse(spamtest$spam=='spam',1,0))*log(pnull)+
  sum(ifelse(spamtest$spam=='spam',0,1))*log(1-pnull)
sum(ifelse(spamtest$spam=='spam',log(pnull),log(1-pnull)))

entropy=function(x){
  xpos=x[x>0]
  scaledx=xpos/sum(xpos)
  sum(-scaledx*log2(scaledx))

}
entropy(table(spamtest$spam))
ab=table(spamtest$spam); ab
ab/sum(ab)
cm
cm/sum(cm)

conditional_entropy=function(x){
  (sum(x[,1])*entropy(x[,1])+sum(x[,2]*entropy(x[,2])))/sum(x)
}
conditional_entropy(cm)

set.seed(32297)
d <- data.frame(x=runif(100),y=runif(100))
clus <- kmeans(d,centers=5)
d$clusters=clus$cluster
d$clusters

h=do.call(rbind,lapply(unique(clus$cluster),
                       function(c){
                        f=subset(d,clusters==c)
                        f[chull(f), ]  
                        } ))

h
ggplot()+
  geom_text(data=d,aes(x=x,y=y,label=clusters,color=as.factor(clusters)),size=3)+
  geom_polygon(data=h,aes(x=x,y=y,group=clusters,fill=as.factor(clusters)),alpha=0.3,linetype=0)+
  theme(legend.position='none')

table(d$clusters)

n=dim(d)[[1]]
n
pairs <- data.frame(
  ca=as.vector(outer(1:n,1:n,function(a,b) d[a,'clusters'])),
    cb=as.vector(outer(1:n,1:n,function(a,b) d[b,'clusters'])),
    dist=as.vector(outer(1:n,1:n,function(a,b)
      sqrt((d[a,'x']-d[b,'x'])^2 + (d[a,'y']-d[b,'y'])^2)))
  )

pairs
dcast(pairs,ca~cb,value.var='dist',mean)
cb
ca



c=matrix(1:5,1:5,nrow=5,ncol=5)
c
as.vector(c)
