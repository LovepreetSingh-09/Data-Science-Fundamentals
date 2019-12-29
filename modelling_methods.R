library(ROCR)
library(rpart)
library(class)
library(e1071)

d=read.table('orange_small_train.data.gz',sep='\t',header = T, na.strings = c(NA,''))
str(d)

churn<- read.table('orange_small_train_churn.labels.txt',header=F,sep='\t')
d$churn <- churn$V1

appetency <- read.table('orange_small_train_appetency.labels.txt', header=F,sep='\t')
d$appetency <- appetency$V1

upselling <- read.table('orange_small_train_upselling.labels.txt',header=F,sep='\t')
d$upselling <- upselling$V1

set.seed(729375)
d$rgroup=runif(dim(d)[[1]])
dtrainall <- subset(d,rgroup>0.1)
dtest <- subset(d,rgroup<=0.1)
outcomes <- c('churn','appetency','upselling')
colnames(dtrainall)

vars <- setdiff(colnames(d),c(outcomes,'rgroup'))
vars

catvars <- vars[sapply(dtrainall[ ,vars], class) %in% c('factor','character')]
catvars

numvars <- vars[sapply(dtrainall[ ,vars], class) %in% c('integer','numeric')]
numvars

rm(list=c('d','churn','appetency','upselling'))
outcome <- "churn"
pos <- '1'

useforcal <- rbinom(n=dim(dtrainall)[[1]],size = 1, prob=0.1)>0
useforcal
dcal <- subset(dtrainall,useforcal)
dim(dcal)
dim(dtrainall)
dtrain <- subset(dtrainall,!useforcal)
dim(dtrain)

table218 <- table(var218=dtrain[,"Var218"],outcome=dtrain[ ,outcome],useNA = 'ifany')
'churn' %in% colnames(dtrain)
colnames(dtrain)
table218
table218[,'1']
colSums(table218)
print(table218[,2]/(table218[,1]+table218[,2]))

makepred <- function(outcol,varcol,appcol){
  pPos <- sum(outcol==pos)/length(outcol)
  natab <- table(as.factor(outcol[is.na(varcol)]))
  pPoswna=(natab/sum(natab))[pos]
  vtab <- table(as.factor(outcol),varcol)
  pPoswv <- (vtab[pos, ])/(colSums(vtab))
  pred <- pPoswv[appcol]
  pred[is.na(appcol)]=pPoswna
  pred[is.na(pred)] <- pPos
  pred
}


for (v in catvars) {
  pi <- paste('pred',v,sep='')
  dtrain[,pi] <- makepred(dtrain[,outcome],dtrain[,v],dtrain[,v])
  dcal[,pi] <- makepred(dtrain[,outcome],dtrain[,v],dcal[,v])
  dtest[,pi] <- makepred(dtrain[,outcome],dtrain[,v],dtest[,v])
}

calcAUC <- function(predcol,outcol){
  perf <- performance(prediction(predcol,outcol==pos),measure = 'auc')
  as.numeric(perf@y.values)
}

for (v in catvars){
  pi <- paste('pred',v,sep='')
  auctrain <- calcAUC(dtrain[,pi],dtrain[,outcome])
  if (auctrain>0.8){
    auccal <- calcAUC(dcal[,pi],dcal[,outcome])
    print(sprintf('%s trainAUC: %4.3f  CalibrtionAUC: %4.3f',pi,auctrain,auccal))
  }
}

mkPredN <- function(varcol,outcol,appcol){
  cuts <- unique(as.numeric(quantile(varcol,probs=seq(0,1,0.1),na.rm = T)))
  varC <- cut(varcol,cuts)
  appC <- cut(appcol,cuts)
  makepred(outcol,varC,appC)
}
for (v in numvars){
  pi <- paste('pred',v,sep='')
  dtrain[,pi] <- mkPredN(dtrain[,outcome],dtrain[,v],dtrain[,v])
  dtest[,pi] <- mkPredN(dtrain[,outcome],dtrain[,v],dtest[,v])
  dcal[,pi] <- mkPredN(dtrain[,outcome],dtrain[,v],dcal[,v])
  aucTrain <- calcAUC(dtrain[,pi],dtrain[,outcome])
  if(aucTrain>=0.55) {
    aucCal <- calcAUC(dcal[,pi],dcal[,outcome])
    print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f", pi,aucTrain,aucCal))
  } 
}

a=sample(20,9)
a=seq(1,5,1)
a
sort(a)
v=quantile(a,probs=seq(0,1,0.1),na.rm = T)
v
v=unique(as.numeric(v))
cut(a,v)

logLikelyhood <- function(outcol,predcol){
  sum(ifelse(outcol==pos,log2(predcol),log2(1-predcol)))
}
selVars <- c()
minStep <- 5
baseRateCheck <-logLikelyhood(dcal[,outcome],sum(as.numeric(dcal[,outcome]==pos)/length(dcal[,outcome])))
for(v in catvars) {
  pi <- paste('pred',v,sep='')
  liCheck <- 2*((logLikelyhood(dcal[,outcome],dcal[,pi]) -baseRateCheck))
  if(liCheck>minStep) {
    print(sprintf("%s, calibrationScore: %g", pi,liCheck))
    selVars <- c(selVars,pi)
  }
}

for(v in numvars) {
  pi <- paste('pred',v,sep='')
  liCheck <- 2*((logLikelyhood(dcal[,outcome],dcal[,pi]) -baseRateCheck) - 1)
  if(liCheck>=minStep) {
    print(sprintf("%s, calibrationScore: %g", pi,liCheck))
    selVars <- c(selVars,pi)
  }
}

f <- paste(outcome,'>0 ~ ',paste(selVars,collapse = '+'),sep='')
f
tmodel <- rpart(f,data=dtrain,control=rpart.control(minsplit = 1000,minbucket = 1000,cp=0.001,maxdepth = 5))
tmodel

print(calcAUC(predict(tmodel,dtrain),dtrain[,outcome]))
print(calcAUC(predict(tmodel,dtest),dtest[,outcome]))
print(calcAUC(predict(tmodel,dcal),dcal[,outcome]))

par(cex=0.7)
plot(tmodel)
text(tmodel)

nK <- 200
knnTrain <- dtrain[,selVars]
knnCl <- dtrain[,outcome]==pos
knnPred <- function(df) {
  knnDecision <- knn(knnTrain,df,knnCl,k=nK,prob=T)
  ifelse(knnDecision==TRUE,
         attributes(knnDecision)$prob,
         1-(attributes(knnDecision)$prob))
}
print(calcAUC(knnPred(dtrain[,selVars]),dtrain[,outcome]))
print(calcAUC(knnPred(dcal[,selVars]),dcal[,outcome]))
print(calcAUC(knnPred(dtest[,selVars]),dtest[,outcome]))

dcal$kpred <- knnPred(dcal[,selVars])
ggplot(data=dcal) +
  geom_density(aes(x=kpred, color=as.factor(churn),linetype=as.factor(churn)))

plotROC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'tpr','fpr')
  pf <- data.frame(
    FalsePositiveRate=perf@x.values[[1]],
    TruePositiveRate=perf@y.values[[1]])
  ggplot() +
    geom_line(data=pf,aes(x=FalsePositiveRate,y=TruePositiveRate)) +
    geom_line(aes(x=c(0,1),y=c(0,1)))
}
print(plotROC(knnPred(dtest[,selVars]),dtest[,outcome]))


lVars <- c(catvars,numvars)
ff <- paste('as.factor(',outcome,'>0) ~ ', paste(lVars,collapse=' + '),sep='')
ff
nbmodel <- naiveBayes(as.formula(ff),data=dtrain)
dtrain$nbpred <- predict(nbmodel,newdata=dtrain,type='raw')[,'TRUE']
dcal$nbpred <- predict(nbmodel,newdata=dcal,type='raw')[,'TRUE']
dtest$nbpred <- predict(nbmodel,newdata=dtest,type='raw')[,'TRUE']
calcAUC(dTrain$nbpred,dtrain[,outcome])
calcAUC(dCal$nbpred,dcal[,outcome])
calcAUC(dTest$nbpred,dtest[,outcome])
