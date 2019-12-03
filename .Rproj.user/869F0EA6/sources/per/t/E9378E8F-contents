library(ROCR)

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
  pPoswv <- vtab[pos,]/colSums(vtab)
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


