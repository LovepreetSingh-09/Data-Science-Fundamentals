library(randomForest)
library(rpart)

spamD <- read.table('spamD.txt',sep='\t',header=T)
str(spamD)
View(spamD)

spam_train <- spamD[spamD$rgroup>=10,]
spam_test <- spamD[spamD$rgroup<10,]
spamvars <- setdiff(colnames(spamD),c('rgroup','spam'))

fml <- paste('spam=="spam"',paste(spamvars,collapse='+'),sep='~')
fml

loglikelihood <- function(y,py){
  pysmooth <- ifelse(py==0,1e-12,ifelse(py==1,1e-12,py))
  sum(y*log(pysmooth)+((1-y)*log(1-pysmooth)))
}

accuracy_measure <- function(pred,truth,name='model'){
  tr=ifelse(truth=='spam',1,ifelse(truth=='non-spam',0,as.numeric(truth)))
  dev.normalized <- -2*loglikelihood(tr,pred)/length(pred)
  ctable <- table(truth=truth,pred=(pred>0.5))
  accuracy <- sum(diag(ctable))/(sum(ctable))
  prec <- ctable[2,2]/(ctable[2,2]+ctable[1,2])
  recall=ctable[2,2]/(ctable[2,2]+ctable[2,1])
  f1 <- 2*prec*recall/(prec+recall)
  data.frame(model=name,accuracy=accuracy,deviance=dev.normalized,precision=prec,recall=recall,f1_score=f1)
}

model <- rpart(fml,spam_train)
accuracy_measure(predict(model,spam_train),spam_train$spam,name='training_model')
accuracy_measure(predict(model,spam_test),spam_test$spam,name='test_model')

# Bagging
n_train <- dim(spam_train)[1]
n <- n_train
n_tree <- 100

samples <- sapply(1:n_tree, FUN=function(iter){
  sample(1:n_train,size = n,replace = T)
})
dim(samples)

tree_list <- lapply(1:n_tree,FUN=function(iter){
  samp <- samples[,iter]
  rpart(fml,spam_train[samp,])
})
length(tree_list)
tree_list[[1]]

predict.bag <- function(treelist,newdata){
  preds <- sapply(1:length(treelist), FUN=function(iter){
    predict(treelist[[iter]],newdata)
  })
  predsums <- rowSums(preds)
  predsums/length(treelist)
}

accuracy_measure(predict.bag(tree_list,spam_train),spam_train$spam,name='bagging_training')
accuracy_measure(predict.bag(tree_list,spam_test),spam_test$spam,name='bagging_testing')


# Random Forest
set.seed(123)
rmodel <- randomForest(spam_train[,spamvars],spam_train$spam,ntree = 100,nodesize = 7,importance=T)
summary(rmodel)

accuracy_measure(predict(rmodel,spam_train,type='prob')[,'spam'],spam_train$spam=='spam',name='randomForest_training')
accuracy_measure(predict(rmodel,spam_test,type='prob')[,'spam'],spam_test$spam=='spam',name='randomForest_testing')

varimp=importance(rmodel)
dim(varimp)
varimp[1:10,]
varImpPlot(rmodel,type=1)

selvar=names(sort(varimp[,1],decreasing = T))[1:25]
selvar
new_rmodel <- randomForest(spam_train[,selvar],spam_train$spam,ntree = 100,nodesize = 7,importance=T)
accuracy_measure(predict(new_rmodel,spam_train,type='prob')[,'spam'],spam_train$spam=='spam',name='New_randomForest_training')
accuracy_measure(predict(new_rmodel,spam_test,type='prob')[,'spam'],spam_test$spam=='spam',name='New_randomForest_testing')

