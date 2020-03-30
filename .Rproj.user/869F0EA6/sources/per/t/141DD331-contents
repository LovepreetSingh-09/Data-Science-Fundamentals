library(mgcv)
library(ggplot2)

load('psub.rData')
str(psub)

dtrain <- subset(psub,ORIGRANDGROUP >= 500)
dim(dtrain)
dtest <- subset(psub,ORIGRANDGROUP < 500)

m1 <- step(
  lm(log(PINCP,base=10) ~ AGEP + SEX + COW + SCHL,data=dtrain),
  direction='both')

rmse <- function(y, f) { sqrt(mean( (y-f)^2 )) }
print(rmse(log(dtest$PINCP,base=10),predict(m1,newdata=dtest)))

phi <- function(x){
  x <- as.numeric(x)
  c(x,x*x,combn(x,2,FUN=prod))
}

phiNames <- function(n){
  c(n,paste(n,n,sep = ':'),
    combn(n,2,FUN = function(x){ 
      paste(x,collapse = ':')
      }))
}

modelMatrix <- model.matrix(~ 0 + AGEP + SEX + COW + SCHL,psub)

colnames(modelMatrix) <- gsub('[^a-zA-Z0-9]+','_', colnames(modelMatrix))
colnames(modelMatrix)

pM <- t(apply(modelMatrix,1,phi))
vars <- phiNames(colnames(modelMatrix))
vars <- gsub('[^a-zA-Z0-9]+','_',vars)
colnames(pM) <- vars
pM <- as.data.frame(pM)
str(pM)

pM$PINCP <- psub$PINCP
pM$ORIGRANDGROUP <- psub$ORIGRANDGROUP
pMtrain <- subset(pM,ORIGRANDGROUP >= 500)
pMtest <- subset(pM,ORIGRANDGROUP < 500)

formulaStr2 <- paste('log(PINCP,base=10)',
                     paste(vars,collapse=' + '),sep=' ~ ')
m2 <- lm(as.formula(formulaStr2),data=pMtrain)
coef2 <- summary(m2)$coefficients
interestingVars <- setdiff(rownames(coef2)[coef2[,'Pr(>|t|)']<0.01],'(Intercept)')
interestingVars <- union(colnames(modelMatrix),interestingVars)

formulaStr3 <- paste('log(PINCP,base=10)',
                     paste(interestingVars,collapse=' + '), sep=' ~ ')
m3 <- step(lm(as.formula(formulaStr3),data=pMtrain),direction='both')
print(rmse(log(pMtest$PINCP,base=10),predict(m3,newdata=pMtest)))
print(summary(m3))

