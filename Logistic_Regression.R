library(ggplot2)
library(ROCR)
library(grid)

load("NatalRiskData.rData")
str(sdata)
dtrain=sdata[sdata$ORIGRANDGROUP<=5,]
dtest=sdata[sdata$ORIGRANDGROUP>5,]
str(dtrain)

complications <- c("ULD_MECO","ULD_PRECIP","ULD_BREECH")
riskfactors <- c("URF_DIAB", "URF_CHYPER", "URF_PHYPER", "URF_ECLAM")
y <- "atRisk"
x <- c("PWGT","UPREVIS", "CIG_REC","GESTREC3", "DPLURAL",complications,riskfactors)
fml <- paste(y,paste(x,collapse='+'),sep='~')
fml

model=glm(fml,dtrain,family = binomial(link='logit'))
summary(model)

# type='response' to get the values in the probabilities otherwise the values will be logits
dtrain$pred <- predict(model,dtrain,type='response')
dtest$pred <- predict(model,dtest,type = 'response')

ggplot(dtrain,aes(x=pred,color=atRisk,linetype=atRisk))+
  geom_density()

predobj <- prediction(dtrain$pred,dtrain$atRisk)
prec.obj <- performance(predobj,measure='prec')
prec.obj
rec.obj <- performance(predobj,measure = 'rec')

(prec.obj@y.values)[[1]]
precision <- prec.obj@y.values[[1]]
recall <- rec.obj@y.values[[1]]
prec.x <- prec.obj@x.values[[1]]

rocframe <- data.frame(threshold=prec.x,precision=precision,recall=recall)
str(rocframe)

nplot <- function(plist){
  n=length(plist)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(n,1)))
  vplayout <- function(x,y){
    viewport(layout.pos.row = x,layout.pos.col = y)
  }
  for (i in 1:n){
    print(plist[i],vp=vplayout(i,1))
  } 
}

pnull <- mean(as.numeric(dtrain$atRisk))
pnull

# enrichment vs threshold lot
p1 <- ggplot(rocframe,aes(x=threshold,y=precision/pnull))+
  geom_line()+
  coord_cartesian(xlim=c(0,0.05),ylim = c(0,10))

p2 <- ggplot(rocframe,aes(x=threshold,y=recall))+
  geom_line()+
  coord_cartesian(xlim=c(0,0.05))

nplot(list(p1,p2))

ctab.test <- table(True=dtest$atRisk,Predicted=dtest$pred>0.02)
ctab.test
prec=ctab.test[2,2]/(ctab.test[1,2]+ctab.test[2,2])
prec
rec=ctab.test[2,2]/(ctab.test[2,2]+ctab.test[2,1])
rec

enrichment <- prec/pnull
enrichment

summary(model)
coefficients(model)

llcomponents <- function(y,py){
  y*log(py)+(1-y)*log(1-py)
}

edev <- sign(as.numeric(dtrain$atRisk)-dtrain$pred)*
  sqrt(-2*llcomponents(as.numeric(dtrain$atRisk),dtrain$pred))

summary(edev)
summary(residuals(model))

pnull
null.dev <- -2*sum(llcomponents(as.numeric(dtrain$atRisk),pnull))
null.dev
model.dev <- -2*sum(llcomponents(as.numeric(dtrain$atRisk),dtrain$pred))
model.dev

pnull.test <- mean(as.numeric(dtest$atRisk))
pnull.test
null.dev <- -2*sum(llcomponents(as.numeric(dtest$atRisk),pnull.test))
null.dev
model.dev <- -2*sum(llcomponents(as.numeric(dtest$atRisk),dtest$pred))
model.dev

DOF.null <- dim(dtrain)[[1]]-1
DOF.null
DOF.model <- dim(dtrain)[[1]]-length(coefficients(model))
DOF.model

delDev <- null.dev-model.dev
delDev
delDOF <- DOF.null-DOF.model
delDOF

p <- pchisq(delDev,delDOF,lower.tail = F)
p

prsq <- function(x,y){
  1-(x/y)
}
prsq(model.dev,null.dev)

AIC <- -2*sum(llcomponents(as.numeric(dtrain$atRisk),dtrain$pred))+2*length(coefficients(model))
AIC
