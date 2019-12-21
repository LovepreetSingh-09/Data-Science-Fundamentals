library(ggplot2)
library(scales)
custdata=read.table('custdata.tsv',sep='\t',header=T)
summary(custdata)

# Missing values in categorical variables
summary(custdata[is.na(custdata$housing.type),c('recent.move','num.vehicles')])

custdata$employed.fix=as.factor(ifelse(is.na(custdata$is.employed),'missing',
                             ifelse(custdata$is.employed==T,'employed','un-employed')))
summary(custdata$employed.fix)                             

# Missing values in numeric data
summary(custdata$income)
custdata$income=ifelse(custdata$income<0,NA,custdata$income)

mean_income=mean(custdata$income,na.rm = T)
mean_income
custdata$income.fix=ifelse(is.na(custdata$income),mean_income,custdata$income)
summary(custdata$income.fix)

breaks=c(0,10000,50000,100000,1000000)
income_group=cut(custdata$income,breaks=breaks,include.lowest = T)
summary(income_group)
income_group=as.character((income_group))
Income_gp=as.factor(ifelse(is.na(income_group),'no_income',income_group))
summary(Income_gp)

# Data Transformation
median_income=aggregate(income~state.of.res,custdata,FUN=median)
colnames(median_income)=c('State','median.income')
summary(median_income)

custdata=merge(custdata,median_income,by.x='state.of.res',by.y='State')
summary(custdata)

custdata$income.norm=with(custdata,income/median.income)
summary(custdata$income.norm)

summary(custdata$age)
mean_age=mean(custdata$age)
custdata$age.norm=custdata$age/mean_age
summary(custdata$age.norm)
summary(scale(custdata$age))

sign(-3)

signedlog10=function(x){
  ifelse(abs(x)<=1,0,sign(x)*log10(abs(x)))
}

custdata$gp=runif(dim(custdata)[1])
test_set=subset(custdata,custdata$gp<=0.1)
dim(test_set)
