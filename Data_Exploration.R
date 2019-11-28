library(ggplot2)
library(scales)
custdata=read.table('custdata.tsv',header = T,sep='\t')
str(custdata)
summary(custdata)

ggplot(custdata)+geom_histogram(aes(x=age),binwidth = 5, fill='gray')

ggplot(custdata)+geom_density(aes(x=income)) +
  scale_x_continuous(labels=dollar)

ggplot(custdata)+geom_density(aes(x=income)) +
  scale_x_log10(labels=dollar,breaks=c(100,1000,10000,100000))+
  annotation_logticks(sides='bt')

ggplot(custdata)+geom_bar(aes(x=state.of.res),fill='darkgray')+
  coord_flip()+
  theme(axis.text.y = element_text(size=rel(0.8)))

statesum=table(custdata$state.of.res)
statesum
statesf=as.data.frame(statesum)
statesf
colnames(statesf)=c('states.of.res','count')
summary(statesf)
statef=transform(statesf,states.of.res=reorder(states.of.res,count))
statef
summary(statef)

ggplot(statef)+ geom_bar(aes(x=states.of.res,y=count),stat='identity',fill='darkgray')+
  coord_flip()+
  theme(axis.text.y=element_text(size=rel(0.8)))

custdata2=subset(custdata,(custdata$income>0 & custdata$age>0 & custdata$age<100))
summary(custdata2)

ggplot(custdata2)+geom_point(aes(x=age,y=income))+
  ylim(0,200000)

ggplot(custdata2)+geom_point(aes(x=age,y=income))+
  ylim(0,200000) +stat_smooth(aes(x=age,y=income),method='lm')

ggplot(custdata2)+geom_point(aes(x=age,y=income))+
  ylim(0,200000) +geom_smooth(aes(x=age,y=income))

ggplot(custdata2,aes(x=age,y=as.numeric(health.ins)))+
  geom_point(position = position_jitter(w=0.05,h=0.05))+
  geom_smooth()

ggplot(custdata2,aes(x=age,y=income))+
  geom_hex(binwidth=c(5,10000))+ ylim(0,200000)+
  geom_smooth(color='white',se=F)

ggplot(custdata2)+
  geom_bar(aes(x=marital.stat,fill=health.ins))

ggplot(custdata2)+
  geom_bar(aes(x=marital.stat,fill=health.ins),position='dodge')

ggplot(custdata2)+
  geom_bar(aes(x=marital.stat,fill=health.ins),position='fill')

ggplot(custdata2)+
  geom_bar(aes(x=marital.stat,fill=health.ins),position='fill') +
  geom_point(aes(x=marital.stat,y=-0.05),size=0.8,alpha=0.3,position = position_jitter(h=0.01))

ggplot(custdata2)+
  geom_bar(aes(x=housing.type,,fill=marital.stat),position='dodge') +
  theme(axis.text.x=element_text(angle=30,hjust=01))

ggplot(custdata2)+
  geom_bar(aes(x=marital.stat),position = 'dodge')+
  facet_wrap(~housing.type,scales = 'free_y')+
  theme(axis.text.x=element_text(angle=45,hjust=1))
