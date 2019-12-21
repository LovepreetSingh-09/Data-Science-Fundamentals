library(ggplot2)
library(fpc)
library(reshape2)

protein <- read.table('protein.txt',sep='\t',header = T)
str(protein)

var_to_use <-colnames(protein)[-1]
pmatrix <- scale(protein[,var_to_use])
pcenter <- attr(pmatrix,'scaled:center')
pscae <- attr(pmatrix,'scaled:scale')

help(dist)
dmatrix <- dist(pmatrix,method='euclidean')
dmatrix
str(dmatrix)

p <- hclust(dmatrix,method = 'ward')
summary(p)
plot(p,labels=protein$Country)
rect.hclust(p,k=5)

groups <- cutree(p,k=5)
groups


print_clusters <- function(labels,k) {
  for (i in 1:k) {
    print(paste('Cluster: ',i))
    print(protein[labels==i,c("Country","RedMeat","Fish","Fr.Veg")])
  }
}

print_clusters(groups,5)

princ <- prcomp(pmatrix)
ncomp <- 2
project <- predict(princ,pmatrix)[,1:ncomp]
project
project.plus <- cbind(data.frame(project),clusters=as.factor(groups),country=protein$Country)
project.plus
ggplot(project.plus,aes(x=PC1,y=PC2))+
  geom_point(aes(shape=clusters))+
  geom_text(aes(label=country),hjust=0,vjust=1)

k.bestp=5
cboot.hclust <- clusterboot(pmatrix,clustermethod = hclustCBI,method='ward.D',k=k.bestp)
summary(cboot.hclust)

cboot.hclust$result$
cboot.hclust$partition
cboot.hclust$bootmean
# no of times clusters dissolved
cboot.hclust$bootbrd
print_clusters(cboot.hclust$partition,k.bestp)

sqr_eqdist <- function(x,y){
  sum((x-y)^2)
}

wss.cluster <- function(clustermat){
  c0 <- apply(clustermat,MARGIN = 2,FUN=mean)
  sum(apply(clustermat,MARGIN = 1,FUN=function(clust){ sum((clust-c0)^2) }))
}

wss.tot <- function(dmatrix,labels){
  wsstot <- 0
  k <- length(unique(labels))
  for (i in 1:k){
    wsstot <- wsstot + wss.cluster(subset(dmatrix,labels==i))
  }
  wsstot
}

totss <- function(dmatrix){
  grandmean <- apply(dmatrix, 2, FUN = mean)
  sum(apply(dmatrix,1,FUN <- function(x){sqr_eqdist(x,grandmean)}))
}

ch_criterion <- function(dmatrix,kmax,methods=c('kmeans','hclust')){
  if(!(methods %in% c('kmeans','hclust'))){
    stop("method must be one of c('kmeans', 'hclust')")
  }
  npts <- dim(dmatrix)[[1]]
  totss <- totss(dmatrix)
  wss <- numeric(kmax)
  crit <- numeric(kmax)
  wss[1] <- (npts-1)*sum(apply(dmatrix,2,var))
  for (i in 2:kmax){
    if (methods=='kmeans'){
      clustering <- kmeans(dmatrix,k,nstart=10,iter.max = 100)
    wss[i] <- clustering$tot.withinss
    }
    else{
      d=dist(dmatrix)
      clustering <- hclust(d,method='ward.D2')
      labels <- cutree(clustering,k=i)
      wss[i] <- wss.tot(dmatrix,labels)
    }
  }
  bss <- totss - wss
  crit.num <- bss/(0:(kmax -1))
  npts=30
  crit.denom <- wss/(npts-1:kmax)
  list(crit = crit.num/crit.denom, wss = wss, totss = totss)
}

clustcrit <- ch_criterion(pmatrix, 10, method="hclust")
critframe <- data.frame(k=1:10, ch=scale(clustcrit$crit),wss=scale(clustcrit$wss))
critframe <- melt(critframe, id.vars=c("k"),variable.name="measure",value.name="score")
ggplot(critframe, aes(x=k, y=score, color=measure)) +
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks=1:10, labels=1:10)


pclusters <- kmeans(pmatrix, 5, nstart=100, iter.max=100)
summary(pclusters)
pclusters$centers
pclusters$size
groups <- pclusters$cluster
print_clusters(groups,5)

clustering.ch <- kmeansruns(pmatrix, krange=1:10, criterion="ch")
clustering.ch$bestk
clustering.asw <- kmeansruns(pmatrix, krange=1:10, criterion="asw")
clustering.asw$bestk
clustering.ch$crit
clustcrit$crit
critframe <- data.frame(k=1:10, ch=scale(clustering.ch$crit),asw=scale(clustering.asw$crit))
critframe <- melt(critframe, id.vars=c("k"),variable.name="measure",value.name="score")
ggplot(critframe, aes(x=k, y=score, color=measure)) +
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks=1:10, labels=1:10)
summary(clustering.ch)

kbest.p<-5
cboot<-clusterboot(pmatrix, clustermethod=kmeansCBI, runs=100,iter.max=100,krange=kbest.p, seed=15555)
groups <- cboot$result$partition
print_clusters(cboot$result$partition, kbest.p)

assign_clusters <- function(newpt,centers,xcenter=0,xscale=1){
  pt <- (newpt-xcenter)/xscale
  dists <- apply(centers,1,FUN=function(row){ sqr_eqdist(row,pt) })
  which.min(dists)
}

rnorm.multidim <- function(n,mean,sd,colstr='x'){
  ndim <- length(mean)
  data <- NULL
  for (i in 1:ndim){
    col <- rnorm(n,mean=mean[i],sd=sd[i])
    data <- cbind(data,col)
  }
  cnames <- paste(colstr,1:ndim,sep='')
  colnames(data) <- cnames 
  data
}

mean1 <- c(1, 1, 1)
sd1 <- c(1, 2, 1)
mean2 <- c(10, -3, 5)
sd2 <- c(2, 1, 2)
mean3 <- c(-5, -5, -5)
sd3 <- c(1.5, 2, 1)
clust1 <- rnorm.multidim(100, mean1, sd1)
clust2 <- rnorm.multidim(100, mean2, sd2)
clust3 <- rnorm.multidim(100, mean3, sd3)
toydata <- rbind(clust1,rbind(clust2,clust3))
tmatrix <- scale(toydata)
tcenter <- attr(tmatrix,'scaled:center')
tscale <- attr(tmatrix,'scaled:scale')

kbest.t <- 3
tclusters <- kmeans(tmatrix, kbest.t, nstart=100, iter.max=100)
summary(tclusters)
tclusters$size
unscale <- function(x,mean,sd){
  x*sd+mean
}

unscale(tclusters$centers[1,],tcenter,tscale)
unscale(tclusters$centers[2,], tcenter, tscale)
unscale(tclusters$centers[3,], tcenter, tscale)

assign_clusters(rnorm.multidim(1, mean1, sd1), tclusters$centers, tcenter, tscale)
assign_clusters(rnorm.multidim(1, mean2, sd2), tclusters$centers, tcenter, tscale)
assign_clusters(rnorm.multidim(1, mean3, sd3), tclusters$centers, tcenter, tscale)
