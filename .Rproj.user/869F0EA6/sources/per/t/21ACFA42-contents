library(ggplot2)
library(fpc)

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

pfit <- hclust(dmatrix,method = 'ward')
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
  sum(apply(custermat,MARGIN = 1,FUN=function(clust){ sum((clust-c0)^2) }))
}

wss.tot <- function(dmatrix,labels){
  wsstot <- 0
  k <- length(unique(labels))
  for (i in 1:k){
    wsstot <- wsstot + subset(dmatrix,labels==i)
  }
  wsstot
}
