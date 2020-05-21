#
# Lab 3: NCI60 Data Example
#
library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)
#
# Principal Components Analysis
#
pr.out <- prcomp(nci.data,scale=T)
# rainbow() takes an integer and returns a vector
# containing integer number of different colors
Cols <- function(vec) {
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
# Create Figure 10.15
par(mfrow=c(1,2))
plot(pr.out$x[,1:2],col=Cols(nci.labs),pch=19,xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)],col=Cols(nci.labs),pch=19,xlab="Z1",ylab="Z3")
# Examine the proportion of variance explained by each
# principal component.
summary(pr.out)
plot(pr.out)
pve <- 100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve,type="o",ylab="PVE",xlab="Principal Component",col="blue")
plot(cumsum(pve),type="o",ylab="Cumulative PVE",xlab="Principal Component",col="brown3")
#
# Clustering
#
sd.data <- scale(nci.data)
data.dist <- dist(sd.data)
# Create Figure 10.17
par(mfrow=c(1,1))
# The default for hclust is complete but I like to
# specify it anyway as documentation.
plot(hclust(data.dist,method="complete"),labels=nci.labs,main="Complete Linkage",xlab="",sub="",ylab="",cex=0.5)
plot(hclust(data.dist,method="average"),labels=nci.labs,main="Average Linkage",xlab="",sub="",ylab="",cex=0.5)
plot(hclust(data.dist,method="single"),labels=nci.labs,main="Single Linkage",xlab="",sub="",ylab="",cex=0.5)
# Use complete linkage and cut tree to obtain four
# clusters.
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out,4)
# Which labels appear in which clusters?
# There are many labels so I'm widening the display.
options(width=140)
table(hc.clusters,nci.labs)
plot(hc.out,labels=nci.labs,cex=0.5)
abline(h=139,col="red")
# Obtain a summary of the output of hclust()
hc.out
# Compare the result to K-means to see mild differences.
set.seed(2)
km.out <- kmeans(sd.data,4,nstart=20)
km.clusters <- km.out$cluster
table(km.clusters,hc.clusters)
# Hierarchical clustering on first five principal
# component scores vectors
hc.out <- hclust(dist(pr.out$x[,1:5]))
plot(hc.out,labels=nci.labs,main="Hier Clust on 1st Five Score Vectors",cex=0.5)
table(cutree(hc.out,4),nci.labs)
