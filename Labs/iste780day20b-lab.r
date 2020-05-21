#
# Lab 2: Clustering
#
# K-means clustering
set.seed(2)
x <- matrix(rnorm(50*2),ncol=2)
x[1:25,1] <- x[1:25,1]+3
x[1:25,2] <- x[1:25,2]-4
# K-means clustering with K=2
km.out <- kmeans(x,2,nstart=20)
km.out$cluster
plot(x,col=(km.out$cluster+1),main="K-means clustering results with K=2",xlab="",ylab="",pch=20,cex=2)
# Pretend we don't know there are two clusters
set.seed(4)
# Try multiple initial assignments by setting nstart=20
km.out <- kmeans(x,3,nstart=20)
km.out
plot(x,col=(km.out$cluster+1),main="K-means clustering results with K=3",xlab="",ylab="",pch=20,cex=2)
# Demonstrate the usefulness of nstart=20
set.seed(3)
# Compare the within-cluster sum of squares
km.out <- kmeans(x,3,nstart=1)
km.out$tot.withinss
km.out <- kmeans(x,3,nstart=20)
km.out$tot.withinss
#
# Hierarchical clustering
#
hc.complete <- hclust(dist(x),method="complete")
hc.average <- hclust(dist(x),method="average")
hc.single <- hclust(dist(x),method="single")
# Display dendrograms
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage",xlab="",sub="",cex=.9)
plot(hc.average,main="Average Linkage",xlab="",sub="",cex=.9)
plot(hc.single,main="Single Linkage",xlab="",sub="",cex=.9)
# Cut the trees
cutree(hc.complete,2)
cutree(hc.average,2)
cutree(hc.single,2)
# Single leads to non-useful answer so try cutting at a
# different level.
cutree(hc.single,4)
# Scale the variables before hierarchical clustering
xsc <- scale(x)
plot(hclust(dist(xsc),method="complete"),main="Hierarchical Clustering with Scaled Features",cex=0.7)
# Try it with correlation-based distance
x <- matrix(rnorm(30*3),ncol=3)
dd <- as.dist(1-cor(t(x)))
plot(hclust(dd,method="complete"),main="Complete Linkage with Correlation-Based Distance",xlab="",sub="",cex=0.7)
