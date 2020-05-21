##
## Lab 1: Principal Components Analysis
##
system("clear")
USArrests
## n=50, p=4
states <- row.names(USArrests)
states
names(USArrests)
## apply() applies a function to data
## 1 means to apply to rows, 2 means to apply to columns
apply(USArrests,2,mean)
apply(USArrests,2,var)
## Assaults dominate the data, so scaling will make a big
## difference.
pr.out <- prcomp(USArrests,scale=T)
names(pr.out)
## center => means of raw vars
## scale  => standard deviations of raw vars
pr.out$center
pr.out$scale
## rotation => principal component loadings vectors
pr.out$rotation
## x => principal component scores vectors
dim(pr.out$x)
## biplot() of loadings and scores
biplot(pr.out,scale=0,cex=0.5)
## Make it look just like Figure 10.1
pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x
biplot(pr.out,scale=0)
## Standard deviation of each principal component
pr.out$sdev
pr.var <- pr.out$sdev^2
pr.var
## Find the proportion of variance explained by each
## principal component.
pve <- pr.var/sum(pr.var)
pve
## Reproduce Figure 10.4
par(mfrow=c(1,2))
plot(pve,xlab="Principal Component",ylab="Proportion of Variance Explained",ylim=c(0,1),type="b")
plot(cumsum(pve),xlab="Principal Component",ylab="Cumulative Proportion of Variance Explained",ylim=c(0,1),type="b")
## cumsum() computes the cumulative sum of a numeric
## vector. Here's an example.
a <- c(1,2,8,-3)
cumsum(a)
