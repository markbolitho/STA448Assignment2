dat=read.csv("C:/Users/Mark/Downloads/Ass2data.csv")
# Question (2)
# (a)
# i) Sample Mean vector
colMeans(dat)
# ii) Sample variance-covariance matrix
var(dat)
# iii) Sample correlation matrix
cor(dat)
# (b) squared generalised distance for the first two observations
mahalanobis(dat,colMeans(dat),var(dat))[1:2]
# (c)
# Univariate outputs
# basic for loop, before starting loop par is called so it's possible to have
# all graphs in the one visual output
par(mfrow=c(2,2))
for (i in 1:4){
qqnorm(dat[,i],main=colnames(dat)[i])
qqline(dat[,i])
}
dev.off() # "turns off" the output screen so we don't continue to have all graphs in one output
# Bivariate outputs
pairs(dat)
# Multivariate outputs
library(MVN)
mvn(dat)
# This is to graph multivariate data. FALSE is passed to bypass the outlier removal prompt
library(mvoutlier)
chisq.plot(dat,ask=FALSE)
abline(0,1)
# (d)
# Trying two different approaches to the anaylsis of this data
# the first way, we are going to display a bunch of boxplots of the data
# another interesting way is by calculating the Mahalanobis distances of the dataset
# and then we will calculate p-values connected to those distances
# in general, if the p-values are less than 0.001 then we can consider if an outlier
par(mfrow=c(2,2))
for (i in 1:4){
boxplot(dat[,i],main=colnames(dat)[i])
}
dev.off()
# next we will do the second, numerical, test
dat$mahal <- mahalanobis(dat,colMeans(dat),var(dat))
dat$p <- pchisq(dat$mahal,df=3,lower.tail=FALSE)
dat
# the following commands is to "clean up" the data set, as we made changes to it
# we want to remove the columns afterwards to ensure nothing weird happens later
dat[5:6] <- NULL
# Question (3)
# (b) quick look at the data
head(dat)
# (c)
# i) the principal component loadings
dat_pc=princomp(dat,cor=T)
dat_pc$loadings
# ii) proportions of variances explained by each principal component
summary(dat_pc)
# (d) eigenvalues of the correlation matrix
eigen(cor(dat))$values # using the $values extra to eliminate the vectors from output
# (e) scree plot
plot(dat_pc,type="lines")
# (g) correlations between the principal components retained
cor(dat_pc$scores)
# (i) biplot
biplot(dat_pc)