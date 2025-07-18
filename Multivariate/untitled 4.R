final <- read.csv("C:/Users/benja/OneDrive/Desktop/Group7-Final-Submission/BI project files/final.csv")
final_factors <- factanal(covmat = cov(final[7:13]), factors = 2)
final_pca <- princomp(covmat=cor(final[7:13]))

head(iris)
mydata <- iris[,-5]
plot(mydata,col=iris$Species)

#
x <- rnorm(50,4,3)
y <- rnorm(50,8,3)
c1 <- rep(1,50)
d1 <- cbind(c1,x,y)
x <- rnorm(50,8,3)
y <- rnorm(50,0,3)
c2 <- rep(2,50)
d2 <- cbind(c2,x,y)
x <- rnorm(50,0,3)
y <- rnorm(50,0,3)
c3 <- rep(3,50)
d3 <- cbind(c3,x,y)
mydata3 <- rbind(d1,d2,d3)
mydata3 <- data.frame(mydata3)
colnames(mydata3) <- c("True.Cluster", "x", "y")

plot(mydata3[,2:3], col = mydata3$True.Cluster,
     main = "True Clusters")

#
library(mclust)

head(iris)
mbc.i <- Mclust(iris[,1:4],3)
table(mbc.i$classification,iris$Species)
