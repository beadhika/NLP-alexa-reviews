urlRemote <- "https://raw.githubusercontent.com/"
pathGithub <- "EricBrownTTU/ISQS6350/main/"
filename <- "crime.csv"
crime <- read.csv(paste0(urlRemote, pathGithub, filename))
crime[-1]
s <- cov(crime[-1])
u <- cov(crime[2:8])
s_eigen <- eigen(s)
lambda <- s_eigen$values
round(lambda,4)
ev <- s_eigen$vectors
ev


princomp(covmat=s)
crime_pca <- princomp(covmat=s)
crime_pca$loadings
summary(crime_pca)

crime.pca <- princomp(crime[2:8])
crime.pca$loadings
rownames(crime) <- crime[,1]
rownames(crime)
crime[-1]

crime.pca <- princomp(crime[-1])
biplot(crime.pca, col = c("black","red"), cex = 0.6)


# inclass assignment
crime_p <- cor(crime[-1])
crime_p
1-crime_p
crime.mds <- cmdscale(1-abs(crime_p)) #mds distance
crime.mds

plot(crime.mds,type="n")
text(crime.mds,labels=colnames(crime))

#hw2
urlremote <- urlRemote <- "https://raw.githubusercontent.com/"
pathGithub <- pathGithub <- "EricBrownTTU/ISQS6350/main/"
filename <- "fish.csv"

fish <- read.csv(paste0(urlremote, pathGithub, filename))
fish <- fish[,2:7]
fish <- fish[complete.cases(fish),]

xbar <- colMeans(fish)
S <- cov(fish)
d_mahal <- mahalanobis(fish, xbar, S)

# identify outliers
out <- which(1 - pchisq(d_mahal, 2) < 0.05)
out

library(ks)
# execute kde, the eval.points argument gives us density estimates at each of our raw data points, rather than using a grid
fish.kde <- kde(fish, eval.points = fish)

# identify outlier
out_kde <- which(fish.kde$estimate <= quantile(fish.kde$estimate, .37))

row.names(fish)


length(out)
length(out_kde)

intersect(out,out_kde)
length(intersect(out,out_kde))
fish_sc <- scale(fish)
out_kde
fish_outlier <- fish_sc[row.names(fish_sc) %in% out_kde,]


scale(fish[out_index,])
scale(fish[1,])
fish

urlremote <- urlRemote <- "https://raw.githubusercontent.com/"
pathGithub <- pathGithub <- "EricBrownTTU/ISQS6350/main/"
filename <- "protein.csv"

protein <- read.csv(paste0(urlremote, pathGithub, filename), row.names = "Country")
protein <- protein[,1:9]
protein

r <- cor(protein)
protein_pca <- princomp(covmat=r)
summary(protein_pca)
round(scale(protein),2)

# 
data("heptathlon", package = "HSAUR2")
head(heptathlon)

heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m
head(heptathlon)

heptathlon_sc <- scale(heptathlon)
d <- round(dist(heptathlon,diag=TRUE,upper=TRUE),2)

# Lets find the covariance matrix and its eigenvalues.
hep_s <- cov(heptathlon)
hep_eigen <- eigen(hep_s)
lambda <- hep_eigen$values

#
hep.mds <- cmdscale(d)
hep.mds
head(heptathlon[,1])
rownames(heptathlon)
plot(hep.mds, type = "n",
     xlab = "1st component", ylab = "2nd component",
     main = "MDS of heptathlon Data")
text(hep.mds, labels = rownames(heptathlon),cex = 0.6)

d_unscaled <- round(dist(heptathlon,diag=TRUE,upper=TRUE),2)
unscaled.mds <- cmdscale(d_unscaled )
plot(unscaled.mds, type = "n",
     xlab = "1st component", ylab = "2nd component",
     main = "MDS of heptathlon Data")
text(unscaled.mds, labels = rownames(heptathlon),cex = 0.6)

head(hep.mds)
head(unscaled.mds)

hep_r <- cor(heptathlon_sc)
hep_r
cor_dist <- 1-abs(hep_r)
cor_dist

cor.mds <- cmdscale(cor_dist)
plot(cor.mds, type = "n",
     xlab = "1st component", ylab = "2nd component",
     main = "MDS of correlation distance of heptathlon")
text(cor.mds, labels = colnames(heptathlon),cex = 0.6)
heptathlon

#p2
P2_cor <-matrix(c(1.000,0.402,0.396,0.301,0.305,0.339,0.340,
            0.402,1.000,0.618,0.150,0.135,0.206,0.183,
            0.396,0.618,1.000,0.321,0.289,0.363,0.345,
            0.301,0.150,0.321,1.000,0.846,0.759,0.661,
            0.305,0.135,0.289,0.846,1.000,0.797,0.800,
            0.339,0.206,0.363,0.759,0.797,1.000,0.736,
            0.340,0.183,0.345,0.661,0.800,0.736,1.000),nrow=7,byrow=TRUE)
P2_cor.pca <- princomp(covmat=P2_cor)
P2_cor.pca$loadings
summary(P2_cor.pca)
row.names(P2_cor) <-  c('Head length','Head breadth','Face breadth'
                        ,'Left finger length','Left forearm length',
                        'Left foot length','Height')

P2_cor.pca$loadings[,1]
?biplot
