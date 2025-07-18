urlRemote <- "https://raw.githubusercontent.com/"
pathGithub <- "EricBrownTTU/ISQS6350/main/"
filename <- "HeightWeight.csv"

hw <- read.csv(paste0(urlRemote, pathGithub, filename))

# get mean vector and covariance matrix
xbar <- colMeans(hw)
S <- cov(hw)

# compute the Mahalanobis distance for all observations
d2 <- mahalanobis(hw, xbar, S)

# identify outliers
out <- which(1 - pchisq(d2, 2) < .1)

# plot all points and mark outliers
plot(hw$Height, hw$Weight, xlab = "Height", ylab = "Weight")
points(hw$Height[out], hw$Weight[out], col = "red", pch = 19)