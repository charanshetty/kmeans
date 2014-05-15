#plotting the datapoints from normal distribution with 4 centroids
#1st plot of cluster
n = 110
g = 4 
set.seed(7)
d <- data.frame(x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))), 
                y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))
plot(d)


#plotting the datapoints from normal distribution with 1 centroid
#2nd plot for cluster
n = 110
g = 1 
set.seed(2)
d <- data.frame(x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))), 
                y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))
plot(d)

#elbow plot for cluster
data <- d
fvalue <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data,
                                     centers=i)$withinss)
plot(1:15, fvalue, type="b", xlab="Number of Clusters",
     ylab="sum square of difference between cluster c(i) and x(i)")
