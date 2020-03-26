university <- Lesson.12.Clustering..Ex.1
head(university,5)
# converting all columns to Z-scores
stand_uni <- scale(university[,-1])

stand_uni

# deriving the distance values
d <- dist(stand_uni, method = 'euclidean')

d

# performaning hierarchical clustering

fit <- hclust(d, method = 'complete')
plot(fit)

plot(fit,labels=university[,1])

# Creating group of clusters based on defined parameters
groups <- cutree(fit,k=3)
groups
rect.hclust(fit,k=3,border = 'red')

membership <- matrix(groups)
membership

hclust_uni <- data.frame(university,membership)
hclust_uni

#cluster Profiling
tapply(hclust_uni$Expenses,hclust_uni$membership,min)
tapply(hclust_uni$Expenses,hclust_uni$membership,max)
tapply(hclust_uni$Expenses,hclust_uni$membership,mean)

# Defining Cluster
# 3 - where min exp is 8704, max = 25026, mean = 13384
# 2 - where min exp = 30220, max= 63575, mean = 41176
# with the above, the student can compare amongst clusters and decide on whic one to attend.

# Using aggregate function to group / define clusters
aggregate(hclust_uni[,2:7],list(membership),FUN = min)
aggregate(hclust_uni[,2:7],list(membership),FUN = max)
aggregate(hclust_uni[,2:7],list(membership),FUN = mean)

# Deciding the number of clusters
library('factoextra')

fviz_nbclust(hclust_uni,FUNcluster = hcut,method = 'wss')

#finding the optimal number of clusters so as to reduce SSR

# Using Kmeans

fit_km  <- kmeans(stand_uni,3)
fit_km$cluster
data.frame(university,matrix(fit_km$cluster))
stand_uni
