#Clustering using principal companent analysis
#here using both hierarchical and k means clustering

#Loading the data
wine<-read.csv(file.choose(),header=T)
View(wine)

#display first few data in a data set
head(wine)

#check if any null values present in the data set
sum(is.na(wine))
#there is no null value present in the data set

#display the structure of the data set
str(wine)

#display the column names of the data set
colnames(wine)

#convert the variables in to a pricipal component  objects using principal component analysis(PCA)
## here we remove 'Type' variable from our data set

pcaObj<-princomp(wine[-1], cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaObj)

# here creating 13 proncipal component object

help(loadings)

#showing the weights of the variable using following code
loadings(pcaObj)

# graph showing importance of principal components
plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

#to check the scores of  principal components
head(pcaObj$scores) 

# Top 3 PCA Scores which represents the whole data
pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole data
head(pcaObj$scores[,1:3])

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with wine data
wine<-cbind(wine,pcaObj$scores[,1:3])
View(wine)
head(wine)

# preparing data for clustering (considering only pca scores as they represent the entire data)
colnames(wine)
clus_data<-wine[,15:17]
head(clus_data)

# Normalizing the data
# Scale function is used to normalize data.but here we dont need to convert
#data into a normal form, because we already scale data using covarience matrix
#norm_clus<-scale(clus_data) 
wine_distance<-dist(clus_data,method = "euclidean") # method for finding the distance

###########################################################

#     HIERARCHICAL CLUSTERING
#############################################3

# Clustering the data using hclust function --> Hierarchical clustering

# method here is complete linkage
fit1<-hclust(wine_distance,method="complete")
plot(fit1, hang=-1) # Displaying Dendrogram

# method here is single linkage
fit2<-hclust(wine_distance,method="single") 
plot(fit2, hang=-1) # Displaying Dendrogram

# method here is ward.D2 linkage
fit3<-hclust(wine_distance,method="ward.D2")
plot(fit3, hang=-1) # Displaying Dendrogram

# method here is average linkage
fit4<-hclust(wine_distance,method="average")
plot(fit4, hang=-1) # Displaying Dendrogram

# method here is centroid linkage
fit5<-hclust(wine_distance,method="centroid")
plot(fit5, hang=-1) # Displaying Dendrogram

##using complete linkage we got better dendrogram

#next step is to cut the dedrogram
#here i am going to  cut dendrogram into three cluster

wine_groups <- cutree(fit1, k=3)# cut tree into 5 clusters

rect.hclust(fit1, k=3, border="red")

#convert groups information into a matrix for better understanding
wine_cluster<-as.matrix(wine_groups)
View(wine_cluster)
head(wine_cluster)

#create dataframe to combine wine cluster and original data
final_data <- data.frame(wine, wine_cluster)
View(final_data)
head(final_data)

#here i am going to change the position of the column cluster in to first
final_data1 <- final_data[,c(ncol(final_data),1:(ncol(final_data)-1))]
View(final_data1)
head(final_data1)

colnames(final_data1)

## Inferences can be drawn from the aggregate of the universities data on wine_cluster
View(aggregate(final_data1[,-c(2,16:18)],by=list(wine_cluster),FUN=mean))

#view a data frame that contain original cluster and predicted cluster
Type<- wine$Type
head(data.frame(Type,wine_cluster))

#########################################################################

#       K-MEANS CLUSTERING
########################################################################

#lets create clusters using kmeans function
#initially we are going to create three clusters, k is the number of clusters
wine_fit1 <- kmeans(clus_data, 3) # 3 cluster solution
str(wine_fit1)
#here we have total withiness between the observation and cluster centre should be very small
#and betweenss between the observation and cluster centre should be very large
#our objective of k means clustering into keep these values large and small
#so we have to try different k values to get higher betweeness and fewer withiness between the clusters

#elbow curve & k ~ sqrt(n/2) to decide the k value
install.packages("factoextra")
library(factoextra)

clus_data
#use thest clus_data foe creating Elbow curve

#install.packages("factoextra")
library(factoextra)

fviz_nbclust(clus_data,kmeans,method="wss")+labs(subtitle = "Elbow method")

#the elbow curve we can see that 10 data points first data pont contain a big slope
#similarly data points2,3,4,5,6 also contain some slope.
#but the data points 7,8,9,10 contains no slope. this is our cutoff
# we can try k=7,8,9,10, which k value will give the result of smaller withiness and higher betweeness
#considerd as final fit


wine_fit2 <- kmeans(clus_data, 7) # 7 cluster solution
str(wine_fit2)
#tot.withinss: num 313
#betweenss   : num 1227

wine_fit3 <- kmeans(clus_data, 8) # 7 cluster solution
str(wine_fit3)
#tot.withinss: num 271
#betweenss   : num 1268

wine_fit4 <- kmeans(clus_data, 9) # 7 cluster solution
str(wine_fit4)
#tot.withinss: num 248
#betweenss   : num 1291

wine_fit5 <- kmeans(clus_data, 10) # 7 cluster solution
str(wine_fit5)
#tot.withinss: num 237
#betweenss   : num 1303
##here k=10, we got small withiness and higher betweeness

#create final clusters using wine_fit5$cluster
wine_final<- data.frame(wine[-1], wine_fit5$cluster) # append cluster membership
View(wine_final)
head(wine_final)

#find the average values of each clusters
View(aggregate(wine_final[,-c(2,16:18)],by=list(wine_fit5$cluster),FUN=mean))

#in our problem given three types of wine
#so here i am going to construct three clusters
#compare the predicted three clusters into original cluster

predicted_clust<-kmeans(clus_data,3)
str(predicted_clust)

#compare with original cluster
original_clust<-wine$Type
View(data.frame(original_clust,predicted_clust$cluster))

#here we can see that original cluster and predicted clustes are different in case of k=3

