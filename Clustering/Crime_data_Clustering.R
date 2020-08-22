crime_data<-read.csv(file.choose(),header=T)
View(crime_data)
head(crime_data)

#Normalize the data
normalized_crime_data<-scale(crime_data[2:5])
View((normalized_crime_data))
head(normalized_crime_data)

#here I am using hierarchical clustering 

#create distance matrix using Euclidean distance

distance<-dist(normalized_crime_data,method = "euclidean")

#create dendrogram using different linkage, and choose better linkage for clustering

# create dendrogram using single linkage
fit_crime_data1 <- hclust(distance, method="single") 
fit_crime_data1

#plot the dendrogram
plot(fit_crime_data1, hang=-1)

# using single, it is very difficult to interpret, so we would try different 
#linkages for better dendrogram

## create dendrogram using complete linkage
# create dendrogram using single linkage
fit_crime_data2 <- hclust(distance, method="complete") 
fit_crime_data2

#plot the dendrogram
plot(fit_crime_data2, hang=-1)

#using complete linkage we got better dendrogram

#lets try centroid linkage for creating dendroid
fit_crime_data3 <- hclust(distance, method="centroid") 
fit_crime_data3

#plot the dendrogram
plot(fit_crime_data3, hang=-1)

# using average linkages
fit_crime_data4 <- hclust(distance, method="average") 
fit_crime_data4

#plot the dendrogram
plot(fit_crime_data4, hang=-1)

#here complete linkage provide a simple dendrogram
#so we can choose fit_crime_data2 for further processing

#next step is to cut the dedrogram
#here i am going to  cut dendrogram into four cluster

crime_groups <- cutree(fit_crime_data2, k=4)# cut tree into 4 clusters

rect.hclust(fit_crime_data2, k=4, border="red")

#convert groups information into a matrix for better understanding
crime_cluster<-as.matrix(crime_groups)
View(crime_cluster)

#create dataframe to combine crime_cluster and original data
data <- data.frame(crime_data, crime_cluster)
View(data)
head(data)

#here i am going to change the position of the column crime_cluster in to first
final_data <- data[,c(ncol(data),1:(ncol(data)-1))]
View(final_data)
head(final_data)

#here I am changing second column name as State for better analysis
names(final_data)[2]<-"State"
View(final_data)
head(final_data)

#display the states of each cluster

crime_cluster1 <-subset(final_data,crime_cluster==1)
crime_cluster1$State

crime_cluster2 <-subset(final_data,crime_cluster==2)
crime_cluster2$State

crime_cluster3 <-subset(final_data,crime_cluster==3)
crime_cluster3$State

crime_cluster4 <-subset(final_data,crime_cluster==4)
crime_cluster4$State

#display the average murder rate of each cluster
tapply(final_data$Murder,crime_cluster,mean)

#here we can see that cluster 1 has highest murder rate

#display average Assualt rate of each cluster
tapply(final_data$Assault,crime_cluster,mean)

#here cluster2 has highest assult rate and cluster 4 has lowest assault rate

#display average urban population of each cluster
tapply(final_data$UrbanPop,crime_cluster,mean)

#here  cluster 2 has highest urben population and cluster 4 has lowest

#display average Rape rate in different cluster
tapply(final_data$Rape,crime_cluster,mean)

##here  cluster 2 has highest rape rate

