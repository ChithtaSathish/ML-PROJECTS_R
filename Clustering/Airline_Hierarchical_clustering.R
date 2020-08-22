#using readx1 package for reading excel file
install.packages("readxl")
library(readxl)

data <- read_xlsx(file.choose(),2)# Read xlsx file 
View(data)
head(data)
sum(is.na(data))

#display the structure of the data
str(data)

# here we can see that variable cc1_miles,cc2_miles,cc3_miles and Award? are categorical form
#1 = under 5,000
#2 = 5,000 - 10,000
#3 = 10,001 - 25,000
#4 = 25,001 - 50,000
#5 = over 50,000
# we have to take average of cc1_miles,cc2_miles,cc3_miles above respective range
data$cc1_miles = ifelse(data$cc1_miles==1,3500,
                        ifelse(data$cc1_miles==2,8000,
                               ifelse(data$cc1_miles==3,20000,
                                      ifelse(data$cc1_miles==4,38000,
                                             ifelse(data$cc1_miles==5,65000,0)))))

data$cc2_miles = ifelse(data$cc2_miles==1,3500,
                        ifelse(data$cc2_miles==2,8000,
                               ifelse(data$cc2_miles==3,20000,
                                      ifelse(data$cc2_miles==4,38000,
                                             ifelse(data$cc2_miles==5,65000,0)))))


data$cc3_miles = ifelse(data$cc3_miles==1,3500,
                        ifelse(data$cc3_miles==2,8000,
                               ifelse(data$cc3_miles==3,20000,
                                      ifelse(data$cc3_miles==4,38000,
                                             ifelse(data$cc3_miles==5,65000,0)))))
View(data)
head(data)

#Normalize the data
#here i am taking 2 to 11 columns

normalized_Airline_data <- scale(data[2:11]) #excluding the ID and Award column before normalizing
View(normalized_Airline_data)
head(normalized_Airline_data)

#create distance matrix using Euclidean distance

distance<-dist(normalized_Airline_data,method = "euclidean")

# create dendrogram  using complete linkage

fit_Airline_data1 <- hclust(distance, method="complete") 
fit_Airline_data1

#plot the dendrogram
plot(fit_Airline_data1, hang=-1)

# using complete, it is very difficult to interpret, so we would try different 
#linkages for better dendrogram

# next i am using single linkages
fit_Airline_data2 <- hclust(distance, method="single") 
fit_Airline_data2

#plot the dendrogram
plot(fit_Airline_data2, hang=-1)

# second dendrogram using single linkage is also very difficult to interpret

# using average linkages
fit_Airline_data3 <- hclust(distance, method="average") 
fit_Airline_data3

#plot the dendrogram
plot(fit_Airline_data3, hang=-1)

#using average linkage we got complex dendrogram


#using centroid linkage
fit_Airline_data4 <- hclust(distance, method="centroid") 
fit_Airline_data4

#plot the dendrogram
plot(fit_Airline_data4, hang=-1)

#using centriod linkage we got complex dendrogram

#Ward's minimum variance method: It minimizes the total within-cluster variance. 
#At each step the pair of clusters with minimum between-cluster distance are merged.

fit_Airline_data5 <- hclust(distance, method="ward.D2") 
fit_Airline_data5

#plot the dendrogram
plot(fit_Airline_data5, hang=-1,)

#using ward.D2 linkage we got better dendrogram

#next step is to cut the dedrogram
#here i am going to  cut dendrogram into five cluster

Airline_groups <- cutree(fit_Airline_data5, k=5)# cut tree into 5 clusters

rect.hclust(fit_Airline_data5, k=5, border="red")

#convert groups information into a matrix for better understanding
Airline_cluster<-as.matrix(Airline_groups)
View(Airline_cluster)
head(Airline_cluster)

#create dataframe to combine membership and original data
final_data <- data.frame(data, Airline_cluster)
View(final_data)
head(final_data)

#here i am going to change the position of the column Airline_cluster in to first
final_data1 <- final_data[,c(ncol(final_data),1:(ncol(final_data)-1))]
View(final_data1)
head(final_data1)

#display first  10 ID of each clusters

cluster1 <-subset(final_data1,Airline_cluster==1)
cluster1$ID.[1:10]

cluster2 <-subset(final_data1,Airline_cluster==2)
cluster2$ID.[1:10]

cluster3 <-subset(final_data1,Airline_cluster==3)
cluster3$ID.[1:10]

cluster4 <-subset(final_data1,Airline_cluster==4)
cluster4$ID.[1:10]

cluster5 <-subset(final_data1,Airline_cluster==5)
cluster5$ID.

#display the average of  the Number of flight miles in the past 12 months of each cluster
tapply(final_data1$Flight_miles_12mo,Airline_cluster,mean)
#here we can see that second cluster has the highest average of the Number of flight miles in the past 12 months

#Award--whether that person had award flight (free flight) or not
tapply(final_data1$Award.,Airline_cluster,sum)
#here we can see that first cluster passanger has got more awards

 
