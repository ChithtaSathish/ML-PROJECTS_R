install.packages("readxl")
library(readxl)

data <- read_xlsx(file.choose(),2)# Read xlsx file 
View(data)
head(data)

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
summary(normalized_Airline_data)

#lets create clusters using kmeans function
#initially we are going to create three clusters, k is the number of clusters
fit1 <- kmeans(normalized_Airline_data, 3) # 3 cluster solution
str(fit1)
#here we have total withiness between the observation and cluster centre should be very small
#and betweenss between the observation and cluster centre should be very large
#our objective of k means clustering into keep these values large and small
#so we have to try different k values to get higher betweeness and fewer withiness between the clusters

#elbow curve & k ~ sqrt(n/2) to decide the k value
install.packages("factoextra")
library(factoextra)
fviz_nbclust(data[2:11],kmeans,method="wss")+labs(subtitle = "Elbow method")

#the elbow curve we can see that 10 data points first data pont contain a big slope
#similarly data points2,3,4,5,6 also contain some slope.
#but the data points 7,8,9,10 contains no slope. this is our cutoff
# we can try k=7,8,9,10, which k value will give the result of smaller withiness and higher betweeness
#considerd as final fit

fit2 <- kmeans(normalized_Airline_data, 7) # 7 cluster solution
str(fit2)
#tot.withinss: num 18002
#betweenss   : num 21978

fit3 <- kmeans(normalized_Airline_data, 8) # 8 cluster solution
str(fit3)
#tot.withinss: num 17177
# betweenss   : num 22803

fit4 <- kmeans(normalized_Airline_data, 9) # 9 cluster solution
str(fit4)
#tot.withinss: num 13775
# betweenss   : num 26205

fit5 <- kmeans(normalized_Airline_data, 10) # 10 cluster solution
str(fit5)
#tot.withinss: num 12826
# betweenss   : num 27154

#here k=10, we got small withiness and higher betweeness

#to view centers of each clusters using following methods
fit5$centers

#create final clusters using fit5$cluster
final<- data.frame(data, fit5$cluster) # append cluster membership
View(final)
head(final)

#change the position fit$cluster last to first
final_Airline_data <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final_Airline_data)
head(final_Airline_data)

#find the average values of each clusters
aggregate(data[,2:11], by=list(fit5$cluster), FUN=mean)

###VISUALIZATION

# k=3
eclust(normalized_Airline_data, "kmeans", k = 3, nstart = 25, graph = TRUE)

#k=5
eclust(normalized_Airline_data, "kmeans", k = 5, nstart = 25, graph = TRUE)

#k=10
eclust(normalized_Airline_data, "kmeans", k = 10, nstart = 25, graph = TRUE)
