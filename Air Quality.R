library(datasets)
head(airquality)
data(airquality) 
attach(airquality)
head(airquality,10)
summary(airquality)

month7=subset(airquality,Month=7)
              month8=subset(airquality,Month=8)
              month9=subset(airquality,Month=9)
              par(mfrow = c(1,2))  # 3 rows and 2 columns
             
boxplot(month7$Temp~airquality$Day)
boxplot(month8$Temp~airquality$Day)
boxplot(month9$Temp~airquality$Day)

hist(airquality$Temp)

plot(airquality$Temp~airquality$Day+airquality$Solar.R+airquality$Wind+airquality$Ozone)

coplot(Ozone~Solar.R|Wind,panel=panel.smooth,airquality)



data("airquality")
str(airquality)

head(airquality)

col1<- mapply(anyNA,airquality) # apply function anyNA() on all columns of airquality dataset
col1

# Impute monthly mean in Ozone
for (i in 1:nrow(airquality)){
  if(is.na(airquality[i,"Ozone"])){
    airquality[i,"Ozone"]<- mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Ozone"],na.rm = TRUE)
  }
  # Impute monthly mean in Solar.R
  if(is.na(airquality[i,"Solar.R"])){
    airquality[i,"Solar.R"]<- mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Solar.R"],na.rm = TRUE)
  }
  
}
#Normalize the dataset so that no particular attribute has more impact on clustering algorithm than others.
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
airquality<- normalize(airquality) # replace contents of dataset with normalized values

result<- kmeans(airquality[c(1,2,3,4)],3) # apply k-means algorithmusing first 4 attributes and with k=3(no. of required clusters)
result$size # gives no. of records in each cluster

result$centers # gives value of cluster center datapoint value(3 centers for k=3)

result$cluster #gives cluster vector showing the custer where each record falls

plot(airquality[,1:2], col=result$cluster) # Plot to see how Ozone and Solar.R data points have been distributed in clusters

plot(airquality[,3:4], col=result$cluster) # Plot to see how Wind and Temp data points have been distributed in clusters

plot(airquality[,], col=result$cluster) # Plot to see all attribute combinations
