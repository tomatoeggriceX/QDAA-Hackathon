# required libraries: tidyverse

customerData <- read.csv("C:/Users/Gary/Documents/2021/Winter 2021/QDAA Hackathon/Customers.csv")

# Sort the customers by DOB (year) and plot into a bar graph
# currently lightblue
customerData[order(as.Date(customerData$DOB, format = "%m/%d/%Y")),]
a <- customerData$DOB
yearDOB <- as.integer(substring(a, 7, 10))
customerDataYOB <- mutate(customerData, yearDOB)
customerDataYOBPlot <-ggplot(customerData, aes(x = yearDOB)) + geom_bar(fill = "lightblue")
customerDataYOBPlot + coord_cartesian(ylim=c(200, 300))

# Sort the customers by city
cityCode <- customerData$city_code
cityCodePlot <- ggplot(customerData, aes(x = city_code)) + geom_bar(fill = "pink")
cityCodePlot + scale_x_continuous(breaks = seq(0, 10, by = 1))
cityCodePlot + coord_cartesian(ylim = c(500, 600))

# Sort the customers by gender
customerGender <- customerData$Gender
customerDataForGender <- customerData
customerDataForGender[customerDataForGender[,3] == '',3]<- NA
customerDataForGender <- na.omit(customerDataForGender)
customerGenderPlot <- ggplot(customerDataForGender, aes(x = Gender)) + geom_bar(fill = "thistle3")
customerGenderPlot + scale_x_discrete(breaks = c("F", "M"),
  labels = c("Female", "Male"))
customerGenderPlot + coord_cartesian(ylim = c(2500, 3000))
nrow(subset(customerDataForGender, Gender == "F"))
# Standardizing Data Type to be all integer types

customerDataYOB <- mutate(customerDataYOB, DOB = NULL)
customerDataYOB$Gender[customerDataYOB$Gender == 'M'] <- as.integer("0")
customerDataYOB$Gender[customerDataYOB$Gender == 'F'] <- as.integer("1")
customerDataYOB[customerDataYOB[,2] =='',2] <- NA

# Clustering the Data by consumer ID
# Clustering together DOB, Gender and City Code
# K means cluster

view(customerDataYOB)

# omit missing data from customerDataYOB
customerDataYOBCluster <- na.omit(customerDataYOB)
view(customerDataYOBCluster)
customerDataYOBCluster$Gender <- as.integer(customerDataYOBCluster$Gender)

# WSS plot function

wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data, 2, var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")
}


wssplot(customerDataYOBCluster)

#optimum number of clusters is 2

# K means cluster analysis
KM = kmeans(customerDataYOBCluster, 2)

# cluster plot
autoplot(KM, customerDataYOBCluster, frame = TRUE)

# cluster centres
KM$centres

