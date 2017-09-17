#packages needed

library(plyr)
library(ggplot2)
library("readr")

#Import the Land registry data
inputdata <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2016.txt"
names <- c('Transaction_unique_identifier', 'Price', 'Date_of_Transfer', 'Postcode', 'Property_Type', 'Old_New', 'Duration', 'PAON', 'SAON', 'Street', 'Locality', 'Town_City', 'District', 'County', 'PPDCategory_Type', 'Record_Status_monthly_file_only')
landregistry <- read_csv(inputdata, col_names=names)

#Remove the hour and minutes from the date variable and create a month variable
landregistry$Transfer_Date <- as.Date(as.POSIXct(landregistry$Date_of_Transfer,tz="", "%Y-%m-%d %H:%M"))
landregistry$Transfer_Month <- as.Date(paste((format(landregistry$Transfer_Date, "%Y-%m")),"-01",sep=""))
landregistry$Transfer_Year <- as.numeric(format(landregistry$Transfer_Date, "%Y"))

#Remove rows where the value is over £1M, these will skew the model later
landregistry <- landregistry[!(landregistry$Price>1000000),]

#I want just sales from 2016
sales2016 <- landregistry[ which(landregistry$Transfer_Year==2016), ]

#Keep only the variables I want to use
columns <- c('Price', 'Transfer_Month', 'Property_Type', 'Old_New', 'Duration', 'Town_City', 'District', 'County')
Initialdata <- subset(sales2016,select=columns)

# Create random 70% sample
#Set the random seed
set.seed(100)

#Randomly pick 70% of the data
index <- sample(1:nrow(Initialdata),size = 0.7*nrow(Initialdata)) 

#Select the 70% subset, and the testing dataset (30%) and only keep the columns I want
cols <- c("Price", "Transfer_Month", "Property_Type", "Old_New", 'Duration', "Town_City", "District", "County")
training <- sales2016[index, cols]
test <- sales2016 [-index, cols] 


#Initial data exploration

#Simple function to calculate sales and average price by a given factor
calc_Average <- function(dataframe, variable){
  Average <<- ddply(dataframe, variable, summarise, 
                    Sales         = length(Price), 
                    Average_Price = mean(Price)
  )
  return(Average)
}

#Calculate and plot averages
calc_Average(training, "Transfer_Month")
ggplot(data=Average, aes(x=Transfer_Month, y=Average_Price)) + geom_line(stat="identity")

calc_Average(training, "Property_Type")
ggplot(data=Average, aes(x=Property_Type, y=Average_Price)) + geom_bar(stat="identity")

calc_Average(training, "Old_New")
ggplot(data=Average, aes(x=Old_New, y=Average_Price)) + geom_bar(stat="identity")

calc_Average(training, "County")
ggplot(data=Average, aes(x=County, y=Average_Price)) + geom_bar(stat="identity")

#Or I could have done it this way
ggplot(training, aes(x=Duration, y=Price)) + stat_summary(fun.y="mean", geom="bar")

#Baseline model
best.guess <- mean(training$Price)
RMSE.baseline <- sqrt(mean((best.guess-test$Price)^2))

#Create a simple model
GLM1 <- glm(Price ~ Property_Type+Old_New, data=training, family=poisson())
GLM2 <- glm(Price ~ Property_Type+Duration, data=training, family=poisson())
GLM3 <- glm(Price ~ Old_New+Duration, data=training, family=poisson())
GLM4 <- glm(Price ~ Property_Type+Old_New+Duration, data=training, family=poisson())

#Test the glm on the test data
GLM1.pred <- predict(GLM1, test, type='response')
RMSE.GLM1 <- sqrt(mean((GLM1.pred-test$Price)^2))

GLM2.pred <- predict(GLM2, test, type='response')
RMSE.GLM2 <- sqrt(mean((GLM2.pred-test$Price)^2))

GLM3.pred <- predict(GLM3, test, type='response')
RMSE.GLM3 <- sqrt(mean((GLM3.pred-test$Price)^2))

GLM4.pred <- predict(GLM4, test, type='response')
RMSE.GLM4 <- sqrt(mean((GLM4.pred-test$Price)^2))

RMSE.baseline
RMSE.GLM1
RMSE.GLM2
RMSE.GLM3
RMSE.GLM4

#A small improvement on the baseline, but not that much
