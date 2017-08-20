#Code below based upon the this blog post:
#   https://www.r-bloggers.com/part-4a-modelling-predicting-the-amount-of-rain/

#packages needed
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


# Create random 70% sample
#Set the random seed
set.seed(100)

#Randomly pick 70% of the data
index <- sample(1:nrow(Initialdata),size = 0.7*nrow(Initialdata)) 

#Select the 70% subset, and the testing dataset (30%)
training <- sales2016[index,]
test <- sales2016 [-index,] 

#Baseline model
best.guess <- mean(training$Price)
RMSE.baseline <- sqrt(mean((best.guess-test$Price)^2))
RMSE.baseline

MAE.baseline <- mean(abs(best.guess-test$Price))
MAE.baseline

#Linear regression model
lin.reg <- lm(Price ~ Old_New + Property_Type + Transfer_Month, data = training)
summary(lin.reg)

plot(lin.reg)

#Test the model
test.pred.lin <- predict(lin.reg,test)
RMSE.lin.reg <- sqrt(mean((test.pred.lin-test$Price)^2))
RMSE.lin.reg

MAE.lin.reg <- mean(abs(test.pred.lin-test$Price))
MAE.lin.reg

rt <- rpart(Price ~ Old_New + District + Property_Type +  District, data=training)

test.pred.rtree <- predict(rt,test) 

RMSE.rtree <- sqrt(mean((test.pred.rtree-test$Price)^2))
RMSE.rtree

MAE.rtree <- mean(abs(test.pred.rtree-test$rain))
MAE.rtree

RMSE.baseline
RMSE.lin.reg
RMSE.rtree
#Theyre both worse than the baseline!
