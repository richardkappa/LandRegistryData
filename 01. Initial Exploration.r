# The URL of the Land Registry data
inputdata <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-monthly-update.txt"

# Bigger dataset, all 2016 data
#inputdata <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.txt"

# Import the data
landregistry <- read.csv(file=inputdata, sep=",", header = FALSE)

#Name the columns
names <- c('Transaction_unique_identifier', 'Price', 'Date_of_Transfer', 'Postcode', 'Property_Type', 'Old_New', 'Duration', 'PAON', 'SAON', 'Street', 'Locality', 'Town_City', 'District', 'County', 'PPDCategory_Type', 'Record_Status_monthly_file_only')
names(landregistry) <- names

#Remove the hour and minutes from the date variable and create a month variable
landregistry$Transfer_Date <- as.Date(as.POSIXct(landregistry$Date_of_Transfer, "%Y-%m-%d %H:%M"))
landregistry$Transfer_Month <- format(landregistry$Transfer_Date, "%Y-%m")
landregistry$Transfer_Year <- as.numeric(format(landregistry$Transfer_Date, "%Y"))

#Remove rows where the value is over £1M
landregistry <- landregistry[!(landregistry$Price>1000000),]

#Look at the distribution of prices
hist(landregistry$Price)

#Look at the distribution of prices for 2016 sales
sales2016 <- landregistry[ which(landregistry$Transfer_Year==2016), ]
hist(sales2016$Price)

#Calculate the average prices for a number of factors and plot the outcomes

#Calculate the average price by month
library(plyr)
Average_Month <- ddply(sales2016, c("Transfer_Month"), summarise, 
                       Sales         = length(Price), 
                       Average_Price = mean(Price)
)


#Plot a bar chart of the results
barplot(Average_Month$Average_Price, main="Average price by month", xlab="Transaction Month")

#Or using ggplot
ggplot(data=Average_Month, aes(x=Transfer_Month, y=Average_Price)) + geom_bar(stat="identity")


#Now do the same for property type
Average_Type <- ddply(sales2016, c("Property_Type"), summarise, 
                       Sales         = length(Price), 
                       Average_Price = mean(Price)
)

ggplot(data=Average_Type, aes(x=Property_Type, y=Average_Price)) + geom_bar(stat="identity")

#And Now Old/new
Average_Old_New <- ddply(sales2016, c("Old_New"), summarise, 
                      Sales         = length(Price), 
                      Average_Price = mean(Price)
)

ggplot(data=Average_Old_New, aes(x=Old_New, y=Average_Price)) + geom_bar(stat="identity")
