# The URL of the Land Registry data
inputdata <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-monthly-update.txt"

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

#Calculate the average price by month
AveragePrice <- aggregate(landregistry$Price, list(landregistry$Transfer_Year), mean)

#Rename the columns
names <- c("Year", "Average_Price")
names(AveragePrice) <- names

#Plot the average price by month
require(ggplot2)
ggplot( data = AveragePrice, aes( x=Year, y=Average_Price )) + geom_point(shape=1) + geom_line()

ggplot( data=landregistry, aes(x=Transfer_Year, y=Price)) + geom_smooth()


#Remove data that isn't needed
rm(inputdata, names)



