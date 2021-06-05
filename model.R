####################################
# Walmart Weekly Sales Prediction  #
# Linear Regression Model          #  
# Author: Nikhil Shrestha          #
####################################

# Importing libraries
library(tidyverse)#opinionated collection of R packages
library(ggplot2) #data visualization
library(tibble) #handle tibbles
library(dplyr) #data manipulation
library(skimr) #for better summary
library(caret) # for data splitting, pre-processing,feature selection etc.
library(lubridate) # to work in ease with dates
library(lmtest) #Testing linear regression Model

#For multicollinearity (VIF)
library(sp)
library(raster)
library(usdm) 
library(car)


# Importing the Walmart data set
Walmart <- read.csv("C:/data/Walmart_Store_sales.csv")

# ***Convert date into date format and make variables month, year, Week***
Walmart$Date <- as.Date(Walmart$Date, format = "%d-%m-%Y")
Walmart$Week<- format(as.Date(Walmart$Date), "%V")
Walmart$Month <- month(Walmart$Date)
Walmart$Year <- year(Walmart$Date)

# **Making factors using store. **
# weekly sales of Stores when grouped by Month
Walmart<-Walmart %>% 
  group_by(Store ,Month) %>%
  mutate(sum_month_store_wise = sum(Weekly_Sales))

#Monthly mean of Sales 
Walmart<-Walmart %>% 
  group_by(Month) %>% 
  mutate(mean_Monthly = mean(sum_month_store_wise))

# upper threshold limit 
# we mean here again to get single value as Threshold limit
Walmart$Upper_limit <- mean(Walmart$mean_Monthly) * 1.05

# lower threshold limit 
# we mean here again to get single value as Threshold limit
Walmart$Lower_limit <- mean(Walmart$mean_Monthly) * 0.95

Walmart$Store_Type <- "Medium" # 16th column
Walmart[which(Walmart$sum_month_store_wise > Walmart$Upper_limit), 16] = "High"
Walmart[which(Walmart$sum_month_store_wise < Walmart$Lower_limit), 16] = "Low"

# Letting go all other stores as we have to predict for Only store 1
Walmart<- Walmart %>% 
  filter(Store==1)

# delete all un neccesary variables
Walmart<- Walmart[,-12:-15]

#### **Making factors using Week.**
# get mean Weekly Sales when group by month 
Walmart<- Walmart %>% 
  group_by(Month) %>% 
  mutate(WeeklySales = mean(Weekly_Sales))

# mean of Weekly_Sales when grouped by Year
Walmart<-Walmart %>% 
  group_by(Year) %>% 
  mutate(mean_yearly = mean(WeeklySales))

# upper threshold limit 
Walmart<- Walmart %>% 
  group_by(Year) %>% 
  mutate(Upper_limit = mean_yearly *1.05)

# Lower threshold limit 
Walmart<- Walmart %>% 
  group_by(Year) %>% 
  mutate(Lower_limit = mean_yearly *0.95)

# Making Week_Type
Walmart$Week_type <- "Medium" # 17th column
Walmart[which(Walmart$Weekly_Sales > Walmart$Upper_limit), 17] = "High"
Walmart[which(Walmart$Weekly_Sales < Walmart$Lower_limit), 17] = "Low"

# Drop unwanted Variables
Walmart<- Walmart[,-c(13:16)]

#creating Column for Markdown Events.
# Creating Super_Bowl variable
Walmart$Super_Bowl <- ifelse((Walmart$Date == "2010-02-10" |
                                Walmart$Date == "2011-02-11"|
                                Walmart$Date == "2012-02-10"), 1, 0)
# Creating Labour_Day variable
Walmart$Labour_day <- ifelse((Walmart$Date == "2010-09-10" |
                                Walmart$Date == "2011-09-09"|
                                Walmart$Date == "2012-09-07"), 1, 0)
# Creating Thanksgiving variable
Walmart$Thanksgiving <- ifelse((Walmart$Date == "2010-11-26" |
                                  Walmart$Date == "2011-11-25"|
                                  Walmart$Date == "2012-11-23"), 1, 0)
# Creating Christmas variable
Walmart$Christmas<- ifelse((Walmart$Date == "2010-12-31" |
                              Walmart$Date == "2011-12-30"|
                              Walmart$Date == "2012-12-28"), 1, 0)

#remove unnecessary variables
Walmart <- subset(Walmart, select = -c(Store, Date, Week, Month, 
                                       Year))

# Outliers treatment
uv1 = quantile(Walmart$Weekly_Sales, 0.75)
Walmart$Weekly_Sales[Walmart$Weekly_Sales> uv1] <- uv1

uv = quantile(Walmart$Unemployment, 0.75)
Walmart$Unemployment[Walmart$Unemployment> uv] <- uv

#Log transformation for skewness
Walmart$Weekly_Sales <- log(Walmart$Weekly_Sales)
Walmart$CPI <- log(Walmart$CPI)
Walmart$Temperature <- log(Walmart$Temperature)

#Factor the Categorical Variables
Walmart$Week_type <- factor(Walmart$Week_type, levels = c("Low", "Medium", "High"),labels = 0,1,2)
Walmart$Store_Type <- factor(Walmart$Store_Type, levels = c("Low", "Medium", "High"),labels = 0,1,2)

# **Split the Data : Test & Train**
# **We have Holiday_Flag variable that divides our dataset into two halves, hence lets use that ratio to distribute test and train**

#selecting 75% for Train(ratio maintained 93:7 No holiday : holiday)
set.seed(7)
t <- createDataPartition(Walmart$Holiday_Flag, p = 0.75)
T1=t[[1]]

train_set <- Walmart[T1,]
test_set <- Walmart[-T1,]

write.csv(train_set, "training.csv")
write.csv(test_set, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

# 6. All except Super_Bowl, Holiday_Flag, Labour_day, Thanksgiving & Unemployment +CPI+Fuel_Price+CPI=5.35, Fuel_Price = 2.5,
model=lm(Weekly_Sales~CPI+Fuel_Price+Temperature+as.factor(Store_Type)+as.factor(Week_type)+Christmas,data=TrainSet)

new_val <-  data.frame(CPI=5.35, Fuel_Price = 2.54,Temperature = 3.65, Store_Type =3, Week_type = 2, Christmas = 0)
new_val

Weekly_Sale<- predict(model, new_val)
exp(round(Weekly_Sale,1))
Weekly_Sale

# Save model to RDS file
saveRDS(model, "model.rds")

