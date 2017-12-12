setwd("C:\\Users\\SROY\\Documents\\CodeBase\\Datasets\\Mckinsey")
rm(list=ls())
seedVal = 17869
options(warn=-1)
options(scipen=999)

# Load libraries
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(zoo)
theme_set(theme_bw())
library("forecastHybrid")
library("caret")
library("xts")
library("highfrequency")

# Load Data
DataDump <- read.csv2("train_aWnotuB.csv", header = TRUE, sep = ',')
train <- read.csv2("train_aWnotuB.csv", header = TRUE, sep = ',')
test <- read.csv2("test_BdBKkAj.csv", header = TRUE, sep = ',')
sample <- read.csv2("sample_submission_EZmX9uE.csv", header = TRUE, sep = ',')

# Check missing values
apply(train, 2, function(x) length(which(x == "" | is.na(x) | x == "NA")))
apply(test, 2, function(x) length(which(x == "" | is.na(x) | x == "NA")))

# Check stats
summary(train)

# Check structure
str(train)
# Looks fine. Let's factor the junctions
train$Junction <- as.factor(train$Junction)
# Extract date features
train$DateTime <- as.character(train$DateTime)
train$DateOnly <- as.Date(train$DateTime)
train <- train %>% mutate(Report_Year = year(ymd_hms(DateTime)))
train <- train %>% mutate(Report_Month = month(ymd_hms(DateTime)))
train <- train %>% mutate(Report_Day = day(ymd_hms(DateTime)))
train <- train %>% mutate(Report_Hour = hour(ymd_hms(DateTime)))
train <- train %>% mutate(Report_Wday = wday(ymd_hms(DateTime)))
train$DateTime <- ymd_hms(train$DateTime)

# Descriptive statistics
countVehicleByJunc <- train %>% group_by(Junction) %>% summarise(Total = sum(Vehicles))
ggplot(countVehicleByJunc, aes(Junction, Total)) + 
  geom_bar(stat="identity", width = 0.5, fill="tomato2")
# Junction 4 has very less reports. Junction 1 has too many

countVehicleByJuncYr <- train %>% group_by(Junction, Report_Year) %>% summarise(Total = sum(Vehicles))
ggplot(countVehicleByJuncYr, aes(Junction, Total)) + 
  geom_bar(stat='identity', aes(fill=Report_Year), width=.5)
# Junction 4 doesn't have 2015,2016 data

countVehicleByJuncMonth <- train %>% group_by(Junction, Report_Month) %>% summarise(Total = sum(Vehicles))
# Junction 4 has data for only 6 months

countVehicleByJuncHour <- train %>% group_by(Junction, Report_Hour) %>% summarise(Total = sum(Vehicles))
# Looks fine

countVehicleByJuncWDay <- train %>% group_by(Junction, Report_Wday) %>% summarise(Total = sum(Vehicles))
# Looks fine

countVehicleByJuncDay <- train %>% group_by(Junction, Report_Day) %>% summarise(Total = sum(Vehicles))
# Looks fine

# Plot time series
tsVehicleByJuncDate <- train %>% group_by(DateOnly, Junction) %>% summarise(Total = sum(Vehicles))
ggplot(tsVehicleByJuncDate, aes(x=DateOnly)) + 
  geom_line(aes(y=Total, col=Junction)) + 
  scale_color_manual(values = c('green','blue','red','black')) + 
  theme(panel.grid.minor = element_blank())
# Junction 1 is ever increasing
# Junction 3 has periodic spikes

tsVehicleByJuncWDay <- train %>% group_by(Report_Wday, Junction) %>% summarise(Total = sum(Vehicles))
ggplot(tsVehicleByJuncWDay, aes(x=Report_Wday)) + 
  geom_line(aes(y=Total, col=Junction)) + 
  scale_color_manual(values = c('green','blue','red','black')) + 
  theme(panel.grid.minor = element_blank())
# Junction 1 has peak traffic on weekdays but dips over weekends

# We have to predict from 2017-07-01 00:00:00
# Check range of dates
range(train$DateOnly)
range(train[train$Junction == 4, 'DateOnly'])
range(train[train$Junction == 3, 'DateOnly'])
range(train[train$Junction == 2, 'DateOnly'])
range(train[train$Junction == 1, 'DateOnly'])

# Now start modelling
# We will create 4 models for 4 junctions
# Split train into 4 datasets
train_Junc1 <- data.frame(train[train$Junction == 1, c('Vehicles')])
train_Junc2 <- data.frame(train[train$Junction == 2, c('Vehicles')])
train_Junc3 <- data.frame(train[train$Junction == 3, c('Vehicles')])
train_Junc4 <- data.frame(train[train$Junction == 4, c('Vehicles')])
names(train_Junc1) <- 'Vehicles'
names(train_Junc2) <- 'Vehicles'
names(train_Junc3) <- 'Vehicles'
names(train_Junc4) <- 'Vehicles'


################### For Junction 1#############################333
# #convert data into xts
#r_xts=xts(train_Junc1, order.by=as.POSIXct(train_Junc1$DateTime))
#head(r_xts)

#train_Junc1.ts<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,3]))))

# Split test train
train_Junc1_train <- train_Junc1[1:11600,]
train_Junc1_test <- train_Junc1[11601:14592,]

train_Junc1_train.ts <- ts(train_Junc1_train)
train_Junc1_test.ts <- ts(train_Junc1_test)

ensbl.1 <- hybridModel(y = log10(train_Junc1_train.ts), models="aent", errorMethod="RMSE",
                        a.args = list(approximation = TRUE, trace=FALSE, allowdrift = FALSE),
                        n.args = list(repeats = 10), 
                        t.args = list(use.arma.errors = FALSE))
summary(ensbl.1)

pred.1 <- forecast(ensbl.1, h = 2992)

##################################### Write to output #####################################

# Append values to test
Vehicles <- c(unlist(ceiling(exp(Predmodel1))),
                   unlist(ceiling(exp(Predmodel2))),
                   unlist(ceiling(exp(Predmodel3))),
                   unlist(ceiling(exp(Predmodel4))))
test <- cbind(test, Vehicles)
submission <- test[,c('ID','Vehicles')]
names(submission) <- c('ID','Vehicles')

# Export to file
write.table(submission, file = "Output.csv", quote = FALSE, row.names=FALSE, sep=",")

