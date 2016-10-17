##########################################################
## Final Course Project Script for Reproducible Research##
##########################################################

## Loading libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(R.utils)
library(data.table)
library(lubridate)

 ## Downloading file and decompressing
setwd("/Users/rohitpittu/R Projects/DataScience/Reproducible Research/Final Course Project/")
url <- c("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2")

## run the following code once
# download.file(url,"./Data/StormData.csv.bz2",method = "curl")
# bunzip2("./Data/StormData.csv.bz2",remove = FALSE)

## Reading into a data.table
StormData<-fread(input = "./Data/StormData.csv",header = TRUE,strip.white = TRUE)
str(StormData)
tail(StormData)

StormData$BgnDate <- as_date(mdy_hms(StormData$BGN_DATE))
StormData$EndDate <- as_date(mdy_hms(StormData$END_DATE))

# Getting only post 1995 data and required columns
StormData_96 <- StormData[year(BgnDate)>1995,.(BgnDate,EndDate,REFNUM,
                                               STATE,STATE__,COUNTY,COUNTYNAME,
                                               EVTYPE,FATALITIES,INJURIES,
                                               PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)]



source('damage.R')
Data_df<-as.data.frame(StormData_96)
TOTAL_DAMAGE <- apply(Data_df[,c(11:14)],1,damage)
Data_df<-cbind(Data_df,TOTAL_DAMAGE)
n=10
Data_top <- subset(Data_df, TOTAL_DAMAGE > quantile(TOTAL_DAMAGE, prob = 1 - n/100))





