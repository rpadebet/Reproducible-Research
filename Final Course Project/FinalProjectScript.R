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

# system.time(TOTAL_DAMAGE <- apply(Data_df[,c(11:14)],1,damage))

PROPEXP<-apply(Data_df[,c(11,12)],1,decoder)
CROPEXP<-apply(Data_df[,c(13,14)],1,decoder)
Data_df<-cbind(Data_df,PROPEXP,CROPEXP)
Data_df <- mutate(Data_df,Tot_damage = (PROPDMG*PROPEXP)+(CROPDMG*CROPEXP))

n=20

Data_top<-Data_df%>%
    filter(Tot_damage > quantile(Tot_damage, prob = 1 - n/100))%>%
    select(Date=BgnDate,Ref=REFNUM,State=STATE,CountyId=COUNTY,Event=EVTYPE
           ,Damage = Tot_damage)%>%
    mutate(Event = as.factor(toupper(trim(Event))))%>%
    group_by(Event)%>%
    summarize(EventDamage = sum(Damage))%>%
    arrange(desc(EventDamage))

barplot(Data_top[10,]$EventDamage,horiz = TRUE,col=Data_top[10,]$Event)



