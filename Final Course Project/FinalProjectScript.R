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
library(ggthemes)
library(plotly)


## Downloading file and decompressing
setwd("/Users/rohitpittu/R Projects/DataScience/Reproducible Research/Final Course Project/")
url <- c("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2")

## Run the following code once
# download.file(url,"./Data/StormData.csv.bz2",method = "curl")
# bunzip2("./Data/StormData.csv.bz2",remove = FALSE)

## Reading into a data.table
StormData<-fread(input = "./Data/StormData.csv",header = TRUE,strip.white = TRUE)

### Converting to Date format to allow subsetting
StormData$BgnDate <- as_date(mdy_hms(StormData$BGN_DATE))
StormData$EndDate <- as_date(mdy_hms(StormData$END_DATE))

### Getting only post 1992 data
StormData_sub <- StormData[year(BgnDate)>1992,.(BgnDate,EndDate,REFNUM,
                                               STATE,STATE__,COUNTY,COUNTYNAME,
                                               EVTYPE,FATALITIES,INJURIES,
                                               PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)]

Data_df<-as.data.frame(StormData_sub)

### Computing the Total Dollar Damage per event
source('decode.R')
PROPEXP<-apply(Data_df[,c(11,12)],1,decoder)
CROPEXP<-apply(Data_df[,c(13,14)],1,decoder)
Data_df<-cbind(Data_df,PROPEXP,CROPEXP)
Data_df <- mutate(Data_df,Tot_damage = (PROPDMG*PROPEXP)+(CROPDMG*CROPEXP))

## Top Event Types which cause the most economic damage

### Select the top 50 percentile of damage data
n=50
Data_calc<-Data_df%>%
    filter(Tot_damage > quantile(Tot_damage, prob = 1 - n/100))%>%
    select(Date=BgnDate,Ref=REFNUM,State=STATE,CountyId=COUNTY,Event=EVTYPE
           ,Damage = Tot_damage)%>%
    mutate(Event = as.factor(toupper(trim(Event))))%>%
    group_by(Event)%>%
    summarize(EventDamage = sum(Damage)/1000000000)%>%
    arrange(desc(EventDamage))

### Select top 10 Event Types
Data_top10<-Data_calc[c(1:10),]

### Plot the results
g1<-ggplot(Data_top10,aes(x=factor(Event),y=EventDamage,fill=Event))+
    geom_bar(stat="identity",col="black",width=0.5)+
    coord_flip()+
    guides(fill=FALSE)+
    ylab("Economic Damage(in $ billions)")+
    xlab("Top 10 Event Types")+
    ggtitle("Most Economically Damaging Event Types")+
    theme_gdocs()
print(g1)

## Yearly Economic Damage
Data_year<-Data_df%>%
    select(Date=BgnDate,Ref=REFNUM,State=STATE,CountyId=COUNTY,Event=EVTYPE
           ,Damage = Tot_damage)%>%
    group_by(Year = year(Date))%>%
    summarize(YearlyDamage = sum(Damage)/1000000000)%>%
    arrange(Year)

### Plot the results
g2<-ggplot(Data_year,aes(x=factor(Year),y=YearlyDamage,fill=Year))+
    geom_bar(stat="identity",col="black",width=0.5)+
    coord_flip()+
    guides(fill=FALSE)+
    ylab("Economic Damage(in $ billions)")+
    xlab("Years")+
    ggtitle("Yearly Economic Damage")+
    theme_gdocs()
ggplotly(g2)


## Top Event Types which cause the most human damage
Data_calc<-Data_df%>%
    select(Date=BgnDate,Ref=REFNUM,State=STATE,CountyId=COUNTY,Event=EVTYPE
           ,FATALITIES,INJURIES)%>%
    mutate(Event = as.factor(toupper(trim(Event))))%>%
    group_by(Event)%>%
    summarize(HumanDamage = sum(FATALITIES,INJURIES),Deaths = sum(FATALITIES))%>%
    mutate(Intensity = Deaths/HumanDamage)%>%
    arrange(desc(HumanDamage))

### Select top 10 Event Types
Data_top10<-Data_calc[c(1:10),]

### Plot the results
g3<-ggplot(Data_top10,aes(x=factor(Event),y=HumanDamage,fill=Intensity))+
    geom_bar(stat="identity",col="black",width=0.5)+
    coord_flip()+
    ylab("Human Damage(Fatalities+Injuries)")+
    xlab("Top 10 Event Types")+
    ggtitle("Event types causing most human damage")+
    theme_fivethirtyeight()
ggplotly(g3)
