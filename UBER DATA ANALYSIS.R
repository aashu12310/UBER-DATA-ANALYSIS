library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

#CREATING VECTOR OF COLOURS
colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

#IMPORTING DATASET
apr_data = read.csv(file.choose(),sep=',',header=T,stringsAsFactors = F)
may_data = read.csv(file.choose(),sep=',',header=T,stringsAsFactors = F)
jun_data = read.csv(file.choose(),sep=',',header=T,stringsAsFactors = F)
jul_data = read.csv(file.choose(),sep=',',header=T,stringsAsFactors = F)
aug_data = read.csv(file.choose(),sep=',',header=T,stringsAsFactors = F)
sep_data = read.csv(file.choose(),sep=',',header=T,stringsAsFactors = F)

data_2014 = rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)
data_2014$Date.Time = as.POSIXct(data_2014$Date.Time,format="%m/%d/%Y %H:%M:%S")
data_2014$Time = format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

#View(data_2014)
data_2014$Date.Time = ymd_hms(data_2014$Date.Time)
data_2014$day = factor(day(data_2014$Date.Time))
data_2014$month = factor(month(data_2014$Date.Time,label = TRUE))
data_2014$year = factor(year(data_2014$Date.Time))
data_2014$dayofweek = factor(wday(data_2014$Date.Time,label=TRUE))

data_2014$hour = factor(hour(hms(data_2014$Time)))
data_2014$minute = factor(minute(hms(data_2014$Time)))
data_2014$second = factor(second(hms(data_2014$Time)))

#PLOTTING THE TRIPS BY HOURS IN DAY

hour_data = data_2014%>%
  group_by(hour)%>%
  summarise(Total=n())
datatable(hour_data)

ggplot(hour_data,aes(hour,Total))+
  geom_bar(stat="identity",fill="steelblue",color="red")+
  ggtitle("Trips Every Hour")+
  theme(legend.position = "none")+
  scale_y_continuous(labels=comma)

month_hour= data_2014%>%
  group_by(month,hour)%>%
  summarise(Total=n())

ggplot(month_hour,aes(hour,Total,fill=month))+
  geom_bar(stat="identity")+
  ggtitle("Trips by Hour and Month")+
  scale_y_continuous(labels=comma)
