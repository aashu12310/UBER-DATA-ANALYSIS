library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(ggpubr)

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

a=ggplot(hour_data,aes(hour,Total))+
  geom_bar(stat="identity",fill="steelblue",color="red")+
  ggtitle("Trips Every Hour")+
  theme(legend.position = "none")+
  scale_y_continuous(labels=comma)
a
month_hour= data_2014%>%
  group_by(month,hour)%>%
  summarise(Total=n())

b=ggplot(month_hour,aes(hour,Total,fill=month))+
  geom_bar(stat="identity")+
  ggtitle("Trips by Hour and Month")+
  scale_y_continuous(labels=comma)
b

day_group = data_2014%>%
  group_by(day)%>%
  summarise(Total=n())
datatable(day_group)

c=ggplot(day_group,aes(day,Total))+
  geom_bar(stat = "identity",fill="steelblue")+
  ggtitle("Trips Every Data")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)
c

day_month_group = data_2014%>%
  group_by(month,day)%>%
  summarise(Total=n())

d=ggplot(day_month_group,aes(day,Total,fill=month))+
  geom_bar(stat = "identity")+
  ggtitle("Trips by Day and Month")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = colors)
d

#Number of trips taking place during months in a year
month_group=data_2014%>%
  group_by(month)%>%
  summarise(Total=n())

datatable(month_group)

e=ggplot(month_group,aes(month,Total,fill=month))+
  geom_bar(stat="identity")+
  ggtitle("Trips by Month")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = colors)
e

monthly_weekend=data_2014%>%
  group_by(month,dayofweek)%>%
  summarise(Total=n())

f=ggplot(monthly_weekend,aes(month,Total,fill=dayofweek))+
  geom_bar(stat = "identity",position = "dodge")+
  ggtitle("Trips by Day and Month")+
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values=colors)


f
#number of Trips by bases
g=ggplot(data_2014,aes(Base))+
  geom_bar(fill="darkblue")+
  scale_y_continuous(labels = comma)+
  ggtitle("Trips by Bases")
g

h=ggplot(data_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)

h
i=ggplot(data_2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)
i
day_and_hour= data_2014 %>%
                group_by(day, hour) %>%
                summarize(Total = n())
datatable(day_and_hour)

j=ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")
j

k=ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")

k

l=ggplot(monthly_weekend, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")  
l

month_base =data_2014 %>%
              group_by(Base, month) %>%
              summarize(Total = n()) 
day0fweek_bases=data_2014 %>%
                   group_by(Base, dayofweek) %>%
                   summarize(Total = n()) 
m=ggplot(month_base, aes(Base, month, fill = Total)) +
     geom_tile(color = "white") +
     ggtitle("Heat Map by Month and Bases")
m

min_lat = 40.5774
max_lat = 40.9176
min_long = -74.15
max_long = -73.7004
n=ggplot(data_2014, aes(x=Lon, y=Lat)) +
     geom_point(size=1, color = "blue") +
     scale_x_continuous(limits=c(min_long, max_long)) +
     scale_y_continuous(limits=c(min_lat, max_lat)) +
     theme_map() +
     ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")
n
o=ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +
    geom_point(size=1) +
    scale_x_continuous(limits=c(min_long, max_long)) +
    scale_y_continuous(limits=c(min_lat, max_lat)) +
    theme_map() +
    ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")
o

  
figure = ggarrange(a, b, c, d, e, f,g,h,i,j,k,l,m,n,o,
                    labels = c("A", "B","C","D","E","F","G","H","I","J","K","L","M","N","O"),
                    ncol = 3, nrow = 3)
figure
