
library(data.table)
library(dplyr)

# Reading relevant information from daily county files and save in R data format

daily_files <- list.files(path="data", pattern="daily_aqi_by_county.*.csv", full.names=TRUE, recursive=FALSE)
files_with_data <- daily_files[-(1:10)]
lapply(files_with_data, function(x) {
  df <- fread(x, select = c("State Name","county Name","Date","AQI","Category","Defining Parameter")
             ,header = T, sep = ',')
  fileName = paste(substring(x, 6,29) , ".Rds", sep="");
  ifelse(!dir.exists("rds"), dir.create("rds"),"")
  fileName = paste("rds/" , fileName, sep="")
  saveRDS(df, file=fileName)
  }
)


# Reading relevant information from hourly temp,wind,pollutant files, merge and save in and save in R data format

hourly_WIND_2018 <- fread("data/hourly_WIND_2018.csv", select = c("Date Local",	"Time Local"	,"Sample Measurement","State Name",	"County Name")
            ,header = T, sep = ',')
saveRDS(hourly_WIND_2018, file="rds/hourly_WIND_2018.Rds")

hourly_TEMP_2018 <- fread("data/hourly_TEMP_2018.csv", select = c("Date Local",	"Time Local"	,"Sample Measurement","State Name",	"County Name")
                          ,header = T, sep = ',')

saveRDS(hourly_TEMP_2018, file="rds/hourly_TEMP_2018.Rds")

hourly_88101_2018 <- fread("data/hourly_88101_2018.csv",header = T, sep = ',')
hourly_81102_2018 <- fread("data/hourly_81102_2018.csv",header = T, sep = ',')
hourly_42101_2018 <- fread("data/hourly_42101_2018.csv",header = T, sep = ',')
hourly_44201_2018 <- fread("data/hourly_44201_2018.csv",header = T, sep = ',')
hourly_42602_2018 <- fread("data/hourly_42602_2018.csv",header = T, sep = ',')
hourly_42401_2018 <- fread("data/hourly_42401_2018.csv",header = T, sep = ',')

hourly_42401_2018_group <-aggregate(hourly_42401_2018$`Sample Measurement`,by=list(hourly_42401_2018$`State Name`, hourly_42401_2018$`County Name`, hourly_42401_2018$`Date Local`,hourly_42401_2018$`Time Local`),FUN=mean)
hourly_42101_2018_group <-aggregate(hourly_42101_2018$`Sample Measurement`,by=list(hourly_42101_2018$`State Name`, hourly_42101_2018$`County Name`, hourly_42101_2018$`Date Local`,hourly_42101_2018$`Time Local`),FUN=mean)
hourly_42602_2018_group <-aggregate(hourly_42602_2018$`Sample Measurement`,by=list(hourly_42602_2018$`State Name`, hourly_42602_2018$`County Name`, hourly_42602_2018$`Date Local`,hourly_42602_2018$`Time Local`),FUN=mean)
hourly_44201_2018_group <-aggregate(hourly_44201_2018$`Sample Measurement`,by=list(hourly_44201_2018$`State Name`, hourly_44201_2018$`County Name`, hourly_44201_2018$`Date Local`,hourly_44201_2018$`Time Local`),FUN=mean)
hourly_88101_2018_group <-aggregate(hourly_88101_2018$`Sample Measurement`,by=list(hourly_88101_2018$`State Name`, hourly_88101_2018$`County Name`, hourly_88101_2018$`Date Local`,hourly_88101_2018$`Time Local`),FUN=mean)
hourly_81102_2018_group <-aggregate(hourly_81102_2018$`Sample Measurement`,by=list(hourly_81102_2018$`State Name`, hourly_81102_2018$`County Name`, hourly_81102_2018$`Date Local`,hourly_81102_2018$`Time Local`),FUN=mean)

setnames(hourly_42401_2018_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","Sample Measurement(SO2)"))
setnames(hourly_42101_2018_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","Sample Measurement(CO)"))
setnames(hourly_42602_2018_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","Sample Measurement(No2)"))
setnames(hourly_44201_2018_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","Sample Measurement(Ozone)"))
setnames(hourly_88101_2018_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","Sample Measurement(PM2.5)"))
setnames(hourly_81102_2018_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","Sample Measurement(PM10)"))

hourly_all_poll_2018<-Reduce(function(x,y) merge(x = x, y = y, c("State Name", "County Name", "Date Local","Time Local"),all=TRUE),
       list(hourly_42401_2018_group, hourly_42101_2018_group,hourly_42602_2018_group,hourly_44201_2018_group,hourly_88101_2018_group,hourly_81102_2018_group))
hourly_all_poll_2018$`Date Local` <- as.Date(hourly_all_poll_2018$`Date Local`)
hourly_all_poll_2018$Year <- as.numeric(format(hourly_all_poll_2018$`Date Local`, format = "%Y"))
hourly_all_poll_2018$Month <- as.numeric(format(hourly_all_poll_2018$`Date Local`, format = "%m"))
hourly_all_poll_2018$Day <- as.numeric(format(hourly_all_poll_2018$`Date Local`, format = "%d"))
fileName = paste("rds/" , "hourly_all_pollutants_2018.Rds", sep="")
saveRDS(hourly_all_poll_2018, file=fileName)

# Reading relevant information from daily pollutant files, merge and save in R data format

daily_88101_2018 <- fread("data/daily_88101_2018.csv",header = T, sep = ',')
daily_81102_2018 <- fread("data/daily_81102_2018.csv",header = T, sep = ',')
daily_42101_2018 <- fread("data/daily_42101_2018.csv",header = T, sep = ',')
daily_44201_2018 <- fread("data/daily_44201_2018.csv",header = T, sep = ',')
daily_42602_2018 <- fread("data/daily_42602_2018.csv",header = T, sep = ',')
daily_42401_2018 <- fread("data/daily_42401_2018.csv",header = T, sep = ',')

daily_42401_2018_group <-aggregate(daily_42401_2018$`Arithmetic Mean`,by=list(daily_42401_2018$`State Name`, daily_42401_2018$`County Name`, daily_42401_2018$`Date Local`),FUN=mean)
daily_42101_2018_group <-aggregate(daily_42101_2018$`Arithmetic Mean`,by=list(daily_42101_2018$`State Name`, daily_42101_2018$`County Name`, daily_42101_2018$`Date Local`),FUN=mean)
daily_42602_2018_group <-aggregate(daily_42602_2018$`Arithmetic Mean`,by=list(daily_42602_2018$`State Name`, daily_42602_2018$`County Name`, daily_42602_2018$`Date Local`),FUN=mean)
daily_44201_2018_group <-aggregate(daily_44201_2018$`Arithmetic Mean`,by=list(daily_44201_2018$`State Name`, daily_44201_2018$`County Name`, daily_44201_2018$`Date Local`),FUN=mean)
daily_88101_2018_group <-aggregate(daily_88101_2018$`Arithmetic Mean`,by=list(daily_88101_2018$`State Name`, daily_88101_2018$`County Name`, daily_88101_2018$`Date Local`),FUN=mean)
daily_81102_2018_group <-aggregate(daily_81102_2018$`Arithmetic Mean`,by=list(daily_81102_2018$`State Name`, daily_81102_2018$`County Name`, daily_81102_2018$`Date Local`),FUN=mean)

setnames(daily_42401_2018_group, old=c("Group.1","Group.2","Group.3","x"), new=c("State Name", "County Name", "Date Local","Arithmetic Mean(SO2)"))
setnames(daily_42101_2018_group, old=c("Group.1","Group.2","Group.3","x"), new=c("State Name", "County Name", "Date Local","Arithmetic Mean(CO)"))
setnames(daily_42602_2018_group, old=c("Group.1","Group.2","Group.3","x"), new=c("State Name", "County Name", "Date Local","Arithmetic Mean(No2)"))
setnames(daily_44201_2018_group, old=c("Group.1","Group.2","Group.3","x"), new=c("State Name", "County Name", "Date Local","Arithmetic Mean(Ozone)"))
setnames(daily_88101_2018_group, old=c("Group.1","Group.2","Group.3","x"), new=c("State Name", "County Name", "Date Local","Arithmetic Mean(PM2.5)"))
setnames(daily_81102_2018_group, old=c("Group.1","Group.2","Group.3","x"), new=c("State Name", "County Name", "Date Local","Arithmetic Mean(PM10)"))

daily_all_poll_2018<-Reduce(function(x,y) merge(x = x, y = y, c("State Name", "County Name", "Date Local"),all=TRUE),
               list(daily_42401_2018_group, daily_42101_2018_group,daily_42602_2018_group,daily_44201_2018_group,daily_88101_2018_group,daily_81102_2018_group))

daily_all_poll_2018$`Date Local` <- as.Date(daily_all_poll_2018$`Date Local`)
daily_all_poll_2018$Year <- as.numeric(format(daily_all_poll_2018$`Date Local`, format = "%Y"))
daily_all_poll_2018$Month <- as.numeric(format(daily_all_poll_2018$`Date Local`, format = "%m"))
daily_all_poll_2018$Day <- as.numeric(format(daily_all_poll_2018$`Date Local`, format = "%d"))


fileName = paste("rds/" , "daily_all_pollutants_2018.Rds", sep="")
saveRDS(daily_all_poll_2018, file=fileName)


