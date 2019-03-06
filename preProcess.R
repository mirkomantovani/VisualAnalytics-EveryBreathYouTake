library(data.table)
library(dplyr)
library(fst)

#Check if the folder "feather" exists in the current directory, if not creates it
ifelse(!dir.exists("fst"), dir.create("fst"),"")

# Reading relevant information from daily county files and save in R data format

daily_files <- list.files(path="data", pattern="daily_aqi_by_county.*.csv", full.names=TRUE, recursive=FALSE)
files_with_data <- daily_files[-(1:10)]

print("Aggregating daily AQI data")

daily_aqi = lapply(files_with_data, fread)
daily_df <- do.call(rbind, daily_aqi)

print("Aggregation of daily AQI data done ")

daily_df$Date <- as.Date(daily_df$Date)
daily_df$Year <- format(daily_df$Date, format = "%Y")
daily_df$Month <- format(daily_df$Date, format = "%B")
daily_df$Day <- format(daily_df$Date, format = "%d")
daily_df$Date <- NULL
daily_df$`State Code` <- NULL
daily_df$`County Code` <- NULL
daily_df$`Defining Site` <- NULL
daily_df$`Number of Sites Reporting` <- NULL
fileName = paste("fst/" , "daily_all_aqi_by_county.fst", sep="")
write.fst(daily_df, fileName)

# Reading relevant information from hourly temp,wind,pollutant files, merge and save in and save in R data format

hourly_WIND_2018 <- fread("data/hourly_WIND_2018.csv",header = T, sep = ',')

hourly_WIND_2018_param_types <-split(hourly_WIND_2018, hourly_WIND_2018$`Parameter Name`)

hourly_WIND_2018_direction <- data.frame(hourly_WIND_2018_param_types[1])

names(hourly_WIND_2018_direction) <- substring(names(hourly_WIND_2018_direction),28,nchar(names(hourly_WIND_2018_direction)))

hourly_WIND_2018_speed <- data.frame(hourly_WIND_2018_param_types[2])

names(hourly_WIND_2018_speed) <- substring(names(hourly_WIND_2018_speed),24,nchar(names(hourly_WIND_2018_speed)))

hourly_TEMP_2018 <- fread("data/hourly_TEMP_2018.csv",header = T, sep = ',')

hourly_88101_2018 <- fread("data/hourly_88101_2018.csv",header = T, sep = ',')
hourly_81102_2018 <- fread("data/hourly_81102_2018.csv",header = T, sep = ',')
hourly_42101_2018 <- fread("data/hourly_42101_2018.csv",header = T, sep = ',')
hourly_44201_2018 <- fread("data/hourly_44201_2018.csv",header = T, sep = ',')
hourly_42602_2018 <- fread("data/hourly_42602_2018.csv",header = T, sep = ',')
hourly_42401_2018 <- fread("data/hourly_42401_2018.csv",header = T, sep = ',')

print("Aggregating hourly data")

hourly_42401_2018_group <-aggregate(hourly_42401_2018$`Sample Measurement`,by=list(hourly_42401_2018$`State Name`, hourly_42401_2018$`County Name`, hourly_42401_2018$`Date Local`,hourly_42401_2018$`Time Local`),FUN=mean)
hourly_42101_2018_group <-aggregate(hourly_42101_2018$`Sample Measurement`,by=list(hourly_42101_2018$`State Name`, hourly_42101_2018$`County Name`, hourly_42101_2018$`Date Local`,hourly_42101_2018$`Time Local`),FUN=mean)
hourly_42602_2018_group <-aggregate(hourly_42602_2018$`Sample Measurement`,by=list(hourly_42602_2018$`State Name`, hourly_42602_2018$`County Name`, hourly_42602_2018$`Date Local`,hourly_42602_2018$`Time Local`),FUN=mean)
hourly_44201_2018_group <-aggregate(hourly_44201_2018$`Sample Measurement`,by=list(hourly_44201_2018$`State Name`, hourly_44201_2018$`County Name`, hourly_44201_2018$`Date Local`,hourly_44201_2018$`Time Local`),FUN=mean)
hourly_88101_2018_group <-aggregate(hourly_88101_2018$`Sample Measurement`,by=list(hourly_88101_2018$`State Name`, hourly_88101_2018$`County Name`, hourly_88101_2018$`Date Local`,hourly_88101_2018$`Time Local`),FUN=mean)
hourly_81102_2018_group <-aggregate(hourly_81102_2018$`Sample Measurement`,by=list(hourly_81102_2018$`State Name`, hourly_81102_2018$`County Name`, hourly_81102_2018$`Date Local`,hourly_81102_2018$`Time Local`),FUN=mean)
hourly_WIND_2018_direction_group <-aggregate(hourly_WIND_2018_direction$Sample.Measurement,by=list(hourly_WIND_2018_direction$State.Name, hourly_WIND_2018_direction$County.Name, hourly_WIND_2018_direction$Date.Local,hourly_WIND_2018_direction$Time.Local),FUN=mean)
hourly_WIND_2018_speed_group <-aggregate(hourly_WIND_2018_speed$Sample.Measurement,by=list(hourly_WIND_2018_speed$State.Name, hourly_WIND_2018_speed$County.Name, hourly_WIND_2018_speed$Date.Local,hourly_WIND_2018_speed$Time.Local),FUN=mean)
hourly_TEMP_2018_group <-aggregate(hourly_TEMP_2018$`Sample Measurement`,by=list(hourly_TEMP_2018$`State Name`, hourly_TEMP_2018$`County Name`, hourly_TEMP_2018$`Date Local`,hourly_TEMP_2018$`Time Local`),FUN=mean)


setnames(hourly_42401_2018_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","SO2"))
setnames(hourly_42101_2018_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","CO"))
setnames(hourly_42602_2018_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","NO2"))
setnames(hourly_44201_2018_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","Ozone"))
setnames(hourly_88101_2018_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","PM2.5"))
setnames(hourly_81102_2018_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","PM10"))
setnames(hourly_WIND_2018_direction_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","Wind Direction"))
setnames(hourly_WIND_2018_speed_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","Wind Speed"))
setnames(hourly_TEMP_2018_group, old=c("Group.1","Group.2","Group.3","Group.4","x"), new=c("State Name", "County Name", "Date Local","Time Local","Temperature"))

print("Aggregation done for hourly data")

hourly_all_poll_2018<-Reduce(function(x,y) merge(x = x, y = y, c("State Name", "County Name", "Date Local","Time Local"),all=TRUE),
       list(hourly_42401_2018_group, hourly_42101_2018_group,hourly_42602_2018_group,hourly_44201_2018_group,hourly_88101_2018_group,hourly_81102_2018_group,
              hourly_WIND_2018_direction_group,hourly_WIND_2018_speed_group,hourly_TEMP_2018_group))
hourly_all_poll_2018$`Date Local` <- as.Date(hourly_all_poll_2018$`Date Local`)
hourly_all_poll_2018$Year <- format(hourly_all_poll_2018$`Date Local`, format = "%Y")
hourly_all_poll_2018$Month <- format(hourly_all_poll_2018$`Date Local`, format = "%B")
hourly_all_poll_2018$Day <- format(hourly_all_poll_2018$`Date Local`, format = "%d")

print("Reduction done for hourly data")

hourly_all_poll_2018$`Date Local` <- NULL
fileName = paste("fst/" , "hourly_all_data_2018.fst", sep="")
write.fst(hourly_all_poll_2018, fileName)



# Reading relevant information from daily pollutant files, merge and save in R data format
daily_88101_2018 <- fread("data/daily_88101_2018.csv",header = T, sep = ',')
daily_81102_2018 <- fread("data/daily_81102_2018.csv",header = T, sep = ',')
daily_42101_2018 <- fread("data/daily_42101_2018.csv",header = T, sep = ',')
daily_44201_2018 <- fread("data/daily_44201_2018.csv",header = T, sep = ',')
daily_42602_2018 <- fread("data/daily_42602_2018.csv",header = T, sep = ',')
daily_42401_2018 <- fread("data/daily_42401_2018.csv",header = T, sep = ',')

print("Aggregating daily pollutant data")

daily_42401_2018_group <-aggregate(daily_42401_2018$`Arithmetic Mean`,by=list(daily_42401_2018$`State Name`, daily_42401_2018$`County Name`, daily_42401_2018$`Date Local`),FUN=mean)
daily_42101_2018_group <-aggregate(daily_42101_2018$`Arithmetic Mean`,by=list(daily_42101_2018$`State Name`, daily_42101_2018$`County Name`, daily_42101_2018$`Date Local`),FUN=mean)
daily_42602_2018_group <-aggregate(daily_42602_2018$`Arithmetic Mean`,by=list(daily_42602_2018$`State Name`, daily_42602_2018$`County Name`, daily_42602_2018$`Date Local`),FUN=mean)
daily_44201_2018_group <-aggregate(daily_44201_2018$`Arithmetic Mean`,by=list(daily_44201_2018$`State Name`, daily_44201_2018$`County Name`, daily_44201_2018$`Date Local`),FUN=mean)
daily_88101_2018_group <-aggregate(daily_88101_2018$`Arithmetic Mean`,by=list(daily_88101_2018$`State Name`, daily_88101_2018$`County Name`, daily_88101_2018$`Date Local`),FUN=mean)
daily_81102_2018_group <-aggregate(daily_81102_2018$`Arithmetic Mean`,by=list(daily_81102_2018$`State Name`, daily_81102_2018$`County Name`, daily_81102_2018$`Date Local`),FUN=mean)

setnames(daily_42401_2018_group, old=c("Group.1","Group.2","Group.3","x"), new=c("State Name", "County Name", "Date Local","SO2"))
setnames(daily_42101_2018_group, old=c("Group.1","Group.2","Group.3","x"), new=c("State Name", "County Name", "Date Local","CO"))
setnames(daily_42602_2018_group, old=c("Group.1","Group.2","Group.3","x"), new=c("State Name", "County Name", "Date Local","NO2"))
setnames(daily_44201_2018_group, old=c("Group.1","Group.2","Group.3","x"), new=c("State Name", "County Name", "Date Local","Ozone"))
setnames(daily_88101_2018_group, old=c("Group.1","Group.2","Group.3","x"), new=c("State Name", "County Name", "Date Local","PM2.5"))
setnames(daily_81102_2018_group, old=c("Group.1","Group.2","Group.3","x"), new=c("State Name", "County Name", "Date Local","PM10"))

print("Aggregation done daily pollutant data")

daily_all_poll_2018<-Reduce(function(x,y) merge(x = x, y = y, c("State Name", "County Name", "Date Local"),all=TRUE),
                            list(daily_42401_2018_group, daily_42101_2018_group,daily_42602_2018_group,daily_44201_2018_group,daily_88101_2018_group,daily_81102_2018_group))

print("Reduction done for pollutant data")

daily_all_poll_2018$`Date Local` <- as.Date(daily_all_poll_2018$`Date Local`)
daily_all_poll_2018$Year <- format(daily_all_poll_2018$`Date Local`, format = "%Y")
daily_all_poll_2018$Month <- format(daily_all_poll_2018$`Date Local`, format = "%B")
daily_all_poll_2018$Day <- format(daily_all_poll_2018$`Date Local`, format = "%d")
daily_all_poll_2018$`Date Local` <- NULL
fileName = paste("fst/" , "daily_all_pollutants_2018.fst", sep="")
write.fst(daily_all_poll_2018, fileName)
