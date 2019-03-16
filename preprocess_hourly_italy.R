#setwd("/Users/abhishekv/Desktop/p2/VisualAnalytics-EveryBreathYouTake/italy/")
daily_files <- list.files(path="./", pattern="*.csv", full.names=TRUE, recursive=FALSE)
daily_aqi = lapply(daily_files, read.csv)
a <- do.call(rbind, daily_aqi)

drops <- c("country","unit","attribution","local")
b <- a[ , !(names(a) %in% drops)]

b$date = as.Date(b$utc)
b$day = format(b$date,"%d")
b$month = format(b$date,"%m")
b$year = format(b$date,"%y")
b["date"] <- as.POSIXct(b$utc,"%Y-%m-%dT%H:%M:%S", tz="UTC")
b$time <- format(b$date,"%H:%M")
drops <- c("utc","longitude","latitude","date")
b <- b[ , !(names(b) %in% drops)]


locations_italy<-unique(b$location)
cities_italy <- unique(b$city)

# print(locations_italy)
# print(cities_italy)

#avg calculation example
df = data.frame(city=character(),day=integer(0),month=integer(0),year=integer(0),time=character(),no2=double(0),so2=double(0),co=double(0),pm10=double(0),pm25=double(0),o3=double(0))
names(df) = c("City","Day","Month","Year","Time","no2","so2","co","pm10","pm25","o3")
pollutants <- c("no2","so2","co","pm10","pm25","o3")


for(city_i in cities_italy)
{
  print(paste("Processing city",city_i))
  city_val <- tolower(city_i)
  avg_day4 <- subset(b,city==city_i)
  months_names = c("January","February","March","April","May","June","July","August","September","October","November","December")
  for(year_i in 18:19)
  {
    if(year_i==18)
    months_d = c(12)
    else
    months_d = c(1,2,3)
    avg_day3 <- subset(avg_day4,year==paste0("",year_i))
    for(month_i in months_d)
    {
      month_val_name <- months_names[month_i]
      if(month_i <10)
      {
        month_val = paste0("0",month_i)
      }
      else
      {
        month_val = paste0("",month_i)
      }
      if(month_i == 3)
      day_d <- 1:10
        else
      day_d <- 1:31
      avg_day2 <- subset(avg_day3,month==month_val)
      for(day_i in day_d)
      {
        if(day_i <10)
        {
          day_val = paste0("0",day_i)
        }
        else
        {
          day_val = paste0("",day_i)
        }
        avg_day1 <- subset(avg_day2,day==day_val)
        for(hours_i in 0:24)
        {
          if(hours_i <10)
          {
            hours_val <- paste0("0",hours_i,":00")
          }
          else
          {
            hours_val <- paste0(hours_i,":00")
          }
 
          avg_day <- subset(avg_day1,time==hours_val)
        # print(paste(day_val,month_val,year_i))
        if(length(avg_day$city)==0)
          next
        avg = c(0,0,0,0,0,0)
        names(avg) <- pollutants
        for(p in pollutants)
        {
          temp <- subset(avg_day,parameter==p)
          if(length(temp$value)==0)
          {
          avg[p] <- NA
          next
          }
          avg[p] <- mean(temp$value)
          # if(is.nan(avg[p]))
          # {
          #   avg[p] = NA
          #   next
          # }
        }
        df_row = data.frame(city_val,day_val,month_val_name,paste0("20",year_i),hours_val,avg[1],avg[2],avg[3],avg[4],avg[5],avg[6])
        names(df_row) = c("City","Day","Month","Year","Time","no2","so2","co","pm10","pm25","o3")
        df <- rbind(df,df_row)   
        # bb<-which.max(avg)
        }
      }
    }
  }
}
print(df)
names(hourly_df_italy)[6:11] <- toupper(names(hourly_df_italy)[6:11])
names(hourly_df_italy)[10] <- "PM2.5"
library(fst)
fileName = "./hourly_italy.fst"
write.fst(df, fileName)