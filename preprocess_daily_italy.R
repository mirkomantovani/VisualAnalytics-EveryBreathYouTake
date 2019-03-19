#setwd("~/VisualAnalytics-EveryBreathYouTake/italy/")
daily_files <- list.files(path="./italy", pattern="*.csv", full.names=TRUE, recursive=FALSE)
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
df = data.frame(city=character(),day=integer(0),month=integer(0),year=integer(0),no2=double(0),so2=double(0),co=double(0),pm10=double(0),pm25=double(0),o3=double(0),value=integer(0))
names(df) = c("city","day","month","year","no2","so2","co","pm10","pm25","o3")
pollutants <- c("no2","so2","co","pm10","pm25","o3")
for(city_i in cities_italy)
{
  city_val <- tolower(city_i)
  print(paste("Processing city ",city_i))
  for(year_i in 18:19)
  {
    for(month_i in 1:12)
    {
      if(month_i <10)
      {
        month_val = paste0("0",month_i)
      }
      else
      {
        month_val = paste0("",month_i)
      }
      
      for(day_i in 1:31)
      {
        if(day_i <10)
        {
          day_val = paste0("0",day_i)
        }
        else
        {
          day_val = paste0("",day_i)
        }
        avg_day <- subset(b,city==city_i & day==day_val & month==month_val & year==paste0("",year_i))
        # print(paste(day_val,month_val,year_i))
        if(length(avg_day$city)==0)
          next
        avg = c(0,0,0,0,0,0)
        names(avg) <- pollutants
        for(p in pollutants)
        {
          temp <- subset(avg_day,parameter==p)
          avg[p] <- mean(temp$value)
          if(is.nan(avg[p]))
          {
            avg[p] = NaN
            next
          }
        }
        df_row = data.frame(city_val,day_val,month_val,paste0("",year_i),avg[1],avg[2],avg[3],avg[4],avg[5],avg[6])
        names(df_row) = c("city","day","month","year","no2","so2","co","pm10","pm25","o3")
        df <- rbind(df,df_row)   
        # bb<-which.max(avg)
      
      }
    }
  }
}
print(df)
library(fst)
fileName = "./italy/daily_italy.fst"
write.fst(df, fileName)