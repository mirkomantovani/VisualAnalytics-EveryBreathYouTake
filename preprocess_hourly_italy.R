library(fst)

daily_files <- list.files(path="italy", pattern="*.csv", full.names=TRUE, recursive=FALSE)
daily_aqi = lapply(daily_files, read.csv)
daily_aqi_all <- do.call(rbind, daily_aqi)

drops <- c("country","unit","attribution","local","longitude","latitude")
daily_aqi_sel <- daily_aqi_all[ , !(names(daily_aqi_all) %in% drops)]
daily_aqi_sel$date = as.Date(daily_aqi_sel$utc)
daily_aqi_sel$day = format(daily_aqi_sel$date,"%d")
daily_aqi_sel$month = format(daily_aqi_sel$date,"%B")
daily_aqi_sel$year = format(daily_aqi_sel$date,"%Y")
daily_aqi_sel["date"] <- as.POSIXct(daily_aqi_sel$utc,"%Y-%m-%dT%H:%M:%S", tz="UTC")
daily_aqi_sel$time <- format(daily_aqi_sel$date,"%H:%M")
drops <- c("date","utc")
daily_aqi_sel <- daily_aqi_sel[ , !(names(daily_aqi_sel) %in% drops)]


daily_aqi_param_types <-split(daily_aqi_sel, daily_aqi_sel$`parameter`)

daily_aqi_CO <- data.frame(daily_aqi_param_types[1])
names(daily_aqi_CO) <- substring(names(daily_aqi_CO),4,nchar(names(daily_aqi_CO)))
daily_aqi_CO$parameter <- NULL

daily_aqi_NO2 <- data.frame(daily_aqi_param_types[2])
names(daily_aqi_NO2) <- substring(names(daily_aqi_NO2),5,nchar(names(daily_aqi_NO2)))
daily_aqi_NO2$parameter <- NULL

daily_aqi_PM10 <- data.frame(daily_aqi_param_types[3])
names(daily_aqi_PM10) <- substring(names(daily_aqi_PM10),6,nchar(names(daily_aqi_PM10)))
daily_aqi_PM10$parameter <- NULL

daily_aqi_PM2.5 <- data.frame(daily_aqi_param_types[4])
names(daily_aqi_PM2.5) <- substring(names(daily_aqi_PM2.5),6,nchar(names(daily_aqi_PM2.5)))
daily_aqi_PM2.5$parameter <- NULL

daily_aqi_SO2 <- data.frame(daily_aqi_param_types[5])
names(daily_aqi_SO2) <- substring(names(daily_aqi_SO2),5,nchar(names(daily_aqi_SO2)))
daily_aqi_SO2$parameter <- NULL

daily_aqi_O3 <- data.frame(daily_aqi_param_types[6])
names(daily_aqi_O3) <- substring(names(daily_aqi_O3),4,nchar(names(daily_aqi_O3)))
daily_aqi_O3$parameter <- NULL

daily_aqi_NO2_group <-aggregate(daily_aqi_NO2$value,by=list(daily_aqi_NO2$city,
                                                        daily_aqi_NO2$day,daily_aqi_NO2$month,daily_aqi_NO2$year,daily_aqi_NO2$time),FUN=mean)

daily_aqi_SO2_group <-aggregate(daily_aqi_SO2$value,by=list(daily_aqi_SO2$city,
                                                                         daily_aqi_SO2$day,daily_aqi_SO2$month,daily_aqi_SO2$year,daily_aqi_SO2$time),FUN=mean)

daily_aqi_CO_group <-aggregate(daily_aqi_CO$value,by=list(daily_aqi_CO$city,
                                                                         daily_aqi_CO$day,daily_aqi_CO$month,daily_aqi_CO$year,daily_aqi_CO$time),FUN=mean)

daily_aqi_PM10_group <-aggregate(daily_aqi_PM10$value,by=list(daily_aqi_PM10$city,
                                                              daily_aqi_PM10$day,daily_aqi_PM10$month,daily_aqi_PM10$year,daily_aqi_PM10$time),FUN=mean)
daily_aqi_PM2.5_group <-aggregate(daily_aqi_PM2.5$value,by=list(daily_aqi_PM2.5$city,
                                                                         daily_aqi_PM2.5$day,daily_aqi_PM2.5$month,daily_aqi_PM2.5$year,daily_aqi_PM2.5$time),FUN=mean)
daily_aqi_O3_group <-aggregate(daily_aqi_O3$value,by=list(daily_aqi_O3$city,
                                                                         daily_aqi_O3$day,daily_aqi_O3$month,daily_aqi_O3$year,daily_aqi_O3$time),FUN=mean)

print("Aggregation done daily pollutant data")


setnames(daily_aqi_NO2_group, old=c("Group.1","Group.2","Group.3","Group.4","Group.5","x"), new=c("City","Day","Month","Year","Time","NO2"))
setnames(daily_aqi_SO2_group, old=c("Group.1","Group.2","Group.3","Group.4","Group.5","x"), new=c("City","Day","Month","Year","Time","SO2"))
setnames(daily_aqi_CO_group, old=c("Group.1","Group.2","Group.3","Group.4","Group.5","x"), new=c("City","Day","Month","Year","Time","CO"))
setnames(daily_aqi_PM10_group, old=c("Group.1","Group.2","Group.3","Group.4","Group.5","x"), new=c("City","Day","Month","Year","Time","PM10"))
setnames(daily_aqi_PM2.5_group, old=c("Group.1","Group.2","Group.3","Group.4","Group.5","x"), new=c("City","Day","Month","Year","Time","PM2.5"))
setnames(daily_aqi_O3_group, old=c("Group.1","Group.2","Group.3","Group.4","Group.5","x"), new=c("City","Day","Month","Year","Time","O3"))


daily_aqi_all_italy<-Reduce(function(x,y) merge(x = x, y = y, c("City", "Year","Day","Month","Time"),all=TRUE),
                            list(daily_aqi_NO2_group, daily_aqi_SO2_group,daily_aqi_CO_group,daily_aqi_PM10_group,daily_aqi_PM2.5_group,daily_aqi_O3_group))

print("Reduction done for pollutant data")


fileName = "italy/hourly_italy.fst"
write.fst(daily_aqi_all_italy, fileName)
