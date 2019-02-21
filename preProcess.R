
library(data.table)

# Reading relevant information from daily county files and save in R data format

daily_files <- list.files(path="data", pattern="daily_aqi_by_county.*.csv", full.names=TRUE, recursive=FALSE)

files_with_data <- daily_files[-(1:10)]

lapply(files_with_data, function(x) {
  df <- fread(x, select = c("State Name","county Name","Date","AQI","Category","Defining Parameter")
             ,header = T, sep = ',')
  fileName = paste(substring(x, 6,29) , ".Rda", sep="");
  ifelse(!dir.exists("rda"), dir.create("rda"),"")
  fileName = paste("rda/" , fileName, sep="")
  saveRDS(df, file=fileName)
  }
)


# Reading relevant information from hourly temp,wind,pollutant files and save in and save in R data format

files <- list.files(path="data", pattern="hourly.*.csv", full.names=TRUE, recursive=FALSE)

lapply(files, function(x) {
  df <- fread(x, select = c("Date Local",	"Time Local"	,"Sample Measurement","State Name",	"County Name")
               ,header = T, sep = ',')
  fileName = paste(substring(x, 6,21) , ".Rda", sep="")
  ifelse(!dir.exists("rda"), dir.create("rda"),"")
  fileName = paste("rda/" , fileName, sep="")
  saveRDS(df, file=fileName)
  }
)
