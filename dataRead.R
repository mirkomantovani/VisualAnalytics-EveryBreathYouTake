# SCript to get all the data for the project from the EPA website

library(rvest)

# get urls for file download 

pg <- read_html("https://aqs.epa.gov/aqsweb/airdata/download_files.html")
links <- data.frame(url = html_attr(html_nodes(pg, "a"), "href"))
d_urls <- data.frame(file_name  = links[grep("hourly_WIND_2018", links$url),])
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("hourly_TEMP_2018", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("hourly_44201_2018", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("hourly_42401_2018", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("hourly_42101_2018", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("hourly_42602_2018", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("hourly_88101_2018", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("hourly_81102_2018", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("daily_aqi_by_county", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("daily_44201_2018", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("daily_42401_2018", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("daily_42101_2018", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("daily_42602_2018", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("daily_88101_2018", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("daily_81102_2018", links$url),]))

d_urls$url_name <- paste("https://aqs.epa.gov/aqsweb/airdata/", d_urls$file_name,sep="")

oldw <- getOption("warn")
options(warn = -1)

#Check if the folder "Data" exists in the current directory, if not creates it
ifelse(!dir.exists("data"), dir.create("data"),"")
setwd(paste0(getwd(), "/data"))

# download the relevant files 

for (row in 1:nrow(d_urls)) {
  link <- d_urls[row,"url_name"]
  if(!file.exists(basename(link))){
    print(basename(link))
    print("File not found")
    tryCatch(download.file(link,file.path(basename(link)), method = "libcurl"),
             error = function(e) print(paste(link, 'did not work out')))
    }
  }

options(warn = oldw)

# unzip the data files 
files <- list.files(pattern=".*.zip", full.names=TRUE, recursive=FALSE)
sapply(files, unzip)

setwd("../")
