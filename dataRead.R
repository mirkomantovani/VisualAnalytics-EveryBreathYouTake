# SCript to get all the data for the project from the EPA website

library(rvest)

pg <- read_html("https://aqs.epa.gov/aqsweb/airdata/download_files.html")
links <- data.frame(url = html_attr(html_nodes(pg, "a"), "href"))
d_urls <- data.frame(file_name  = links[grep("hourly_WIND", links$url),])
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("hourly_TEMP", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("hourly_44201", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("hourly_42401", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("hourly_42101", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("hourly_42602", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("hourly_88101", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("hourly_81102", links$url),]))
d_urls <- rbind(d_urls,data.frame(file_name = links[grep("daily_aqi_by_county", links$url),]))


d_urls$url_name <- paste("https://aqs.epa.gov/aqsweb/airdata/", d_urls$file_name,sep="")

workingdir <-getwd()

oldw <- getOption("warn")
options(warn = -1)

for (row in 1:nrow(d_urls)) {
  link <- d_urls[row,"url_name"]
  tryCatch(download.file(link,file.path("data",basename(link)), method = "libcurl"),
           error = function(e) print(paste(link, 'did not work out')))    
  
  }
options(warn = oldw)

# unzip(basename(link), files = NULL, list = FALSE, overwrite = TRUE,
#       junkpaths = FALSE, exdir = ".", unzip = "internal",
#       setTimes = FALSE)
