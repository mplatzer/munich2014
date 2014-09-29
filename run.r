
source("utils.r")

# fetch from Google Docs
require(RCurl)
require(data.table)
google.sheets.url <- "https://docs.google.com/spreadsheet/pub?key=0Ahq-EfFhbwPvdE1UQ2FrdGRmZS1hZnJMVzVKV2ZzbUE&single=true&gid=0&output=csv"
options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
myCsv <- getURL(google.sheets.url)
dt <- data.table(read.csv(textConnection(myCsv), na.strings=""))
dt[, date := as.Date(date, format="%Y-%m-%d")]

# fetch from runkeeper
#...

# build daily grid of 12 weeks prior a marathon 
marathon_dates <- dt[distance>42 & distance<43, list(date, preparation)]
grid <- data.table(merge(data.frame(marathon_dates), data.frame(days_until_race=0:(-12*7)), all=T))
grid[, date := date + days_until_race]
dt <- merge(grid, dt[, list(date, distance, duration)], by=c("date"), all.x=T)
setkey(dt, date)
dt[is.na(distance), distance := 0]

# calculate metrics
dt[, pace := duration / distance]
dt[, speed := distance / (duration/60) ]
dt[!is.na(speed), speed_ma7 := wapply(speed*distance, sum, 7, 0, na.rm=T) / wapply(distance, sum, 7, 0, na.rm=T), by="preparation"]
dt[distance > 42, speed_ma7 := speed]
dt[, dist_ma7  := wapply(distance, mean,  7, 0, na.rm=T), by="preparation"]
dt[, dist_ma14 := wapply(distance, mean, 14, 0, na.rm=T), by="preparation"]
dt[, dist_ma42 := wapply(distance, mean, 42, 0, na.rm=T), by="preparation"]
dt <- dt[date < Sys.Date()]

# write output
write.csv(dt, "running.csv", row.names=F, na="")
