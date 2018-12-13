prefix <- "/srv/shiny-server/eop/"

# deprecated urls
# url_finals2000a <- "https://datacenter.iers.org/eop/-/somos/5Rgv/latestXL/13/finals2000A.daily/csv"
# url_finals_daily <- "https://datacenter.iers.org/eop/-/somos/5Rgv/latestXL/12/finals.daily/csv"
# url_gpsrapid <- "https://datacenter.iers.org/eop/-/somos/5Rgv/latestXL/14/gpsrapid.daily/csv"

# urls
url_finals2000a <- "https://datacenter.iers.org/data/csv/finals2000A.daily.csv"
url_finals_daily <- "https://datacenter.iers.org/data/csv/finals.daily.csv"
url_gpsrapid <- "https://datacenter.iers.org/data/csv/gpsrapid.daily.csv"

# date of today
date.string <- format(Sys.time(), "%Y-%m-%d")

# Modified Julian Date time format. This represents the number of days 
# since midnight on November 17, 1858. For example, 51544.0 in MJD is midnight on January 1, 2000.
mjd.string <- as.character(as.Date(date.string) - as.Date("1858-11-17"))

dir.create(paste(prefix, "csv/", mjd.string, sep=""))

download.file(url_finals2000a, paste(prefix, "csv/", mjd.string, "/finals2000A.daily.csv", sep=""), method="auto", quiet=TRUE)
download.file(url_finals_daily, paste(prefix, "csv/", mjd.string, "/finals.daily.csv", sep=""), method="auto", quiet=TRUE)
download.file(url_gpsrapid, paste(prefix, "csv/", mjd.string, "/gpsrapid.daily.csv", sep=""), method="auto", quiet=TRUE)
