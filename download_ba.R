get_file <- function(filename, url) {
  download.file(url, destfile=filename)
}

prefix <- "/srv/shiny-server/eop/"
url.prefix <- "https://datacenter.iers.org/eop/-/somos/5Rgv/latestXL/6/"

date.string <- format(Sys.time(), "%Y-%m-%d")

year <- as.numeric(format(as.Date(date.string), '%Y'))
volume <- year - 1987
week <- as.numeric(format(as.Date(date.string), '%U'))
mjd <- as.integer(as.Date(date.string) - as.Date("1858-11-17"))

filename <- sprintf("bulletina-%s-%03d.txt/csv", tolower(as.roman(volume)), week)
path <- paste(prefix, "ba/", substr(filename, 1, nchar(filename)-4), sep="")
get_file(path, paste(url.prefix, filename, sep=""))
