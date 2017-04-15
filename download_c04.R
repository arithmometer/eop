prefix <- "/srv/shiny-server/eop/"

url <- "https://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now"
destfile <- paste(prefix, "eopc04_IAU2000.62-now.txt", sep="")
download.file(url, destfile, method = "auto", quiet=TRUE)