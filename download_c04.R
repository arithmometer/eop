url <- "https://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now"
destfile <- "eopc04_IAU2000.62-now.txt"
download.file(url, destfile, method = "auto", quiet=TRUE)
