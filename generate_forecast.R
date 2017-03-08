#!/usr/bin/env Rscript
date.string <- format(Sys.time(), "%Y-%m-%d")

start.forecast <- as.integer(as.Date(date.string) - as.Date("1858-11-17"))

url <- "https://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now"
destfile <- "eopc04_IAU2000.62-now.txt"
download.file(url, destfile, method = "auto", quiet=TRUE)
if(!file.exists("eopc04_IAU2000.62-now.txt")) {
  stop("c04 file does not exist", call.=FALSE)
}

suppressMessages(library(Rssa))

c04 <- read.table("eopc04_IAU2000.62-now.txt", comment.char = "#", skip = 14)
colnames(c04) <- c("Year", "Month", "Day", "MJD", "x", "y", "UT1-UTC", "LOD",
                   "dX", "dY", "x Err", "y Err", "UT1-UTC err", "LOD err", "dX err", "dY err")


MSE <- function(a, b, n) {
  sum((a - b)**2) / n
}

find.n <- function(coord, start, end, L, n, len, type, steps) {
  s <- c()
  x <- c()
  for(i in 0:(steps - 1)) {
    l <- start + i * len
    r <- end + i * len
    s[[i + 1]] <- ssa(c04[l:r, coord], L = L, neig = n)
    x[[i + 1]] <- c04[(r + 1):(r + len), coord]
  }
  
  dists <- c()
  for(num in 1:n) {
    r <- c()
    for(i in 0:(steps - 1)) {
      rf <- rforecast(s[[i + 1]], groups = list(1:num), len = len, only.new = TRUE)
      r <- c(r, MSE(rf, x[[i + 1]], len))
    }
    dists <- c(dists, ifelse(type == "mean", mean(r), median(r)))
    rm(r)
    gc()
  }
  rm(x, s)
  gc()
  
  return(which.min(dists))
}

get.forecast <- function(len, fixed.L = T, pn, lodn, dpn, steps) {
  # index corresponding to the starting point of forecast
  ind <- start.forecast - 37664
  fin <- ind - 1
  
  dstart <- as.numeric(ISOdate(2005, 1, 1) - ISOdate(1962, 1, 1), units = "days") + 1
  
  # use last 40 years
  period <- 40 * 365
  x <- c04[(fin - period + 1):fin, "x"]
  y <- c04[(fin - period + 1):fin, "y"]
  lod <- c04[(fin - period + 1):fin, "LOD"]
  
  if(fixed.L) {
    L <- 800
  } else {
    L <- length(x) %/% 3
  }
  
  s.x <- ssa(x, L = L, neig = pn)
  s.y <- ssa(y, L = L, neig = pn)
  s.lod <- ssa(lod, L = L, neig = lodn)
  
  end <- fin - steps * len
  start <- end - period + 1
  xp <- find.n("x", start, end, L, pn, len, "mean", steps)
  yp <- find.n("y", start, end, L, pn, len, "mean", steps)
  lodp <- find.n("LOD", start, end, L, lodn, len, "mean", steps)
  
  rf.x <- rforecast(s.x, groups = list(1:xp), len = len, only.new = TRUE)
  rf.y <- rforecast(s.y, groups = list(1:yp), len = len, only.new = TRUE)
  rf.lod <- rforecast(s.lod, groups = list(c(1:lodp)), len = len, only.new = TRUE)
  
  print(paste("len =", len, "; L =", L, ":", xp, yp, lodp, sep = " "))
  df <- data.frame(MJD = start.forecast:(start.forecast + len - 1), x = rf.x, y = rf.y, LOD = rf.lod)
}

df.90.small <- get.forecast(90, fixed.L = T, 50, 30, 10, steps = 12)
df.90.big <- get.forecast(90, fixed.L = F, 100, 30, 10, steps = 12) 

my.write <- function(x, file, header, f = write.table, ...){
  # create and open the file connection
  datafile <- file(file, open = 'wt')
  # close on exit 
  on.exit(close(datafile))
  # if a header is defined, write it to the file
  if(!missing(header)) {
    writeLines(header,con = datafile, sep='\t')
    writeLines('', con=datafile, sep='\n')
  }
  # write the file using the defined function and required addition arguments  
  f(x, datafile,...)
}


output.file.name.90.small <- paste(start.forecast - 1, "_ssa_spbu_90_small_L.txt", sep = "")
output.file.name.90.big <- paste(start.forecast - 1, "_ssa_spbu_90_big_L.txt", sep = "")

my.write(format(df.90.small, scientific = FALSE), file = output.file.name.90.small,
         header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD",
         row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)

my.write(format(df.90.big, scientific = FALSE), file = output.file.name.90.big,
         header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD",
         row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)