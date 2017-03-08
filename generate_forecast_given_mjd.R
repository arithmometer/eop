#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)
start.forecast <- as.integer(args[1])

if(!file.exists("eopc04_IAU2000.62-now.txt")) {
  url <- "https://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now"
  destfile <- "eopc04_IAU2000.62-now.txt"
  download.file(url, destfile, method = "auto", quiet=TRUE)
  if(!file.exists("eopc04_IAU2000.62-now.txt")) {
    stop("c04 file does not exist", call.=FALSE)
  }
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
  
  dists <- parLapply(cl, 1:n, function(num) {
    r <- c()
    for(i in 0:(steps - 1)) {
      rf <- rforecast(s[[i + 1]], groups = list(1:num), len = len, only.new = TRUE)
      r <- c(r, MSE(rf, x[[i + 1]], len))
    }
    ifelse(type == "mean", mean(r), median(r))
  })
  rm(x, s)
  gc()
  return(which.min(unlist(dists)))
}

get.forecast <- function(start.forecast, len, L, lodL, dL, pn, lodn, dpn, years, dyears, steps) {
  # index corresponding to the beginning of forecast
  ind <- start.forecast - 37664
  fin <- ind - 1
  
  # make predictions using last "years" years
  period <- years * 365
  x <- c04[(fin - period + 1):fin, "x"]
  y <- c04[(fin - period + 1):fin, "y"]
  lod <- c04[(fin - period + 1):fin, "LOD"]
  # for dX, dY corrections use last "dyears" years
  dperiod <- dyears * 365
  dX <- c04[(fin - dperiod + 1):fin, "dX"]
  dY <- c04[(fin - dperiod + 1):fin, "dY"]
  
  s.x <- ssa(x, L = L, neig = pn)
  s.y <- ssa(y, L = L, neig = pn)
  s.lod <- ssa(lod, L = L, neig = lodn)
  s.dx <- ssa(dX, L = dL, neig = dpn)
  s.dy <- ssa(dY, L = dL, neig = dpn)
  
  end <- fin - steps * len
  start <- end - period + 1
  dstart <- end - dperiod + 1
  xp <- find.n("x", start, end, L, pn, len, "mean", steps)
  yp <- find.n("y", start, end, L, pn, len, "mean", steps)
  lodp <- find.n("LOD", start, end, lodL, lodn, len, "mean", steps)
  dxp <- find.n("dX", dstart, end, dL, dpn, len, "mean", steps)
  dyp <- find.n("dY", dstart, end, dL, dpn, len, "mean", steps)
  
  rf.x <- rforecast(s.x, groups = list(1:xp), len = len, only.new = TRUE)
  rf.y <- rforecast(s.y, groups = list(1:yp), len = len, only.new = TRUE)
  rf.lod <- rforecast(s.lod, groups = list(c(1:lodp)), len = len, only.new = TRUE)
  rf.dx <- rforecast(s.dx, groups = list(c(1:dxp)), len = len, only.new = TRUE)
  rf.dy <- rforecast(s.dy, groups = list(c(1:dyp)), len = len, only.new = TRUE)
  
  print(paste("len =", len, "; L =", L, lodL, dL, ":", xp, yp, lodp, dxp, dyp, sep = " "))
  df <- data.frame(MJD = start.forecast:(start.forecast + len - 1), x = rf.x, y = rf.y, LOD = rf.lod, dX = rf.dx, dY = rf.dy)
}

my.write <- function(x, file, header, f = write.table, ...) {
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

df.365 <- get.forecast(start.forecast, 365, 1500, 2000, 250, pn=50, lodn=70, dpn=30, years=20, dyears=10, steps=5)
df.90 <- get.forecast(start.forecast, 90, 1000, 600, 500, pn=50, lodn=50, dpn=30, years=20, dyears=10, steps=12)

output.file.name.365 <- paste("ssa/", start.forecast, "_ssa_spbu_365.txt", sep = "")
output.file.name.90 <- paste("ssa/", start.forecast, "_ssa_spbu_90.txt", sep = "")

my.write(format(df.365, scientific = FALSE), file = output.file.name.365,
         header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD\t\t\tdX\t\t\tdY",
         row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)

my.write(format(df.90, scientific = FALSE), file = output.file.name.90,
         header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD\t\t\tdX\t\t\tdY",
         row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)