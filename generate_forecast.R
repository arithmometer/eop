#!/usr/bin/env Rscript
library(methods)
suppressMessages(library(plotly))
suppressMessages(library(Rssa))

mjd.given <- FALSE
args = commandArgs(trailingOnly=TRUE)
if(length(args) > 0) {
  mjd.given <- TRUE
  start.forecast <- as.integer(args[1])
} else {
date.string <- format(Sys.time(), "%Y-%m-%d")
# put to start.forecast MJD of today
start.forecast <- as.integer(as.Date(date.string) - as.Date("1858-11-17"))
}

prefix <- "/srv/shiny-server/eop/"
# prefix <- "/home/grigory/data/R/eop/"

url <- "https://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now"
destfile <- paste(prefix, "eopc04_IAU2000.62-now.txt", sep="")
# download.file(url, destfile, method = "auto", quiet=TRUE)
# if(!file.exists(destfile)) {
#   stop("c04 file does not exist", call.=FALSE)
# }

c04 <- read.table(destfile, comment.char = "#", skip = 14)
colnames(c04) <- c("Year", "Month", "Day", "MJD", "x", "y", "UT1-UTC", "LOD",
                   "dX", "dY", "x Err", "y Err", "UT1-UTC err", "LOD err", "dX err", "dY err")

MSE <- function(a, b, n) {
  sum((a - b)**2) / n
}

find.n <- function(start.forecast, coord, start, end, L, n, len, type, steps, for.today) {
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
  
  write.csv(dists, paste(paste(prefix, "ssa/params/", start.forecast, "/", coord, sep=""), len, L, "dists.csv", sep="_"))
  if(for.today) {
    write.csv(dists, paste(paste(prefix, "today/", coord, sep=""), len, L, "dists.csv", sep="_"))
  }
  
  return(list(which.min(dists), min(dists)))
}

find.Ln <- function(start.forecast, coord, start, end, L.list, pn, len, type, steps, for.today) {
  best.L <- L.list[1]
  best.n <- 1
  mindist <- Inf
  for(L in L.list) {
    res <- find.n(start.forecast, coord, start, end, L, pn, len, "mean", steps, for.today)
    if(res[[2]] < mindist) {
      mindist <- res[[2]]
      best.n <- res[[1]]
      best.L <- L
    }
  }
  return(list(best.L, best.n))
}

get.forecast <- function(start.forecast, len, L.list, lodL.list, dL.list, pn, lodn, dpn, years, dyears, steps, for.today) {
  dir.create(paste(prefix, "ssa/params/", start.forecast, "/", sep=""), showWarnings = FALSE)
  write.csv(L.list, paste(prefix, "ssa/params/", start.forecast, "/x_", len, "_L_list.csv", sep=""))
  write.csv(L.list, paste(prefix, "ssa/params/", start.forecast, "/y_", len, "_L_list.csv", sep=""))
  write.csv(lodL.list, paste(prefix, "ssa/params/", start.forecast, "/LOD_", len, "_L_list.csv", sep=""))
  write.csv(dL.list, paste(prefix, "ssa/params/", start.forecast, "/dX_", len, "_L_list.csv", sep=""))
  write.csv(dL.list, paste(prefix, "ssa/params/", start.forecast, "/dY_", len, "_L_list.csv", sep=""))
  if(for.today) {
    write.csv(L.list, paste(prefix, "today/x_", len, "_L_list.csv", sep=""))
    write.csv(L.list, paste(prefix, "today/y_", len, "_L_list.csv", sep=""))
    write.csv(lodL.list, paste(prefix, "today/LOD_", len, "_L_list.csv", sep=""))
    write.csv(dL.list, paste(prefix, "today/dX_", len, "_L_list.csv", sep=""))
    write.csv(dL.list, paste(prefix, "today/dY_", len, "_L_list.csv", sep=""))
  }
  
  # index corresponding to the beginning of forecast
  ind <- start.forecast - 37664
  fin <- ind - 1
  
  # make predictions using last "years" years
  period <- years * 365
  x <- c04[(fin - period + 1):fin, "x"]
  y <- c04[(fin - period + 1):fin, "y"]
  lod <- c04[(fin - period):(fin - 1), "LOD"]
  # for dX, dY corrections use last "dyears" years
  dperiod <- dyears * 365
  dX <- c04[(fin - dperiod + 1):fin, "dX"]
  dY <- c04[(fin - dperiod + 1):fin, "dY"]
  
  end <- fin - steps * len
  start <- end - period + 1
  dstart <- end - dperiod + 1
  xp <- find.Ln(start.forecast, "x", start, end, L.list, pn, len, "mean", steps, for.today)
  yp <- find.Ln(start.forecast, "y", start, end, L.list, pn, len, "mean", steps, for.today)
  lodp <- find.Ln(start.forecast, "LOD", start - 1, end - 1, lodL.list, lodn, len, "mean", steps, for.today)
  dxp <- find.Ln(start.forecast, "dX", dstart, end, dL.list, dpn, len, "mean", steps, for.today)
  dyp <- find.Ln(start.forecast, "dY", dstart, end, dL.list, dpn, len, "mean", steps, for.today)
  
  rf.x <- rforecast(ssa(x, L = xp[[1]], neig = xp[[2]]), groups = list(1:xp[[2]]), len = len, only.new = TRUE)
  rf.y <- rforecast(ssa(y, L = yp[[1]], neig = yp[[2]]), groups = list(1:yp[[2]]), len = len, only.new = TRUE)
  rf.lod <- rforecast(ssa(lod, L = lodp[[1]], neig = lodp[[2]]), groups = list(c(1:lodp[[2]])), len = len + 1, only.new = TRUE)
  rf.dx <- rforecast(ssa(dX, L = dxp[[1]], neig = dxp[[2]]), groups = list(c(1:dxp[[2]])), len = len, only.new = TRUE)
  rf.dy <- rforecast(ssa(dY, L = dyp[[1]], neig = dyp[[2]]), groups = list(c(1:dyp[[2]])), len = len, only.new = TRUE)
  
  params <- data.frame(x=c(xp[[1]], xp[[2]]),
                       y=c(yp[[1]], yp[[2]]),
                       LOD=c(lodp[[1]], lodp[[2]]),
                       dX=c(dxp[[1]], dxp[[2]]),
                       dY=c(dyp[[1]], dyp[[2]])
  )
  write.csv(params, paste(prefix, "ssa/params/", start.forecast, "/", len, "params.csv", sep=""))
  if(for.today) {
    write.csv(params, paste(prefix, "today/", len, "params.csv", sep=""))
  }
  
  df <- data.frame(MJD = start.forecast:(start.forecast + len - 1), x = rf.x, y = rf.y, LOD = rf.lod[-1], dX = rf.dx, dY = rf.dy)
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

if(!mjd.given) {
  finals2000 <- read.csv(paste(prefix, "csv/", start.forecast - 1, "/finals2000A.daily.csv", sep=""), sep=";")
  
  last.mjd.c04 <- c04[nrow(c04), "MJD"]
  mjd.from <- last.mjd.c04 + 1
  mjd.to <- start.forecast - 1
  ind.from <- which(finals2000["MJD"] == mjd.from)
  ind.to <- which(finals2000["MJD"] == mjd.to)
  
  gap.df <- data.frame("MJD"=mjd.from:mjd.to,
                       "x"=finals2000[ind.from:ind.to, "x_pole"],
                       "y"=finals2000[ind.from:ind.to, "y_pole"],
                       "LOD"=finals2000[ind.from:ind.to, "LOD"] / 1000,
                       "dX"=finals2000[ind.from:ind.to, "dX"] / 1000,
                       "dY"=finals2000[ind.from:ind.to, "dY"] / 1000)
  
  c04 <- rbind(c04[, c("MJD", "x", "y", "LOD", "dX", "dY")], gap.df)
}

df.365 <- get.forecast(start.forecast, 365,
                       L.list=c(400, 500, 550, 600, 650, 700, 800, 1000),
                       lodL.list=c(2500, 2700, 2750, 2800, 2850, 3000, 4000),
                       dL.list=c(200, 250, 270, 300, 320, 400),
                       pn=30, lodn=30, dpn=5, years=20, dyears=20, steps=5, for.today=TRUE)
df.90 <- get.forecast(start.forecast, 90,
                      L.list=c(300, 350, 400, 450, 500, 600),
                      lodL.list=c(500, 550, 600, 650, 800, 1000),
                      dL.list=c(200, 250, 270, 300, 320, 350),
                      pn=30, lodn=30, dpn=5, years=20, dyears=20, steps=12, for.today=TRUE)

output.file.name.365 <- paste(prefix, "ssa/", start.forecast, "_ssa_spbu_365.txt", sep = "")
output.file.name.90 <- paste(prefix, "ssa/", start.forecast, "_ssa_spbu_90.txt", sep = "")

my.write(format(df.365, scientific = FALSE), file = output.file.name.365,
         header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD\t\t\tdX\t\t\tdY",
         row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)

my.write(format(df.90, scientific = FALSE), file = output.file.name.90,
         header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD\t\t\tdX\t\t\tdY",
         row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)

if(!mjd.given) {
  output.file.name.today.365 <- paste(prefix, "today/ssa_spbu_365.txt", sep="")
  output.file.name.today.90 <- paste(prefix, "today/ssa_spbu_90.txt", sep="")
  
  my.write(format(df.365, scientific = FALSE), file = output.file.name.today.365,
         header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD\t\t\tdX\t\t\tdY",
         row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
  
  my.write(format(df.90, scientific = FALSE), file = output.file.name.today.90,
         header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD\t\t\tdX\t\t\tdY",
         row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
  
  write.csv(data.frame(start.forecast=start.forecast), "today/start_forecast.csv")
}
