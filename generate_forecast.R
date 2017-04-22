#!/usr/bin/env Rscript
library(methods)
suppressMessages(library(plotly))
suppressMessages(library(Rssa))

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

mjd.given <- FALSE
args = commandArgs(trailingOnly=TRUE)
if(length(args) > 0) {
  mjd.given <- TRUE
  start.forecast <- as.integer(args[1])
} else {
  date.string <- format(Sys.time(), "%Y-%m-%d")
  # put to start.forecast MJD of today
  start.forecast <- as.integer(as.Date(date.string) - as.Date("1858-11-17")) - 1
}

# prefix <- "/srv/shiny-server/eop/"
prefix <- "/home/grigory/data/R/eop/"

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

find.n <- function(start.forecast, coord, start, end, L, n, len, type, steps, for.today, forecast.type) {
  s <- c()
  x <- c()
  for(i in 0:(steps - 1)) {
    l <- start + i
    r <- end + i
    s[[i + 1]] <- ssa(c04[l:r, coord], L = L, neig = n)
    x[[i + 1]] <- c04[(r + 1):(r + len), coord]
  }
  
  dists <- c()
  for(num in 1:n) {
    r <- c()
    for(i in 0:(steps - 1)) {
      rf <- switch(forecast.type,
                   "r" = rforecast(s[[i + 1]], groups = list(1:num), len = len, only.new = TRUE),
                   "v" = vforecast(s[[i + 1]], groups = list(1:num), len = len, only.new = TRUE)
      )
      r <- c(r, MSE(rf, x[[i + 1]], len))
    }
    dists <- c(dists, ifelse(type == "mean", mean(r), median(r)))
    rm(r)
    gc()
  }
  rm(x, s)
  gc()
  
  write.csv(dists, paste(paste(prefix, forecast.type, "ssa/params/", start.forecast, "/", coord, sep=""), len, L, "dists.csv", sep="_"))
  if(for.today) {
    write.csv(dists, paste(paste(prefix, forecast.type, "today/", coord, sep=""), len, L, "dists.csv", sep="_"))
  }
  
  return(list(which.min(dists), min(dists)))
}

find.Ln <- function(start.forecast, coord, start, end, L.list, pn, len, type, steps, for.today, forecast.type) {
  best.L <- L.list[1]
  best.n <- 1
  mindist <- Inf
  for(L in L.list) {
    res <- find.n(start.forecast, coord, start, end, L, pn, len, "mean", steps, for.today, forecast.type)
    if(res[[2]] < mindist) {
      mindist <- res[[2]]
      best.n <- res[[1]]
      best.L <- L
    }
  }
  return(list(best.L, best.n))
}

get.forecast <- function(start.forecast, len, L.list, lodL.list, dL.list, pn, lodn, dpn, years, dyears, steps, for.today) {
  for(forecast.type in c("r", "v")) {
    dir.create(paste(prefix, forecast.type, "ssa/params/", start.forecast, "/", sep=""), showWarnings = FALSE)
    write.csv(L.list, paste(prefix, forecast.type, "ssa/params/", start.forecast, "/x_", len, "_L_list.csv", sep=""))
    write.csv(L.list, paste(prefix, forecast.type, "ssa/params/", start.forecast, "/y_", len, "_L_list.csv", sep=""))
    write.csv(lodL.list, paste(prefix, forecast.type, "ssa/params/", start.forecast, "/LOD_", len, "_L_list.csv", sep=""))
    write.csv(dL.list, paste(prefix, forecast.type, "ssa/params/", start.forecast, "/dX_", len, "_L_list.csv", sep=""))
    write.csv(dL.list, paste(prefix, forecast.type, "ssa/params/", start.forecast, "/dY_", len, "_L_list.csv", sep=""))
    if(for.today) {
      write.csv(L.list, paste(prefix, forecast.type, "today/x_", len, "_L_list.csv", sep=""))
      write.csv(L.list, paste(prefix, forecast.type, "today/y_", len, "_L_list.csv", sep=""))
      write.csv(lodL.list, paste(prefix, forecast.type, "today/LOD_", len, "_L_list.csv", sep=""))
      write.csv(dL.list, paste(prefix, forecast.type, "today/dX_", len, "_L_list.csv", sep=""))
      write.csv(dL.list, paste(prefix, forecast.type, "today/dY_", len, "_L_list.csv", sep=""))
    }
  }
  
  # index corresponding to the beginning of forecast
  ind <- start.forecast - 37664
  fin <- ind - 1
  
  period <- years * 365
  x <- c04[(fin - period + 1):fin, "x"]
  y <- c04[(fin - period + 1):fin, "y"]
  dperiod <- dyears * 365
  lod <- c04[(fin - dperiod):(fin - 1), "LOD"]
  dX <- c04[(fin - dperiod + 1):fin, "dX"]
  dY <- c04[(fin - dperiod + 1):fin, "dY"]
  
  end <- fin - steps - len
  start <- end - period + 1
  dstart <- end - dperiod + 1
  rxp <- find.Ln(start.forecast, "x", start, end, L.list, pn, len, "mean", steps, for.today, "r")
  ryp <- find.Ln(start.forecast, "y", start, end, L.list, pn, len, "mean", steps, for.today, "r")
  rlodp <- find.Ln(start.forecast, "LOD", dstart - 1, end - 1, lodL.list, lodn, len, "mean", steps, for.today, "r")
  rdxp <- find.Ln(start.forecast, "dX", dstart, end, dL.list, dpn, len, "mean", steps, for.today, "r")
  rdyp <- find.Ln(start.forecast, "dY", dstart, end, dL.list, dpn, len, "mean", steps, for.today, "r")
  
  rf.x <- rforecast(ssa(x, L = rxp[[1]], neig = rxp[[2]]), groups = list(1:rxp[[2]]), len = len, only.new = TRUE)
  rf.y <- rforecast(ssa(y, L = ryp[[1]], neig = ryp[[2]]), groups = list(1:ryp[[2]]), len = len, only.new = TRUE)
  rf.lod <- rforecast(ssa(lod, L = rlodp[[1]], neig = rlodp[[2]]), groups = list(c(1:rlodp[[2]])), len = len + 1, only.new = TRUE)
  rf.dx <- rforecast(ssa(dX, L = rdxp[[1]], neig = rdxp[[2]]), groups = list(c(1:rdxp[[2]])), len = len, only.new = TRUE)
  rf.dy <- rforecast(ssa(dY, L = rdyp[[1]], neig = rdyp[[2]]), groups = list(c(1:rdyp[[2]])), len = len, only.new = TRUE)
  
  rparams <- data.frame(x=c(rxp[[1]], rxp[[2]]),
                        y=c(ryp[[1]], ryp[[2]]),
                        LOD=c(rlodp[[1]], rlodp[[2]]),
                        dX=c(rdxp[[1]], rdxp[[2]]),
                        dY=c(rdyp[[1]], rdyp[[2]])
  )
  write.csv(rparams, paste(prefix, "rssa/params/", start.forecast, "/", len, "params.csv", sep=""))
  if(for.today) {
    write.csv(rparams, paste(prefix, "rtoday/", len, "params.csv", sep=""))
  }
  
  rdf <- data.frame(MJD = start.forecast:(start.forecast + len - 1), x = rf.x, y = rf.y, LOD = rf.lod[-1], dX = rf.dx, dY = rf.dy)
  output.file.name <- paste0(prefix, "rssa/", start.forecast, "_ssa_spbu_", len, ".txt")
  my.write(format(rdf, scientific = FALSE), file = output.file.name,
           header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD\t\t\tdX\t\t\tdY",
           row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
  
  output.file.name.today <- paste0(prefix, "rtoday/ssa_spbu_", len, ".txt")
  my.write(format(rdf, scientific = FALSE), file = output.file.name.today,
           header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD\t\t\tdX\t\t\tdY",
           row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
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
  
  write.csv(data.frame(start.forecast=start.forecast), paste(prefix, "rtoday/start_forecast.csv", sep=""))
}

get.forecast(start.forecast, 365,
             L.list=c(300, 400, 500, 600, 700, 800, 900, 1000),
             lodL.list=c(1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500),
             dL.list=c(250, 300, 350, 400, 450, 500),
             pn=30, lodn=30, dpn=5, years=20, dyears=20, steps=12, for.today=!mjd.given)

get.forecast(start.forecast, 90,
             L.list=c(300, 400, 500, 600, 700, 800, 900, 1000),
             lodL.list=c(1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500),
             dL.list=c(250, 300, 350, 400, 450, 500),
             pn=30, lodn=30, dpn=5, years=20, dyears=20, steps=12, for.today=!mjd.given)
