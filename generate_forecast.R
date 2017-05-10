#!/usr/bin/env Rscript
library(methods)
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

prefix <- "/srv/shiny-server/eop/"
# prefix <- "/home/grigory/data/R/eop/"

destfile <- paste0(prefix, "eopc04_IAU2000.62-now.txt")
# url <- "https://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now"
# download.file(url, destfile, method = "auto", quiet=TRUE)
# if(!file.exists(destfile)) {
#   stop("c04 file does not exist", call.=FALSE)
# }

# global C04
c04 <- read.table(destfile, comment.char = "#", skip = 14)
colnames(c04) <- c("Year", "Month", "Day", "MJD", "x", "y", "UT1-UTC", "LOD",
                   "dX", "dY", "x Err", "y Err", "UT1-UTC err", "LOD err", "dX err", "dY err")

MSE <- function(a, b, n) {
  sum((a - b)**2) / n
}

find.r <- function(start.forecast, eop, start, period, steps, step.len, L, n, len, forecast.type) {
  s <- c()
  for(i in 0:(steps - 1)) {
    l <- start + i * step.len - period
    r <- start + i * step.len - 1
    s[[i + 1]] <- ssa(c04[l:r, eop], L = L, neig = n)
  }
  
  dists <- c()
  for(num in 1:n) {
    r <- c()
    for(i in 0:(steps - 1)) {
      f <- switch(forecast.type,
                  "r" = rforecast(s[[i + 1]], groups = list(1:num), len = len, only.new = TRUE),
                  "v" = vforecast(s[[i + 1]], groups = list(1:num), len = len, only.new = TRUE)
      )
      left <- start + i * step.len
      right <- left + len - 1
      r <- c(r, MSE(f, c04[left:right, eop], len))
    }
    dists <- c(dists, mean(r))
    rm(r)
    gc()
  }
  rm(s)
  gc()

  write.csv(dists, paste(paste0(prefix, forecast.type, "ssa/params/", start.forecast, "/", eop), len, L, "dists.csv", sep="_"))
  if(for.today) {
    write.csv(dists, paste(paste0(prefix, forecast.type, "today/", eop), len, L, "dists.csv", sep="_"))
  }
  
  return(list(which.min(dists), min(dists)))
}

find.Ln <- function(start.forecast, eop, base.len.years, step.len, valid.len.years, L.list, pn, len, forecast.type) {
  ind <- start.forecast - 37664
  fin <- ind - 1
  
  period <- base.len.years * 365
  
  steps <- valid.len.years * 365 %/% step.len - len %/% step.len + 1
  start <- fin - steps * step.len + 1
  if(eop == "LOD") {
    start <- start - 1
  }
  
  best.L <- L.list[1]
  best.r <- 1
  mindist <- Inf
  for(L in L.list) {
    if(L >= period - 50) {
      break
    }
    res <- find.r(start.forecast, eop, start, period, steps, step.len, L, pn, len, forecast.type)
    if(res[[2]] < mindist) {
      mindist <- res[[2]]
      best.r <- res[[1]]
      best.L <- L
    }
  }
  return(list(best.L, best.r))
}

get.forecast <- function(start.forecast, len,
                         L.xy, L.lod, L.dxdy, 
                         n.xy, n.lod, n.dxdy,
                         step.len.xy, step.len.lod, step.len.dxdy,
                         base.len.years.xy, base.len.years.lod, base.len.years.dxdy,
                         valid.len.years.xy, valid.len.years.lod, valid.len.years.dxdy,
                         forecast.type, for.today) {
  dir.create(paste(prefix, forecast.type, "ssa/params/", start.forecast, "/", sep=""), showWarnings = FALSE)
  write.csv(L.xy,   paste0(prefix, forecast.type, "ssa/params/", start.forecast, "/x_",   len, "_L_list.csv"))
  write.csv(L.xy,   paste0(prefix, forecast.type, "ssa/params/", start.forecast, "/y_",   len, "_L_list.csv"))
  write.csv(L.lod,  paste0(prefix, forecast.type, "ssa/params/", start.forecast, "/LOD_", len, "_L_list.csv"))
  write.csv(L.dxdy, paste0(prefix, forecast.type, "ssa/params/", start.forecast, "/dX_",  len, "_L_list.csv"))
  write.csv(L.dxdy, paste0(prefix, forecast.type, "ssa/params/", start.forecast, "/dY_",  len, "_L_list.csv"))
  if(for.today) {
    write.csv(L.xy,   paste0(prefix, forecast.type, "today/x_",   len, "_L_list.csv"))
    write.csv(L.xy,   paste0(prefix, forecast.type, "today/y_",   len, "_L_list.csv"))
    write.csv(L.lod,  paste0(prefix, forecast.type, "today/LOD_", len, "_L_list.csv"))
    write.csv(L.dxdy, paste0(prefix, forecast.type, "today/dX_",  len, "_L_list.csv"))
    write.csv(L.dxdy, paste0(prefix, forecast.type, "today/dY_",  len, "_L_list.csv"))
  }

  p.x   <- find.Ln(start.forecast, "x",   base.len.years.xy,   step.len.xy,   valid.len.years.xy,   L.xy,   n.xy,   len, forecast.type)
  p.y   <- find.Ln(start.forecast, "y",   base.len.years.xy,   step.len.xy,   valid.len.years.xy,   L.xy,   n.xy,   len, forecast.type)
  p.lod <- find.Ln(start.forecast, "LOD", base.len.years.lod,  step.len.lod,  valid.len.years.lod,  L.lod,  n.lod,  len, forecast.type)
  p.dx  <- find.Ln(start.forecast, "dX",  base.len.years.dxdy, step.len.dxdy, valid.len.years.dxdy, L.dxdy, n.dxdy, len, forecast.type)
  p.dy  <- find.Ln(start.forecast, "dY",  base.len.years.dxdy, step.len.dxdy, valid.len.years.dxdy, L.dxdy, n.dxdy, len, forecast.type)
  
  fin <- start.forecast - 37664 - 1
  period.xy   <- base.len.years.xy   * 365
  perid.lod   <- base.len.years.lod  * 365
  period.dxdy <- base.len.years.dxdy * 365
  
  x   <- c04[(fin - period.xy + 1):fin, "x"]
  y   <- c04[(fin - period.xy + 1):fin, "y"]
  lod <- c04[(fin - perid.lod):(fin - 1), "LOD"]
  dX  <- c04[(fin - period.dxdy + 1):fin, "dX"]
  dY  <- c04[(fin - period.dxdy + 1):fin, "dY"]
  
  f.x   <- rforecast(ssa(x,   L = p.x[[1]],   neig = p.x[[2]]),   groups = list(1:p.x[[2]]),   len = len,     only.new = TRUE)
  f.y   <- rforecast(ssa(y,   L = p.y[[1]],   neig = p.y[[2]]),   groups = list(1:p.y[[2]]),   len = len,     only.new = TRUE)
  f.lod <- rforecast(ssa(lod, L = p.lod[[1]], neig = p.lod[[2]]), groups = list(1:p.lod[[2]]), len = len + 1, only.new = TRUE)
  f.dx  <- rforecast(ssa(dX,  L = p.dx[[1]],  neig = p.dx[[2]]),  groups = list(1:p.dx[[2]]),  len = len,     only.new = TRUE)
  f.dy  <- rforecast(ssa(dY,  L = p.dy[[1]],  neig = p.dy[[2]]),  groups = list(1:p.dy[[2]]),  len = len,     only.new = TRUE)
  
  params <- data.frame(x  =c(p.x[[1]],   p.x[[2]]),
                       y  =c(p.y[[1]],   p.y[[2]]),
                       LOD=c(p.lod[[1]], p.lod[[2]]),
                       dX =c(p.dx[[1]],  p.dx[[2]]),
                       dY =c(p.dy[[1]],  p.dy[[2]])
  )
  write.csv(params, paste0(prefix, forecast.type, "ssa/params/", start.forecast, "_params.csv"))
  
  df <- data.frame(MJD = start.forecast:(start.forecast + len - 1), x = f.x, y = f.y, LOD = f.lod[-1], dX = f.dx, dY = f.dy)
  
  output.file.name <- paste0(prefix, "rssa/", start.forecast, "_ssa_spbu_", len, ".txt")
  
  my.write(format(df, scientific = FALSE), file = output.file.name,
           header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD\t\t\tdX\t\t\tdY",
           row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
  
  if(for.today) {
    output.file.name.today <- paste0(prefix, "rtoday/ssa_spbu_", len, ".txt")
    my.write(format(rdf, scientific = FALSE), file = output.file.name.today,
             header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD\t\t\tdX\t\t\tdY",
             row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
  }
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

L.xy   <- c( 300,  500,  700,  900, 1100, 1300, 1500, 1700, 1900, 2100)
L.lod  <- c( 300,  600,  900, 1200, 1500, 1800, 2100, 2400, 2700, 3000)
L.dxdy <- c( 250,  300,  350,  400,  450,  500)

get.forecast(start.forecast, 365,
             L.xy=L.xy, L.lod=L.lod, L.dxdy=L.dxdy,
             n.xy=30, n.lod=30, n.dxdy=5, 
             step.len.xy=365, step.len.lod=182, step.len.dxdy=112,
             base.len.years.xy=20, base.len.years.lod=10, base.len.years.dxdy=20,
             valid.len.years.xy=15, valid.len.years.lod=5, valid.len.years.dxdy=3,
             forecast.type="r", for.today=!mjd.given)
