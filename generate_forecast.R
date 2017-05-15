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

find.r <- function(eop, start, base.len, n.steps, step.len, L, n, len, forecast.type, for.today) {
  s <- c()
  for(i in 0:(n.steps - 1)) {
    l <- start + i * step.len - base.len
    r <- start + i * step.len - 1
    s[[i + 1]] <- ssa(c04[l:r, eop], L = L, neig = n)
  }
  
  dists <- c()
  for(num in 1:n) {
    r <- c()
    for(i in 0:(n.steps - 1)) {
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

find.Ln <- function(start.forecast, eop, base.len.years, valid.len.years, n.steps, L.list, pn, len, forecast.type, for.today) {
  ind <- start.forecast - 37664
  
  base.len <- base.len.years * 365
  valid.len <- valid.len.years * 365
  step.len <- (valid.len - len) %/% (n.steps - 1)
  start <- ind - (n.steps - 1) * step.len - len
  
  if(eop == "LOD") {
    start <- start - 1
  }
  
  best.L <- L.list[1]
  best.r <- 1
  mindist <- Inf
  for(L in L.list) {
    if(L >= base.len - 50) {
      break
    }
    res <- find.r(eop, start, base.len, n.steps, step.len, L, pn, len, forecast.type, for.today)
    if(res[[2]] < mindist) {
      mindist <- res[[2]]
      best.r <- res[[1]]
      best.L <- L
    }
  }
  return(list(best.L, best.r))
}

get.forecast <- function(start.forecast, len,
                       L.x, L.y, L.lod, L.dx, L.dy,
                       n.x, n.y, n.lod, n.dx, n.dy,
                       base.len.years.x, base.len.years.y, base.len.years.lod, base.len.years.dx, base.len.years.dy,
                       valid.len.years.x, valid.len.years.y, valid.len.years.lod, valid.len.years.dx, valid.len.years.dy,
                       n.steps.x, n.steps.y, n.steps.lod, n.steps.dx, n.steps.dy,
                       forecast.type, for.today) {

  dir.create(paste0(prefix, forecast.type, "ssa/params/", start.forecast), showWarnings = FALSE, recursive = TRUE)

  write.csv(L.x,   paste0(prefix, forecast.type, "ssa/params/", start.forecast, "/x_",   len, "_L_list.csv"))
  write.csv(L.y,   paste0(prefix, forecast.type, "ssa/params/", start.forecast, "/y_",   len, "_L_list.csv"))
  write.csv(L.lod, paste0(prefix, forecast.type, "ssa/params/", start.forecast, "/LOD_", len, "_L_list.csv"))
  write.csv(L.dx,  paste0(prefix, forecast.type, "ssa/params/", start.forecast, "/dX_",  len, "_L_list.csv"))
  write.csv(L.dy,  paste0(prefix, forecast.type, "ssa/params/", start.forecast, "/dY_",  len, "_L_list.csv"))
  if(for.today) {
    write.csv(L.x,   paste0(prefix, forecast.type, "today/x_",   len, "_L_list.csv"))
    write.csv(L.y,   paste0(prefix, forecast.type, "today/y_",   len, "_L_list.csv"))
    write.csv(L.lod, paste0(prefix, forecast.type, "today/LOD_", len, "_L_list.csv"))
    write.csv(L.dx,  paste0(prefix, forecast.type, "today/dX_",  len, "_L_list.csv"))
    write.csv(L.dy,  paste0(prefix, forecast.type, "today/dY_",  len, "_L_list.csv"))
  }
 
  p.x   <- find.Ln(start.forecast, "x",   base.len.years.x,   valid.len.years.x,   n.steps.x,   L.x,   n.x,   len, forecast.type, for.today)
  p.y   <- find.Ln(start.forecast, "y",   base.len.years.y,   valid.len.years.y,   n.steps.y,   L.y,   n.y,   len, forecast.type, for.today)
  p.lod <- find.Ln(start.forecast, "LOD", base.len.years.lod, valid.len.years.lod, n.steps.lod, L.lod, n.lod, len, forecast.type, for.today)
  p.dx  <- find.Ln(start.forecast, "dX",  base.len.years.dx,  valid.len.years.dx,  n.steps.dx,  L.dx,  n.dx,  len, forecast.type, for.today)
  p.dy  <- find.Ln(start.forecast, "dY",  base.len.years.dy,  valid.len.years.dy,  n.steps.dy,  L.dy,  n.dy,  len, forecast.type, for.today)
  
  fin <- start.forecast - 37664 - 1
  period.x   <- base.len.years.x   * 365
  period.y   <- base.len.years.y   * 365
  period.lod <- base.len.years.lod * 365
  period.dx  <- base.len.years.dx  * 365
  period.dy  <- base.len.years.dy  * 365

  x   <- c04[(fin - period.x + 1):fin, "x"]
  y   <- c04[(fin - period.y + 1):fin, "y"]
  lod <- c04[(fin - period.lod):(fin - 1), "LOD"]
  dX  <- c04[(fin - period.dx + 1):fin, "dX"]
  dY  <- c04[(fin - period.dy + 1):fin, "dY"]

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
  write.csv(params, paste0(prefix, forecast.type, "ssa/params/", start.forecast, "/", len, "_params.csv"))
  
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

last.mjd.c04 <- c04[nrow(c04), "MJD"]

if(start.forecast > last.mjd.c04) {
  finals2000 <- read.csv(paste(prefix, "csv/", start.forecast - 1, "/finals2000A.daily.csv", sep=""), sep=";")
  
  mjd.from <- last.mjd.c04 + 1
  mjd.to   <- start.forecast - 1
  ind.from <- which(finals2000["MJD"] == mjd.from)
  ind.to   <- which(finals2000["MJD"] == mjd.to)
  
  gap.df <- data.frame("MJD"=mjd.from:mjd.to,
                       "x"  =finals2000[ind.from:ind.to, "x_pole"],
                       "y"  =finals2000[ind.from:ind.to, "y_pole"],
                       "LOD"=finals2000[ind.from:ind.to, "LOD"] / 1000,
                       "dX" =finals2000[ind.from:ind.to, "dX"] / 1000,
                       "dY" =finals2000[ind.from:ind.to, "dY"] / 1000)
  
  c04 <- rbind(c04[, c("MJD", "x", "y", "LOD", "dX", "dY")], gap.df)
}

if(!mjd.given) {
  write.csv(data.frame(start.forecast=start.forecast), paste0(prefix, "rtoday/start_forecast.csv"))
}

L.xy   <- c( 300,  500,  700,  900, 1100, 1300, 1500, 1700, 1900, 2100)
L.lod  <- c( 300,  600,  900, 1200, 1500, 1800, 2100, 2400, 2700, 3000)
L.dxdy <- c( 250,  300,  350,  400,  450,  500)

get.forecast(start.forecast, 365,
             L.x=L.xy, L.y=L.xy, L.lod=L.lod, L.dx=L.dxdy, L.dy=L.dxdy,
             n.x=30, n.y=30, n.lod=30, n.dx=5, n.dy=5,
             base.len.years.x=15, base.len.years.y=15, base.len.years.lod=15, base.len.years.dx=15, base.len.years.dy=15,
             valid.len.years.x=7, valid.len.years.y=7, valid.len.years.lod=10, valid.len.years.dx=7, valid.len.years.dy=7,
             n.steps.x=10, n.steps.y=10, n.steps.lod=10, n.steps.dx=10, n.steps.dy=10,
             forecast.type="r", for.today=!mjd.given)
