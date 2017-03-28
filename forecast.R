suppressMessages(library(Rssa))

url <- "https://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now"
destfile <- "eopc04_IAU2000.62-now.txt"
download.file(url, destfile, method = "auto", quiet=TRUE)
if(!file.exists("eopc04_IAU2000.62-now.txt")) {
  stop("c04 file does not exist", call.=FALSE)
}

c04 <- read.table("eopc04_IAU2000.62-now.txt", comment.char = "#", skip = 14)
colnames(c04) <- c("Year", "Month", "Day", "MJD", "x", "y", "UT1-UTC", "LOD",
                   "dX", "dY", "x Err", "y Err", "UT1-UTC err", "LOD err", "dX err", "dY err")

MSE <- function(a, b, n) {
  sum((a - b)**2) / n
}

find.n <- function(start.forecast, coord, start, end, L, n, len, type, steps) {
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
  
  write.csv(dists, paste(paste("ssa/params/", start.forecast, "/", coord, sep=""), len, L, "dists.csv", sep="_"))
  
  return(list(which.min(dists), min(dists)))
}

find.Ln <- function(start.forecast, coord, start, end, L.list, pn, len, type, steps) {
  best.L <- L.list[1]
  best.n <- 1
  mindist <- Inf
  for(L in L.list) {
    res <- find.n(start.forecast, coord, start, end, L, pn, len, "mean", steps)
    print(res)
    if(res[[2]] < mindist) {
      mindist <- res[[2]]
      best.n <- res[[1]]
      best.L <- L
    }
  }
  return(list(best.L, best.n))
}

get.forecast <- function(start.forecast, len, L.list, lodL.list, dL.list, pn, lodn, dpn, years, dyears, steps) {
  dir.create(paste("ssa/params/", start.forecast, "/", sep=""))
  write.csv(L.list, paste("ssa/params/", start.forecast, "/x_L_list.csv", sep=""))
  write.csv(L.list, paste("ssa/params/", start.forecast, "/y_L_list.csv", sep=""))
  write.csv(lodL.list, paste("ssa/params/", start.forecast, "/LOD_L_list.csv", sep=""))
  write.csv(dL.list, paste("ssa/params/", start.forecast, "/dX_L_list.csv", sep=""))
  write.csv(dL.list, paste("ssa/params/", start.forecast, "/dY_L_list.csv", sep=""))
  
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
  
  end <- fin - steps * len
  start <- end - period + 1
  dstart <- end - dperiod + 1
  xp <- find.Ln(start.forecast, "x", start, end, L.list, pn, len, "mean", steps)
  yp <- find.Ln(start.forecast, "y", start, end, L.list, pn, len, "mean", steps)
  lodp <- find.Ln(start.forecast, "LOD", start, end, lodL.list, lodn, len, "mean", steps)
  dxp <- find.Ln(start.forecast, "dX", dstart, end, dL.list, dpn, len, "mean", steps)
  dyp <- find.Ln(start.forecast, "dY", dstart, end, dL.list, dpn, len, "mean", steps)
  
  rf.x <- rforecast(ssa(x, L = xp[[1]], neig = xp[[2]]), groups = list(1:xp[[2]]), len = len, only.new = TRUE)
  rf.y <- rforecast(ssa(y, L = yp[[1]], neig = yp[[2]]), groups = list(1:yp[[2]]), len = len, only.new = TRUE)
  rf.lod <- rforecast(ssa(lod, L = lodp[[1]], neig = lodp[[2]]), groups = list(c(1:lodp[[2]])), len = len, only.new = TRUE)
  rf.dx <- rforecast(ssa(dX, L = dxp[[1]], neig = dxp[[2]]), groups = list(c(1:dxp[[2]])), len = len, only.new = TRUE)
  rf.dy <- rforecast(ssa(dY, L = dyp[[1]], neig = dyp[[2]]), groups = list(c(1:dyp[[2]])), len = len, only.new = TRUE)
  
  write.csv(data.frame(x=c(xp[[1]], xp[[2]]),
                       y=c(yp[[1]], yp[[2]]),
                       LOD=c(lodp[[1]], lodp[[2]]),
                       dX=c(dxp[[1]], dxp[[2]]),
                       dY=c(dyp[[1]], dyp[[2]])
                       ), paste("ssa/params/", start.forecast, "/", len, "params.csv", sep=""))
  
  df <- data.frame(MJD = start.forecast:(start.forecast + len - 1), x = rf.x, y = rf.y, LOD = rf.lod, dX = rf.dx, dY = rf.dy)
}
