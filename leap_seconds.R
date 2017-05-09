leap <- data.frame(leap.seconds = c(
  "2006-01-01", # 1 Jan 2006
  "2009-01-01", # 1 Jan 2009
  "2012-07-01", # 1 Jul 2012
  "2015-07-01", # 1 Jul 2015
  "2017-01-01"  # 1 Jan 2017
))
write.csv(leap, "~/data/R/eop/csv/leap_seconds.csv")
