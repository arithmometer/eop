get_file <- function(filename, url) {
  tryCatch({download.file(url, destfile=filename)}, silent = TRUE, condition = function(err) { } )
}

last.filename <- "pul/last.csv"
last <- read.csv(last.filename)
newlast <- last

date.string <- format(Sys.time(), "%Y-%m-%d")
mjd <- as.integer(as.Date(date.string) - as.Date("1858-11-17"))

for(day in (last$x + 1):mjd) {
  am.filename <- paste(day, "_am_pul.txt", sep="")
  e1.filename <- paste(day, "_e1_pul.txt", sep="")
  
  get_file(paste("pul/", am.filename, sep=""), paste("http://www.gao.spb.ru/english/as/persac/eopcppp/", am.filename, sep=""))
  get_file(paste("pul/", e1.filename, sep=""), paste("http://www.gao.spb.ru/english/as/persac/eopcppp/", e1.filename, sep=""))
  
  if(file.exists(paste("pul/", am.filename, sep="")) || file.exists(paste("pul/", e1.filename, sep=""))) {
    newlast <- day
  }
}

write.csv(newlast, last.filename)
