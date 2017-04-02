get_file <- function(filename, url) {
  tryCatch({download.file(url, destfile=filename)}, silent = TRUE, condition = function(err) { } )
}

last.filename <- "pul/last.csv"
last <- read.csv(last.filename)
newlast <- last

date.string <- format(Sys.time(), "%Y-%m-%d")
mjd <- as.integer(as.Date(date.string) - as.Date("1858-11-17"))

for(day in (last$x + 1):mjd) {
  am.name <- paste(day, "_am_pul.txt", sep="")
  e1.name <- paste(day, "_e1_pul.txt", sep="")
  
  am.filename <- paste("pul/", am.name, sep="")
  e1.filename <- paste("pul/", e1.name, sep="")
  
  get_file(am.filename, paste("http://www.gao.spb.ru/english/as/persac/eopcppp/", am.name, sep=""))
  get_file(e1.filename, paste("http://www.gao.spb.ru/english/as/persac/eopcppp/", e1.name, sep=""))
  
  if(file.info(am.filename)$size > 0 || file.info(e1.filename)$size > 0) {
    newlast <- day
  }
}

write.csv(newlast, last.filename)
