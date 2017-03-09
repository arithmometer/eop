get_file <- function(filename, url) {
  if(!file.exists(filename)) {
    download.file(url, filename, method = "auto", quiet=TRUE)
    if(!file.exists(filename)) {
      stop(paste(filename, " file does not exist"), call.=FALSE)
    }
  }
}

date.string <- format(Sys.time(), "%Y-%m-%d")
mjd <- as.integer(as.Date(date.string) - as.Date("1858-11-17"))

am.filename <- paste(mjd - 1, "_am_pul.txt", sep="")
e1.filename <- paste(mjd - 1, "_e1_pul.txt", sep="")

get_file(paste("pul/", am.filename, sep=""), paste("http://www.gao.spb.ru/english/as/persac/eopcppp/", am.filename, sep=""))
get_file(paste("pul/", e1.filename, sep=""), paste("http://www.gao.spb.ru/english/as/persac/eopcppp/", e1.filename, sep=""))
