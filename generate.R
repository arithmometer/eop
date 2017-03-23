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

df.365 <- get.forecast(start.forecast, 365,
                       L.list=c(400, 500, 550, 600, 650),
                       lodL.list=c(2700, 2750, 2800, 2850, 3000),
                       dL.list=c(250, 270, 300, 320),
                       pn=30, lodn=30, dpn=15, years=20, dyears=20, steps=5)
df.90 <- get.forecast(start.forecast, 90,
                      L.list=c(300, 350, 400, 450, 500),
                      lodL.list=c(500, 550, 600, 650, 800),
                      dL.list=c(200, 250, 270, 300, 320),
                      pn=30, lodn=30, dpn=15, years=20, dyears=20, steps=12)

output.file.name.365 <- paste("ssa/", start.forecast, "_ssa_spbu_365.txt", sep = "")
output.file.name.90 <- paste("ssa/", start.forecast, "_ssa_spbu_90.txt", sep = "")

my.write(format(df.365, scientific = FALSE), file = output.file.name.365,
         header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD\t\t\tdX\t\t\tdY",
         row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)

my.write(format(df.90, scientific = FALSE), file = output.file.name.90,
         header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD\t\t\tdX\t\t\tdY",
         row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)