output.file.name.365 <- "today/ssa_spbu_365.txt"
output.file.name.90 <- "today/ssa_spbu_90.txt"

my.write(format(df.365, scientific = FALSE), file = output.file.name.365,
         header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD\t\t\tdX\t\t\tdY",
         row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)

my.write(format(df.90, scientific = FALSE), file = output.file.name.90,
         header = "# MJD\t\t\tx\t\t\ty\t\t\tLOD\t\t\tdX\t\t\tdY",
         row.names=FALSE, col.names = FALSE, sep = "\t", quote = FALSE)