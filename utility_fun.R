
##########################
# read.cb
read.cb <- function(header=TRUE,...) read.table("clipboard", header = header,
                                                sep ="\t",...)
##########################
# write.cb
write.cb <- function(x, row.names = FALSE, col.names = TRUE,...)
  write.table(x, file="clipboard", sep = "\t", row.names = row.names,
              col.names = col.names,...)

#########################
# wdTable
wdTable<-function(x,..., filename=NULL, path = ""){
  R2wd::wdGet(filename,path , method="RDCOMClient")
  R2wd::wdBody("\n\n")
  R2wd::wdTable(as.data.frame(x), ...)
  cat("Done!\n")
}


