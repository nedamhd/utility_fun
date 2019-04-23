# Author: Ahad Alizadeh
# Last update: 2019-4-23
# Description: Some utility functions to improve coding
# Packages Used: R2wd

##########################
# read.cb: read clipboard
read.cb <- function(header=TRUE,...) read.table("clipboard", header = header,
                                                sep ="\t",...)
##########################
# write.cb: write x to clipbord
write.cb <- function(x, row.names = FALSE, col.names = TRUE,...)
  write.table(x, file="clipboard", sep = "\t", row.names = row.names,
              col.names = col.names,...)

#########################
# wdTable: create a table in word automatically using x dataframe.
wdTable <-function(x,..., filename=NULL, path = ""){
  R2wd::wdGet(filename,path , method="RDCOMClient")
  R2wd::wdBody("\n\n")
  R2wd::wdTable(as.data.frame(x), ...)
  cat("Done!\n")
}

