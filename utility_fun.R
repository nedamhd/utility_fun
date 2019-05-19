# Author: Ahad Alizadeh
# Last update: 2019-4-23
# Description: Some utility functions to improve coding
# Packages Used: R2wd

##########################
# Important common libraries
library(ggplot2); library(dplyr); library(wrapr)


##########################
# `read.cb`: read clipboard
read.cb <- function(header=TRUE,...) read.table("clipboard", header = header,
                                                sep ="\t",...)
##########################
# `write.cb`: write x to clipbord
write.cb = function(x, row.names=TRUE, col.names=TRUE, comment=FALSE, text=NULL, ...){ 
datafile <- file("clipboard", open='wt')
on.exit(close(datafile))
if(comment == TRUE)   {
  if(is.null(comment(x))) warning("There is no comment for x! first add one by comment(x) = '...'") else
  writeLines(comment(x), con=datafile)}
write.table(x, file = datafile, sep = "\t", row.names = row.names,
              col.names = col.names, ...)
if(!is.null(text))   {writeLines(text , con=datafile)}
}

#########################
# `wd.Table`: create a table in word automatically using x dataframe.
wdTable<-function(x,..., filename=NULL, path = ""){
  R2wd::wdGet(filename,path , method="RDCOMClient")
  R2wd::wdBody("\n\n")
  R2wd::wdTable(as.data.frame(x), ...)
  cat("Done!\n")
}

#######################
#`ping.IP`: checking internet connection.
ping.IP <- function() {
  if (.Platform$OS.type == "windows") {
    cat("Please wait...")
    ipmessage <- system("ping www.google.com", intern = TRUE)
  } 
  
  l1=strsplit(ipmessage[3],":")[[1]][2]
  l2=strsplit(ipmessage[4],":")[[1]][2]
  l3=strsplit(ipmessage[5],":")[[1]][2]
  l4=strsplit(ipmessage[6],":")[[1]][2]
  
  result<-  !all(c(l1,l2,l3,l4)==" Destination net unreachable.")
  cat("\nInternet connection: ",result[1])
 invisible(result)
}


  
##################
# `%#%` Pipe to add a comment to a variable (`%#%`).
"%#%"<- function(a,b) {comment(a)=b; a}
  
  
  
  
