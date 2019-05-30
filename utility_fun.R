# Author: Ahad Alizadeh
# Last update: 2019-4-23
# Description: Some utility functions to improve coding
# Packages Used: R2wd

##########################
# Important common libraries
library(ggplot2); library(dplyr); library(wrapr)


##########################
# Read clipboard
read.cb <- function(header=TRUE,...) read.table("clipboard", header = header,
                                                sep ="\t",...)
##########################
# Write x to clipbord
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
# Create a table in word automatically using x dataframe.
wdTable<-function(x,..., filename=NULL, path = ""){
  R2wd::wdGet(filename,path , method="RDCOMClient")
  R2wd::wdBody("\n\n")
  R2wd::wdTable(as.data.frame(x), ...)
  cat("Done!\n")
}

#######################
# Checking internet connection.
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
# A pipe to set or replace a comment into a variable.
"%#%"<- function(a,b) {
  #"Replacing a new comment with old one or
  # adding another one by including '...' to the start of comment"
 sp= strsplit(b,split  ="")[[1]]
 if(all(sp[1:3]==".")){  
   sp=  paste0(sp[-(1:3)],collapse = "")
   comment(a)<-c(comment(a),sp)
   } else {comment(a)<-b}
  a}
  
  #################
  # Easy function to set progress bar.
  progress<- function(it, min = 1, max = 100, 
                    title = "Coding by Ahad Alizadeh", text = "Hello World!"){
                    if (it == min)
                        progression...... <<- winProgressBar(min = min,label="% done",
                                 max = max, width = 300)  
 
                        setWinProgressBar(progression......, it,title =
                                paste0(title, ", ", round((it+1)/(max)*100),"% done."),
                                label = text)
  
                     if(it == max)  close(progression......)

 }
  
  
  
  
