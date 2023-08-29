# Author: Ahad Alizadeh
# Last update: 2019-4-23
# Description: Some utility functions to improve coding
 


##########################
#Check and install requested R packages
librares<- c("ggplot2","dplyr","R2wd")
for(i in librares)
  if(i %in% rownames(installed.packages()) == FALSE) install.packages(i)
rm(librares)

##########################
# Important common libraries
library(ggplot2); library(dplyr); 


##########################
# Read clipboard
read.cb <- function(header=TRUE,...) read.table("clipboard", header = header,
                                                sep ="\t",...)
##########################
# Write x to clipbord
write.cb =
  function(x, row.names=TRUE, col.names=TRUE, comment=FALSE, text=NULL, ...){ 
    datafile <- file(paste0("clipboard-", object.size(x)), open='wt')
    on.exit(close(datafile))
    if(comment == TRUE)   {
      if(is.null(comment(x))) warning("There is no comment for x! first add one by comment(x) = '...'") else
        writeLines(comment(x), con=datafile)}
    write.table(x, file = paste0("clipboard-", object.size(x)), sep = "\t", row.names = row.names,
                col.names = col.names, ...)
    if(!is.null(text))   {writeLines(text , con=datafile)}
  }

#########################
# Create a table in word automatically using x dataframe.
wd.Table=
function(x,..., filename=NULL, path = ""){
  if("RDCOMClient" %in% rownames(installed.packages()) == FALSE)  { 
    # Sys.setenv("TAR" = "internal") # if you need it.
    # devtools::install_github("omegahat/RDCOMClient")
  devtools::install_github('omegahat/RDCOMClient')
  }
  R2wd::wdGet(filename,path , method="RDCOMClient")
  R2wd::wdBody("\n\n")
  R2wd::wdTable(as.data.frame(x), ...)
  cat("Done!\n")
}

#######################
# Checking internet connection in R.
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
  
 ##################
# Estimate error bars based on CI,SD, and CI.
 errorbar <- function(x,
                     type = c("CI", "SD", "SE"),
                     alpha = 0.05) {
  if (!is.numeric(x))
    stop("x must be numeric!")
  m.call <- names(match.call())
  if (!"type" %in% m.call)
    type = "CI"
  
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  n <- sum(!is.na(x))
  se <- s / sqrt(n)
  
  if (type == "CI" | type == "ci") {
    l <- m - qnorm(1 - alpha / 2) * se
    u <- m + qnorm(1 - alpha / 2) * se
    type = "CI"
  } else if (type == "SD" | type == "sd") {
    l <- m - s
    u <- m + s
    type = "SD"
  } else if (type == "SE" | type == "se") {
    l <- m - se
    u <- m + se
    type = "SE"
  } else
    stop("type must be one of \"CI\",\"SD\", or\"SE\"!")
  
  list(lower = l,
       upper = u,
       type = type)
}
  
 
 
 #######################
 # Bayesian and classic Confidence interval
 CI = function(M0,round=3, family="binomial"){
  # M0: glm or arm::bayesglm object.
  if(family=="gaussian"){
    cat("family: ", family, "\n")
  Re2<- data.frame()
  if( class(M0)[1]== "bayesglm" ){
    cat("Bayesian method by bayesglm\n\n")
    s.M0=summary(M0)
    M0.s=coef(arm::sim(M0,n=10000))
    CI<- apply(M0.s,2,quantile,
               c(0.025,0.975))
    mean.sim <- apply(M0.s,2,mean)
    Re=data.frame (OR= round(  (mean.sim),round),round(t(  (CI)),round),P.value=round(s.M0$ coefficients[,4],round+1))
    
    
    Re2= data.frame("Beta(95% CI)"= ( paste0( Re[,1],"(", Re[,2],",", Re[,3], ")")), "P value"= Re[,4])
    row.names(Re2)=row.names(Re)
  }
  
  
  if( class(M0)[1] %in% c("glm" , "svyglm")){
    cat("Classic method by glm\n")
    s.M0=summary(M0)
    CI<-  (confint(M0))
    mean.sim <- s.M0$coefficients[,1]
    Re=data.frame (OR= round(  (mean.sim),round),round((  (CI)),round),P.value=round(s.M0$ coefficients[,4],round+1))
    
    
    Re2= data.frame("Beta(95% CI)"= (paste0( Re[,1],"(", Re[,2],",", Re[,3], ")")), "P value"= Re[,4])
    row.names(Re2)=row.names(Re)
  }
  }
  
 ################################# 
  if(family=="binomial"){
    cat("family: ", family, "\n")
    
    Re2<- data.frame()
    if( class(M0)[1]== "bayesglm" ){
      cat("Bayesian method by bayesglm\n\n")
      s.M0=summary(M0)
      M0.s=coef(arm::sim(M0,n=10000))
      CI<- apply(M0.s,2,quantile,
                 c(0.025,0.975))
      mean.sim <- apply(M0.s,2,mean)
      Re=data.frame (OR= round( exp(mean.sim),round),round(t( exp(CI)),round),P.value=round(s.M0$ coefficients[,4],round+1))
      
      
      Re2= data.frame("OR(95% CI)"= ( paste0( Re[,1],"(", Re[,2],",", Re[,3], ")")), "P value"= Re[,4])
      row.names(Re2)=row.names(Re)
    }
    
    
    if( class(M0)[1] %in% c("glm",  "svyglm") ){
      cat("Classic method by glm\n\n")
      s.M0=summary(M0)
      CI<-  (confint(M0))
      mean.sim <- s.M0$coefficients[,1]
      Re=data.frame (OR= round( exp(mean.sim),round),round(( exp(CI)),round),P.value=round(s.M0$ coefficients[,4],round+1))
      
      
      Re2= data.frame("OR(95% CI)"= (paste0( Re[,1],"(", Re[,2],",", Re[,3], ")")), "P value"= Re[,4])
      row.names(Re2)=row.names(Re)
    }
  } 
  
  
  
  list(type1.result=Re,type2.result=Re2)
}

############################
 # my pipes 
 "%f%" <- function(a, b) {
  # a = "3.anyletter" for replace 0.000 with <0.001
  if(is.matrix(b)) {d = dim(b); m =1}
  if(is.vector(b)) {d = length(b); m =0}
  b =as.vector(b)
  res = c()
  norm = strsplit(as.character(a), split ="[.]")[[1]]
  a = as.numeric(norm[1])
  P = norm[2]
  for(i in 1:length(b)){
    res[i]=  paste0(sprintf(paste0("%.",a,"f"), b[i]) ) 
  if(!is.na(P)) 
    if( res[i] == "0.000") res[i] = "<0.001"
  }
  if(m==1) res=  matrix(res, nrow =d[1] ,ncol =d[2] )  
  return(res) 
}

"%+%" <- function(a, b) {
  if(!(is.atomic(a) | is.atomic(b)| is.matrix(a) | is.matrix(b)))
    stop("a and b must be matrix or atomic")
  if(is.atomic(a) & is.atomic(b)){
    z = paste0(a, b)
  } 
  if(is.matrix(a) & is.matrix(b)){
    if( all (dim(a) == dim(b))){
      z = paste0(a, b)
    dim(z) = dim(a)}
  } 
  if((is.matrix(a) & is.atomic(b))){
    if(length(b) == 1){
      z = paste0(a, b)
    dim(z) = dim(a)}
  } 
  if((is.matrix(b) & is.atomic(a))){
    if(length(a) == 1){
      z = paste0(a, b)
    dim(z) = dim(b)}
  } 
  z}

"%num%" <- function(a=NULL,b=NULL) {
  # Change charecter to numeric. a or b can be null. 
  if(!is.null(a))   {aname <- deparse(substitute(a))
  m.temp.12544521 <<- as.numeric(as.character(a)) 
  te = paste0(aname ,"<- m.temp.12544521")
  temp = parse(text = te)
  eval( temp, envir = globalenv())
  rm(m.temp.12544521, envir = globalenv())}
  
  if(!is.null(b))    {bname <- deparse(substitute(b))
  m.temp.12544522 <<- as.numeric(as.character(b)) 
  te = paste0(bname ,"<- m.temp.12544522")
  temp = parse(text = te)
  eval( temp, envir = globalenv())
  rm(m.temp.12544522, envir = globalenv())}
}


#############################
"%fac%" <- function(a=NULL,b=NULL) {
  # Change charecter to numeric. a or b can be null. 
  if(!is.null(a))   {aname <- deparse(substitute(a))
  m.temp.12544521 <<- as.factor(a)
  te = paste0(aname ,"<- m.temp.12544521")
  temp = parse(text = te)
  eval( temp, envir = globalenv())
  rm(m.temp.12544521, envir = globalenv())}
  
  if(!is.null(b))    {bname <- deparse(substitute(b))
  m.temp.12544522 <<- as.factor(b)
  te = paste0(bname ,"<- m.temp.12544522")
  temp = parse(text = te)
  eval( temp, envir = globalenv())
  rm(m.temp.12544522, envir = globalenv())}
}

