correlations = function(data, type = c("pearson", "spearman", 1, 2)){
  if(as.character(type) == "2") type = "spearman"
  if(as.character(type) == "1") type = "pearson"

  ################
  "%f%" <- function(a, b) {
    if(is.matrix(b)) {d = dim(b); m =1}
    if(is.vector(b)) {d = length(b); m =0}
    b =as.vector(b)
    res = c()
    for(i in 1:length(b))
      res[i]=  paste0(sprintf(paste0("%.",a,"f"), b[i]) )              
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
  
  ######################################
 partial.corr = function() 
  library(ppcor)
 
  x = 
  y = 
  adjusting.list
  
   pcor()
  
  
    library("Hmisc")
  res2 <- rcorr(as.matrix(data),type = type)
  pp = ((2%f%res2$r) %+% " (") %+% ((3 %f% res2$P) %+% ")")
  colnames(pp)      =  colnames(res2$r )  
  row.names(pp)  =  row.names(res2$r ) 
  list(combine = pp, r = res2$r, p = res2$P)
}







