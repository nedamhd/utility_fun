# my pipes 
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


