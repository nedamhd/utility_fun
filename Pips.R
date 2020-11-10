# my pipes 
"%f%" <- function(a, b) {
     if(is.matrix(b)) {d = dim(b); m =1}
     if(is.data.frame(b)) {d = dim(b); m =1}
     if(is.vector(b)) {d = length(b); m =0}
     if(is.list(b)) stop("data must be atomic vector, DF or matrix")
     b = c(b)
     res = c()
     for(i in 1:length(b))
     res[i]=  paste0(sprintf(paste0("%.",a,"f"), b[i]) )              
   if(m==1) res= as.data.frame(matrix(res, nrow =d[1] ,ncol =d[2] )) 
    return(res) 
   }
"%+%" <- function(a, b) paste0(a, b)
