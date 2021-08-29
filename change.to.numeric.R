change.to.numeric <- function(x){
  temp = x
   if (is.atomic(temp)) x <- as.numeric(as.character(x))
  
  if (is.data.frame(temp)) {
    x <- as.data.frame(  apply(x, 2, function(x) as.numeric(as.character(x))) )
    
    for (i in 1:dim(x)[2]) {
      if(all(is.na(x[,i]))) x[,i] <- as.character(temp[,i] )
      row.names(x)<- row.names(temp)
    }
  }
  
  if (is.matrix(temp)) {
     x <- as.data.frame(x)
    x <- apply(x, 2, function(x) as.numeric(as.character(x)))
    x <- as.data.frame(x)
      for (i in 1:(dim(x)[2])) {
      if(all(is.na(x[,i]))) x[,i] <- as.character(temp[,i] )
      row.names(x)<- row.names(temp)
      
    }
  }
  x
}
 
