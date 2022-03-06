DASS = 
  function(data, vars = NULL){
    if(is.null(vars)){
      if(dim(data)[20 != 21]) stop("The number of variables must be 21.")
      Depression = rowSums(data[,c(21,17,16,13,10,5,3)])
      Anxiety    = rowSums(data[,c(20,19,15,9,7,4,2)])
      Stress     = rowSums(data[,c(18,14,12,11,8,6,1)])
   } else {
     if(length(vars) != 21) stop("The number of variables must be 21.")
        data = data[,vars]
         Depression = rowSums(data[,c(21,17,16,13,10,5,3)])
        Anxiety    = rowSums(data[,c(20,19,15,9,7,4,2)])
        Stress     = rowSums(data[,c(18,14,12,11,8,6,1)])
       }
   
 data.frame(Depression, Anxiety, Stress)
 }

