source("https://raw.githubusercontent.com/ahadalizadeh/utility_fun/master/utility_fun.R")
 
 chisqTest <- function(x,y,data=NULL){
   # y: Column variable.
   # x: row variable.
   
   if(!is.null(data)){
         x <- c(data[, deparse(substitute(x))])
         y <- c(data[, deparse(substitute(y))])
    }
    
    t <- table(x ,y)
    p.t <- prop.table(t,2)
    s.t <- matrix(paste0(t,"(", round(p.t*100,2), ")"),nrow = dim(t)[1])
  
      s.t <-cbind(s.t,
       c(round(chisq.test(x,y, simulate.p.value = TRUE)$p.value, 3),
                 rep(NA, dim(t)[1]-1)))
    class(s.t)<- "chisqTest"
    row.names(s.t)<-  row.names(t)  
    colnames(s.t)<-  c(colnames(t), "Pvalue")
    s.t
  }
 
# attach(mydata2)
# chisqTest(x=Calciumchannelblocker, y=CAD )  %>% write.cb()
