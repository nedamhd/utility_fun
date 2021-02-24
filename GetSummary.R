
GetSummary <- function(data, dep.quantitative, factor){
  # data$factor <- as.factor(data[,factor])
 
  ## Pipes ---------------------------------
  "%f%" <- function(a, b) {
    if(is.matrix(b)) {d = dim(b); m =1}
    if(is.vector(b)) {d = length(b); m =0}
    b =as.vector(b)
    res = c()
    for(i in 1:length(b))
      res[i]=  paste0(sprintf(paste0("%.",a,"f"), b[i]) )              
    if(m==1) res= as.data.frame(matrix(res, nrow =d[1] ,ncol =d[2] )) 
    return(res) 
  }
  "%+%" <- function(a, b) paste0(a, b)
  #---------------------------------------------- 
  
  
  # ??Functions---------------------------------------
  quantileFUN  <- function(x, significant.number = 3) {
    r =  quantile(x, p =c(0.5, 0.25, 0.75)) 
    r2 = paste0(significant.number%f%r[1], 
                " (", 
                significant.number%f%r[2], 
                ", ", 
                significant.number%f%r[3], 
                ")")
    
           r2
  }
  
  meansdFUN  <- function(x, significant.number = 3) {
    m =  mean(x, na.rm = TRUE) 
    s =  sd(x, na.rm = TRUE) 
    r2 = paste0(significant.number %f% m, 
                ' \u00B1 ', 
                significant.number %f% s
                )
    
    r2
  }
  
  
  meanseFUN  <- function(x, significant.number = 3) {
    m =  mean(x, na.rm = TRUE) 
    s =  sd(x, na.rm = TRUE) 
    n = length(which(!is.na(x)))
    se = s / sqrt(n)
    r2 = paste0(significant.number %f% m, 
                ' \u00B1 ', 
                significant.number %f% se
    )
    
    r2
  }
  
  
  CIFUN  <- function(x, significant.number = 3, alpha = 0.05) {
    m =  mean(x, na.rm = TRUE) 
    s =  sd(x, na.rm = TRUE) 
    n = length(which(!is.na(x)))
    se = s / sqrt(n)
    lower = m - qnorm(1-(alpha/2)) * se
    upper = m + qnorm(1-(alpha/2)) * se
    r2 = paste0(significant.number%f%m, 
                " (", 
                significant.number%f%lower, 
                ", ", 
                significant.number%f%upper ,
                ")"
    )
    
    r2
  }
 #------------------------------------------- 

 require(dplyr) 
 

 meansd = data %>% group_by(get(factor))  %>%
    summarise(across(all_of(dep.quantitative),
                     ~ meansdFUN(.x)), .groups = 'drop')
  
 meanse = data %>% group_by(get(factor))  %>%
    summarise(across(all_of(dep.quantitative),
                     ~ meanseFUN(.x)), .groups = 'drop')
  
  
 CI = data %>% group_by(get(factor))  %>%
    summarise(across(all_of(dep.quantitative),
                     ~ CIFUN(.x)), .groups = 'drop')
 
   
 Quantile = data %>% group_by(get(factor))  %>%
    summarise(across(all_of(dep.quantitative),
                     ~ quantileFUN(.x)), .groups = 'drop')
 
 
  
 MeanSD         = as.data.frame(t(meansd))
 MeanSE         = as.data.frame(t(meanse))
 CI             = as.data.frame(t(CI))
 MedianQuantile = as.data.frame(t(Quantile))
 
   names(MeanSD) = 
   names(MeanSE) = 
   names(CI) = 
   names(MedianQuantile) = 
     paste0(factor, ": " , c( Quantile[['get(factor)']]))

 
names(meansd)[1] = names(meanse)[1] = names(CI)[1] = names(Quantile)[1] = factor
 
list(MeanSD = MeanSD[-1,], MeanSE = MeanSE[-1,], CI =  CI[-1,], MedianQuantile = MedianQuantile[-1,])

}

#-------------------------------
# d = data.frame(sex=rbinom(10000,1,0.5), age = rnorm(10000), bmi = rnorm(10000))
# Gkk= GetSummary (data = d, dep.quantitative= c("age", "bmi"), factor = "sex")
# Gkk
# Gkk$MeanSD   %>% wd.Table()




