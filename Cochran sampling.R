Cochran <- function(p = 0.5,d = 0.05, alpha = 0.05, N = Inf){
  if (!is.infinite(N)) n = (N*(qnorm(1-(alpha/2))^2)*p*(1-p))/((N*(d^2))+((qnorm(1-(alpha/2))^2)*p*(1-p)))
  if (is.infinite(N))  n = ( (qnorm(1-(alpha/2))^2)*p*(1-p))/ (d^2)
  
  cat("Sample size is: ", ceiling(n)) 
  invisible(ceiling(n))
}