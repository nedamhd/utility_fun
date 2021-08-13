# Compute kapa
KAPPA = function(tab = NULL, a = NULL, b = NULL, c = NULL, d = NULL,
                 weights = c("Equal-Spacing", "Fleiss-Cohen"),
                 help= FALSE){
  
  .tab = tab
  .a = a 
  .b = b
  .c = c 
  .d = d
  
  if(isTRUE(help)) cat(
    "The weighted kappa coefficient takes into consideration the different levels of disagreement
between categories. For example, if one rater 'strongly disagrees' and another 'strongly agrees'
this must be considered a greater level of disagreement than when one rater 'agrees' and another 
'strongly agrees' (Tang et al. 2015).
the linear weight for a given cell is: W_ij = 1-(|i-j|)/(R-1)
the quadratic weight for a given cell is: W_ij = 1-(|i-j|)^2/(R-1)^2
were, |i-j| is the distance between categories and R is the number o categories.
'https://www.datanovia.com/en/lessons/weighted-kappa-in-r-for-two-ordinal-variables/'
Example: 
KAPPA(tab = matrix(c(2, 5, 7,9),2))
KAPPA(a = 2, b = 5, c = 7, d = 9)
\n-----------------------------------------------\n\n"
    
    
  )
  
  if(is.null(.tab) & (is.null(.a) | is.null(.b) | is.null(.c) | is.null(.d))) 
    stop("'teb' or '(a,b,c,d)' not to be null.")
  
  if(!is.null(.tab) & (!is.null(.a) | !is.null(.b) | !is.null(.c) | !is.null(.d))) 
    stop("One of 'tab' or '(a,b,c,d)' must enter.")
  
  if(is.null(.tab)) {
    .tab = as.table(matrix(c(.a, .b, .c, .d), byrow = TRUE,  nrow = 2, ncol=2))
  }else {
    .tab = as.table(.tab)
  }
  #####FUNCTION FROM vcd PACKAGE with some edite.----
  Kappa = function (x, weights = weights) 
  {
    if (is.character(weights)) 
      weights <- match.arg(weights)
    d <- diag(x)
    n <- sum(x)
    nc <- ncol(x)
    colFreqs <- colSums(x)/n
    rowFreqs <- rowSums(x)/n
    kappa <- function(po, pc) (po - pc)/(1 - pc)
    
    std <- function(p, pc, kw, W = diag(1, ncol = nc, nrow = nc)) {
      sqrt((sum(p * sweep(sweep(W, 1, W %*% colSums(p) * (1 - 
                                                            kw)), 2, W %*% rowSums(p) * (1 - kw))^2) - (kw - 
                                                                                                          pc * (1 - kw))^2)/crossprod(1 - pc)/n)
    }
    
    po <- sum(d)/n
    pc <- crossprod(colFreqs, rowFreqs)[1]
    k <- kappa(po, pc)
    s <- std(x/n, pc, k)
    p = lower =upper =c()
    for (i in 1:length(k)) {
      p[i] =   min(c(1,min(c(pnorm(k[i]/s[i]), (1 - pnorm(k[i]/s[i])))*2)))
      lower[i] = k[i]- 1.96*s[i]
      upper[i] = k[i]+ 1.96*s[i]
    }
    
    W <- if (is.matrix(weights)) 
      weights
    else if (weights == "Equal-Spacing") 
      1 - abs(outer(1:nc, 1:nc, "-"))/(nc - 1)
    else 1 - (abs(outer(1:nc, 1:nc, "-"))/(nc - 1))^2
    pow <- sum(W * x)/n
    pcw <- sum(W * colFreqs %o% rowFreqs)
    kw <- kappa(pow, pcw)
    sw <- std(x/n, pcw, kw, W)
    
    pw = lowerw =upperw =c()
    for (i in 1:length(k)) {
      pw[i] =   min(c(1,min(c(pnorm(kw[i]/sw[i]), (1 - pnorm(kw[i]/sw[i])))*2)))
      lowerw[i] = kw[i]- 1.96*sw[i]
      upperw[i] = kw[i]+ 1.96*sw[i]
    }
    "%+%" = function(x,y) paste0(x,y)
    "%r%" = function(x,y) round(y,x)
   px = round(prop.table(x) * 100, 2)  
   
  px.x = x %+% " (" %+% p.x %+% "%)"
  dim(px.x) = dim(x) 
  colnames(px.x) = colnames(x) 
  row.names(px.x) = row.names(x) 
  
    results = list()
    results = list(Agreement =as.data.frame( 
      rbind (Unweighted = 
              c(value = (3 %r%k), ASE = (3 %r%s), Lower = (3 %r%lower), 
                Upper = (3 %r%upper), P.value = (3 %r%p),
                type2 =  (2 %r% k) %+% " (" %+%   (2 %r%lower) %+% ", " %+% (2 %r% upper) %+% ")"
              ), 
            Weighted =   c(value = (3 %r%kw), ASE = (3 %r%sw),
                           Lower = (3 %r%lowerw), Upper = (3 %r%upperw), P.value = (3 %r%pw),
                           type2 =  (2 %r% kw) %+% " (" %+%   (2 %r%lowerw) %+% ", " %+% (2 %r% upperw) %+% ")"
            )),stringsAsFactors = FALSE), table = x, main.table =px.x ,
      Weights = W)
    
    
     
    results$main.table =  cbind(results$main.table,
          Agreement = c(results$Agreement["Weighted", "type2"], NA),
          P.value = c(results$Agreement["Weighted", "P.value"], NA) 
           )
    
    results
    
  }
  
  results= Kappa(x=.tab,weights=weights)
  results
}

# KAPPA(a = 19, b = 40, c = 3, d = 89)
# KAPPA(a = 5, b = 9, c = 0, d = 39)
# KAPPA(a = 23, b = 104, c = 4, d = 112)
# KAPPA(a = 4, b = 42, c = 1, d = 40)
# KAPPA(a = 0, b = 5, c = 0, d = 0, help = TRUE)
# KAPPA(a = 19+5+23+4+0, b = 40+9+104+42+5, c = 3+0+4+1+0, d = 89+39+112+40+0)
