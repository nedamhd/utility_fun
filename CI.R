CI= function(M0,round=3){
  Re2<- data.frame()
if( class(M0)[1]== "bayesglm" ){
  cat("Ahad: Bayesian method by bayesglm\n\n")
   s.M0=summary(M0)
  M0.s=coef(sim(M0,n=10000))
  CI<- apply(M0.s,2,quantile,
             c(0.025,0.975))
  mean.sim <- apply(M0.s,2,mean)
  Re=data.frame (OR= round( exp(mean.sim),round),round(t( exp(CI)),round),P.value=round(s.M0$ coefficients[,4],round+1))
  
  
 Re2= data.frame("OR(95% CI)"= ( paste0( Re[,1],"(", Re[,2],",", Re[,3], ")")), "P value"= Re[,4])
row.names(Re2)=row.names(Re)
}
  

  if( class(M0)[1]== "glm" ){
 cat("Ahad: Classic method by glm\n\n")
  s.M0=summary(M0)
  CI<-  (confint(M0))
  mean.sim <- s.M0$coefficients[,1]
  Re=data.frame (OR= round( exp(mean.sim),round),round(( exp(CI)),round),P.value=round(s.M0$ coefficients[,4],round+1))
  
  
  Re2= data.frame("OR(95% CI)"= (paste0( Re[,1],"(", Re[,2],",", Re[,3], ")")), "P value"= Re[,4])
  row.names(Re2)=row.names(Re)
  }
  list(type1.result=Re,type2.result=Re2)
  }
