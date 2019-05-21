K=c(30,50,100,300) #the value of n
numSimulations <- 500
confounders.ID<- 1:3 
familyX<- "gaussian"
familyY<- "gaussian"
nburn=1000
ndraw=10000
existsfile=TRUE


# Loop for sample size----------------------- 
for(k in 1:length(K)){
  if(K[k]==30) P=c(5,10,15,27 )
  if(K[k]==50) P=c(5,10,25,47 )
  if(K[k]==100) P=c(5,10,50,95 )
  if(K[k]==300) P=c(5,10,150,285)
  # P=c(5,10,round(c(0.1, 0.2, 0.5, 0.94 )*K[k]))
  # P=c(round(c(0.95)*K[k]))
  for(i in 1:length(P)){
    p<-   P[i]
    n<-   K[k]
    
    
    #Check for previous data of loop ----------------------- 
    
      rTotal.s.STEPWISE<- matrix(NA, nrow = numSimulations, ncol = 9)
      start=1
  
      
      
      
      progression   <-  winProgressBar(min = 0,label="% done",
                                       max = numSimulations, width = 300)
      
    
    # Loop for simulations----------------------- 
    for(simNum in start:numSimulations ){
     
      rTotal.stepwise        <- list()
     
      
      
      bX<-  c(     1.0,  0.1,  1.0, 0.0, 1.0, rep(0,p-5))
      bY<-  c(1.0, 1.0,  1.0,  0.1, 1.0, 0.0, rep(0,p-5))
      simdata<- data.generator(p=p,n=n,bX=bX,bY=bY,seed=p+n+simNum,
                               familyX=familyX,familyY=familyY)
      D<- as.matrix(simdata[,-c(1:2)])
      X<- simdata[,"X"] 
      Y<- simdata[,"Y"]
      
  
      
     
      
         
        setWinProgressBar(progression, simNum,title =
                            paste0( "Coding by Dr. Ahad Alizadeh, ",
                                    round(simNum/(numSimulations)*100),"% done."),
                          label= paste0("simNum= ",simNum,   ", n= ", n,", P= ",p)
                          )
        
        if(simNum==(numSimulations)) close(progression)     
      
      
      
      
      
 
    r.full <-  glm(as.formula(paste0("Y ~ X +", paste0("U",1:p , collapse = "+"))),
                   data=simdata,  family = familyY)
     r.full.s<- summary(r.full)
    


  sink("res.txt") 
  r.stepwise <- MASS::stepAIC(r.full, scope = list(lower = ~X) , 
                              direction = "both", k = log(n),steps = 5000000)
  r.stepwise.s<- summary(r.stepwise)
   sink() 
  
   rTotal.s.STEPWISE[simNum,]<-  cbind(simNum,n,p,seed=p+n+simNum,
                             full.est= r.full.s$coefficients["X",c("Estimate")],
                             full.se =  r.full.s$coefficients["X",c("Std. Error")],
                             full.converged =  as.numeric(r.full$converged),
                             stepwise.est=  r.stepwise.s$coefficients["X",c("Estimate")],
                             stepwise.se = r.stepwise.s$coefficients["X",c("Std. Error")] )
   
   
   
   
   
    }
      
      
      if(Sys.info()["nodename"]=="DESKTOP-6OBO0CN")
        save(rTotal.s.STEPWISE, file=paste0("C:/Users/Ahad/Dropbox/Main/Simulation1/Total.STEPWISE-n",K[k]," -p",p,".RData"))
      
      if(Sys.info()["nodename"]=="AMAR49")
        save(rTotal.s.STEPWISE, file=paste0("C:/Users/2/Dropbox/Main/Simulation2/Total.STEPWISE-n",K[k]," -p",p,".RData"))
       
   
  }
  
}
   
                             