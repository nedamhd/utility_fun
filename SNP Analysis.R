
CI.bayes= function(M0,round=3){
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




tab2by2.test=
  function (x, y = NULL, correction = FALSE,simulate.p.value =simulate.p.value, rev = c("neither", 
                                                                                        "rows", "columns", "both")) 
  {
    if (is.matrix(x) && !is.null(y)) {
      stop("y argument should be NULL")
    }
    if (is.null(y)) {
      x <- epitable(x, rev = rev)
    }
    else {
      x <- epitable(x, y, rev = rev)
    }
    nr <- nrow(x)
    nc <- ncol(x)
    fish <- chi2 <- midp <- rep(NA, nr)
    for (i in 2:nr) {
      xx <- x[c(1, i), ]
      a0 <- x[1, 2]
      b0 <- x[1, 1]
      a1 <- x[i, 2]
      b1 <- x[i, 1]
      fish[i] <- fisher.test(xx,simulate.p.value =simulate.p.value )$p.value
      chi2[i] <- chisq.test(xx,simulate.p.value =simulate.p.value , correct = correction)$p.value
      midp[i] <- ormidp.test(a1, a0, b1, b0)$two.sided
    }
    pv <- cbind(midp, fish, chi2)
    colnames(pv) <- c("midp.exact", "fisher.exact", "chi.square")
    rownames(pv) <- rownames(x)
    names(dimnames(pv)) <- c(names(dimnames(x))[1], "two-sided")
    list(x = x, p.value = pv, correction = correction)
  }



re.third<-function(data,genotype ,group,split){
  df<-data.frame(
    data[,genotype],
    data[,group],
    data[,split])
  colnames(df)<-c(genotype ,group,split)
  
  data2<- na.omit(df)
  
  data3<- split(data2,as.factor(data2[,3]))
  data3 }


SNP.third<-function(data.t,genotype.t ,group.t,split.t=NULL ,x.1.allel,SNP.name="Genotype",split.level=NULL){
  #control group=0
  #case group=1
  #genotype: 0,1,2 for major to minor
  #x.1.allel: first element is major allel
  #x.1.allel: second element is minor allel
  #gen.name: the name of SNP
  #Microsoft word must be open
  # require(RDCOMClient)
  require(R2wd)
  # require(rscproxy )
  wdGet(T)
  fu<- function(data,
                genotype.t=genotype.t ,
                group.t=group.t,
                split.t=split.t ,
                x.1.allel=x.1.allel,
                SNP.name=SNP.name,
                split.level=split.level){
    # fu<- function(...){
    genotype.name<-c(
      paste0(x.1.allel[1],x.1.allel[1]),
      paste0(x.1.allel[1],x.1.allel[2]),
      paste0(x.1.allel[2],x.1.allel[2]))
    
    genotype.name2<-c(
      paste0(x.1.allel[1],x.1.allel[1]),
      paste0(x.1.allel[2],x.1.allel[1], "+",
             x.1.allel[2],x.1.allel[2]))
    
    
    genotype<-data[[genotype.t]]
    group<-  data[[group.t]]
    genotype2<- genotype 
    genotype2[genotype2==2]<- 1
    
    
    dat<- table(c(genotype) ,c(group))
    dat2<- table(c(genotype2) ,c(group))
    require(epitools)
    dat.re<-oddsratio(dat, rev="n")
    dat.re2<-oddsratio(dat2, rev="n")
    m1<-dat.re$data[-4,-3]
    m1211<-dat.re2$data[-3,-3]
    m2<-round(100* prop.table(  dat.re$data[-4,-3] ,2),2)
    m22<-round(100* prop.table(  dat.re2$data[-3,-3] ,2),2)
    col.sum<- colSums(dat.re$data[-4,-3])
    col.sum2<- colSums(dat.re2$data[-3,-3])
    m12<-paste0(m1," (",m2,")")
    m122<<- rep(NA,4)
    m122<<-paste0(m1211," (",m22,")")
    dim(m12)<- c(3,2)
    dim(m122)<<- c(2,2)
    dat.end=cbind(m12,
                  round(dat.re$measure,3),
                  round(dat.re$p.value[,2],4))
    
    dat.end2=cbind(m122,
                   round(dat.re2$measure,3),
                   round(dat.re2$p.value[,2],4))
    
    outc<- c( paste0( "Control (n= ",col.sum[1] ,")"),paste0( "Case (n= ",col.sum[2]  , ")"), "OR", 
              "Lower of 95% CI",   "Upper of 95% CI", "P value")
    
    
    dimnames(dat.end) <- list( "Genotype" = genotype.name,
                               "Outcome" = outc)
    
    dimnames(dat.end2) <- list( "Genotype" = genotype.name2,
                                "Outcome" = outc)
    
    ###Allel
    dat.allel <-matrix(c(
      2*m1[1,1]+m1[2,1],
      2*m1[3,1]+m1[2,1],
      2*m1[1,2]+m1[2,2],
      2*m1[3,2]+m1[2,2]),
      ncol = 2,byrow=FALSE)
    
    dat.re.al<- oddsratio(dat.allel,rev = "n")
    
    m1.al<-dat.re.al$data[-3,-3]
    m2.al<-round(100* prop.table(  m1.al ,2),2)
    m12.al<-paste0(m1.al," (",m2.al,")")
    dim(m12.al)<- c(2,2)
    dat.end.al=cbind(m12.al,
                     round(dat.re.al$measure,3),
                     round(dat.re.al$p.value[,2],4)) 
    
    rownames( dat.end.al) <- x.1.allel
    #########
    
    end<- rbind(dat.end,dat.end.al,dat.end2)
    
    
    wdTable( end,caption.pos="above",caption =ifelse(!is.null(split.t), paste0( 
      SNP.name,    ", "     ,split.t,    ", level= ", split.level[i], " (",
      data5[1,3], ")"), SNP.name))
    print("Warrning is not important-- Ahad")
    end
  }
  
  if(!is.null(split.t)){
    #   df<-data.frame(data.t[,genotype.t], data.t[,group.t], data.t[,split.t])
    #   data2<- na.omit(df)
    #   print(data2)
    #   data4<- split(data2,as.factor(data2$split.t))
    # # data3$genotype<-data4[,1]
    #   # data3$group<-data4[,2]
    # lapply(x=data4, FUN=fu,data=data4,genotype=data4$genotype2 ,group=data4$group2,x.1.allel=x.1.allel.t,SNP.name=SNP.name.t)}
    data4= re.third(data=data.t,genotype=genotype.t ,group=group.t,split=split.t)
    
    n=length(data4)
    # print(n)
    # print(data4)
    for (i in 1:n) {
      data5<-data4[[i]]
      
      # print(data5)
      fu(data=data5,
         genotype.t=genotype.t ,
         group.t=group.t,
         split.t=split.t ,
         x.1.allel=x.1.allel,
         SNP.name=SNP.name,
         split.level=split.level)
      
    }}
  
  if(is.null(split.t)){
    fu(data=data.t,
       genotype.t=genotype.t ,
       group.t=group.t,
       split.t=split.t ,
       x.1.allel=x.1.allel,
       SNP.name=SNP.name,
       split.level=split.level)
  }
  
}


oddsratio2=
  function (x, y = NULL,simulate.p.value =simulate.p.value, method = c("midp", "fisher", "wald", "small"), 
            conf.level = 0.95, rev = c("neither", "rows", "columns", 
                                       "both"), correction = FALSE, verbose = FALSE) 
  {
    if (is.matrix(x) && !is.null(y)) {
      stop("y argument should be NULL")
    }
    if (is.null(y)) {
      x <- epitable(x, rev = rev)
    }
    else {
      x <- epitable(x, y, rev = rev)
    }
    method <- match.arg(method)
    if (method == "midp") {
      rr <- oddsratio.midp(x, conf.level = conf.level, verbose = verbose, 
                           correction = correction)
    }
    if (method == "fisher") {
      rr <- oddsratio.fisher(x, conf.level = conf.level, verbose = verbose, 
                             correction = correction)
    }
    if (method == "wald") {
      rr <- oddsratio.wald(x, conf.level = conf.level, verbose = verbose, 
                           correction = correction)
    }
    if (method == "small") {
      rr <- oddsratio.small(x, conf.level = conf.level, verbose = verbose, 
                            correction = correction)
    }
    rr
  }


my.table.cat=
  function(data,formula,x.labels.table){ 
    # formula: just be a group and response
    
    x<- all.vars(formula)[2]
    y<- all.vars(formula)[1]
    d<- na.omit(as.data.frame(cbind(data[,x],data[,y])))
    names(d)<-c(x,y)
    yy<- c(y, paste(  rep("NA",(length(  unique(d[,y]))-1)),letters[1:(length(  unique(d[,y]))-1)]))
     Tab1<- table(d[,y], d[,x]) 
    chi.t<-chisq.test( 
      Tab1
      ,simulate.p.value =TRUE)
    freq<- matrix(paste0(round(prop.table(Tab1,2)*100,2)," (",Tab1 ,")" ),dim(Tab1)[1])
    # colnames(freq)<- paste0 (x.labels.table," (n= " ,margin.table(Tab1,2),")")
    colnames(freq)<- paste0 (x.labels.table," (n= " ,")")
    if( is.null(  names( attr(data[[y]],"labels")))) freq<-cbind(Subgroup=levels(as.factor(d[[y]])),freq) else
      freq<-cbind(Subgroup=names( attr(data[[y]],"labels"))[attr(data[[y]],"labels") %in% levels(as.factor(d[[y]]))],freq)
    rownames(freq)   <-yy
    freq<- as.data.frame(freq)
    freq[1,"P Value"]<- ifelse(   round(chi.t$p.value,3)==0, "<0.001",round(chi.t$p.value,3))
    list(Table=freq, freq=Tab1)}


SNP.table=
  
  function( data,formula,
            allel.lable=c("G","T"),
            genotype=NULL,
            x.labels.table.sad,
            type.of.non.zero=c("common.homozigot", "rare.homozigot"),
            method=c("glm","bayesglm")
  ){ 
    
    if(! method %in% c("glm","bayesglm")) stop("Ahad: Method should be 'glm' or 'bayesglm'")
    
    if(is.null(genotype))
      genotype=c(
        paste0(allel.lable[1],allel.lable[1]),
        paste0(allel.lable[1],allel.lable[2]),
        paste0(allel.lable[2],allel.lable[2]))
    
    
    cat1<-   my.table.cat(data,formula=formula,x.labels.table.sad)
    x<- all.vars(formula)[2]
    y<- all.vars(formula)[1]
    d<- na.omit(as.data.frame(cbind(data[,x],data[,y])))
    names(d)<-c(x,y)
    if(dim(cat1$freq)[1]==3){ 
      Tabl.allel<-rbind( 
        cat1$freq[1,]*2+cat1$freq[2,],
        cat1$freq[3,]*2+cat1$freq[2,])}
    
    if(dim(cat1$freq)[1]==2){ 
      warning("Dimension of table is TWO")
      if(type.of.non.zero=="common.homozigot")  { 
        Tabl.allel<-rbind( 
          cat1$freq[1,]*2+cat1$freq[2,],
          cat1$freq[2,] )
      }
      if(type.of.non.zero=="rare.homozigot"  )  {
        Tabl.allel<-  cat1$freq[1,]+cat1$freq[2,]*2 }
    }
    
    
    if(dim(cat1$freq)[1]==1){ 
      warning("Dimension of table is ONE")
      Tabl.allel<-  cat1$freq[1,]*2}
    
    
    p.table.allel<- round( prop.table( Tabl.allel,2)*100,2)
    colnames(p.table.allel)<-allel.lable
    ch.t<-chisq.test(Tabl.allel,simulate.p.value =TRUE) 
    freq<-data.frame( "Subgroup"=allel.lable, matrix(paste0(p.table.allel," (",Tabl.allel ,")" ),dim(Tabl.allel)[1]))
    rownames(freq)<-allel.lable
    freq[1,"P Value"]<- ifelse(round(ch.t$p.value,3)==0, "<0.001",round(ch.t$p.value,3))
    names(freq)<- names(cat1$Table)
    cat1$Table[,"Subgroup"] <-genotype
    
    if(dim(cat1$freq)[1]!=3){
      return( list(Table =rbind(  cat1$Table,freq[1:2,]),Tabl.allel=Tabl.allel,y=y,allel.lable=allel.lable)
      )
    }
    
    
    if(dim(cat1$freq)[1]==3){
      y=c(0,0,0,1,1,1)
      w=c(cat1$freq)
      Snp<- c(0,1,2,0,1,2)
      library(arm)
      if(method=="glm"){  
        ci=CI.bayes( glm(y~as.factor(Snp),weights = w, family = binomial(logit)))$type2.result
      } 
      if(method=="bayesglm"){  
        ci=CI.bayes(  bayesglm(y~as.factor(Snp),weights = w, family = binomial(logit)))$type2.result
      }
      ci=apply(ci,2 , as.character)
      ci[1,]="-"
      a1=cbind(cat1$Table,ci)
      
      y=c(0,0,1,1)
      w=c(Tabl.allel)
      allels<- c(0,1,0,1)
      if(method=="glm"){  
        ci2=CI.bayes( glm(y~as.factor(allels),weights = w, family = binomial(logit)))$type2.result
      }
      if(method=="bayesglm"){  
        ci2=CI.bayes( bayesglm(y~as.factor(allels),weights = w, family = binomial(logit)))$type2.result
      }
      
      
      ci2=apply(ci2,2 , as.character)
      ci2[1,]="-"
      a2=cbind(freq[1:2,],ci2)
      
      # list(Table =rbind(  cat1$Table,freq[1:2,]),Tabl.allel=Tabl.allel,y=y,allel.lable=allel.lable)
      return( list(Table =rbind( a1,a2),Tabl.allel=Tabl.allel,y=y,allel.lable=allel.lable))
    }
    
    
  }

########################################

cat(' Example:\n
 Re = SNP.table ( data= samples, formula = rs3735590 ~ CAD.FollowUp,
                   allel.lable=c("G","A"),
                   genotype=NULL,
                   x.labels.table.sad = c("non-CAD", "CAD" ),
                   type.of.non.zero=c("common.homozigot" ),
                   method=c("bayesglm" )
')


# Re = SNP.table ( data= samples, formula = rs3735590 ~ CAD.FollowUp,
#                  allel.lable=c("G","A"),
#                  genotype=NULL,
#                  x.labels.table.sad = c("non-CAD", "CAD" ),
#                  type.of.non.zero=c("common.homozigot" ),
#                  method=c("bayesglm" )
