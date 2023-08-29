Repeat.measurment =  function(data = NULL, 
                              formula  , 
                              ID,
                              melt.data = NULL,
                              comparison.formula = NULL,
                              glht.linfct.matrix = NULL,
                              adjust ="none"
){
  # data: wide data
  # formula: like 'cbind(y1, y2, y3) ~(Age+ Sex+ Group)*Time'
  # ID: A variable to identify the subjects 
  # comparison.formula: Formula for multiple comparison like  '~ Group|Time' or '~ Group*Time'
  # adjust: p value adjustment. see '?p.adjust()'
  
  DV.names = all.vars(update(formula, . ~ 1))
  IV.names = all.vars(update(formula, 1 ~ .))
  Time = factor(c(DV.names))
  if(! "Time" %in% IV.names) stop("The name of within subjevt varible in the formula must be 'Time'.")
  IV.names = IV.names[which(IV.names != "Time")]

  formula2<- formula
  if(!is.null(data) ){ 
    data = na.omit(data[,c(ID, IV.names, DV.names)])
    melt.data = reshape2::melt(data,id= c(ID,IV.names), measure.vars = DV.names, variable.name = "Time")
    formula2 <-Reduce(paste, deparse(update(formula, value ~ .)))
    
  } else 
    if(is.null(melt.data)) {
      stop("data or melt.data is empty")
    }  
  IV.names = c(IV.names, "Time")
  
  options(contrasts = c("contr.sum","contr.poly"))
  ...fixed123456789 <<- formula2
  M1 <-nlme::lme(fixed= as.formula(...fixed123456789),
                 random= as.formula(paste0("~ 1|", ID)), data=melt.data,
                 method="REML",
                 correlation=nlme::corCompSymm(form=   as.formula(paste0("~ 1|", ID))))
  # M2<- lm( formula,data=data)
  Anova.M1 <- car::Anova(M1 , type = "III")
  # Anova.M1 = Anova(M2, idata=data.frame(Time), idesign=~Time, type = "III")
  main.table =as.data.frame(Anova.M1)  
  main.table$Chisqstat= paste0(round(Anova.M1$Chisq,2), " (", Anova.M1$Df, ")")
  main.table<- data.frame("Chisq (df)"=  main.table[,4],
                          "P.value"=round(  main.table[,3],3))
  row.names(main.table) = row.names(Anova.M1)
  
  
  comparison        = data.frame()
  emm               = data.frame()
  comparison.object = data.frame()
  l=NULL
  if(!is.null(comparison.formula)){
    emm <- emmeans::emmeans(M1, comparison.formula)
    comparison.object = emmeans::contrast(emm, interaction = "pairwise", adjust = adjust) 
    comparison = data.frame(comparison.object)
    # pairs(emm)  # adjust argument not specified -> default p-value adjustment in this case is "tukey"  
    
    if(dim(comparison)[2] == 7 ) {
      by = unique(comparison[,2] )
      
      
      l = list()
      require(multcompView)                       
      for(i in 1:length(by)) {
        comparison.by =comparison[which(comparison[,2] == by[i]), c(1,7)]
        comparison.by.p =  comparison.by[,2]
        comparison.by.name =  strsplit(as.character(comparison.by[,1]), " - ")
        comparison.by.name2 = c()
        for (ii in 1:length(comparison.by.name)) {
          comparison.by.name2[ii] = paste0(comparison.by.name[[ii]], collapse = "-")
        }
        names(comparison.by.p) = comparison.by.name2
        l[[i]]<-multcompLetters(
          comparison.by.p
        )$Letters
        
      }
      l =do.call(rbind,l)
      
      row.names(l)= by
      
    }
    
    if(dim(comparison)[2] == 6 ) {
      
      
      l = c()
      require(multcompView)                       
      comparison.by =comparison[ , c(1,6)]
      comparison.by.p =  comparison.by[,2]
      comparison.by.name =  strsplit(as.character(comparison.by[,1]), " - ")
      comparison.by.name2 = c()
      for (ii in 1:length(comparison.by.name)) {
        comparison.by.name2[ii] = paste0(comparison.by.name[[ii]], collapse = "-")
      }
      names(comparison.by.p) = comparison.by.name2
      l <-multcompLetters(
        comparison.by.p
      )$Letters
      
    }
    
    
  }
  
  
  ss2 = NULL
  if(!is.null(glht.linfct.matrix)){
    if(ncol(glht.linfct.matrix) != length(coef(M1))) {
      cat("coefficients names have been returned.\nError: ncol(linfct) is not equal to length(coef(model))" )
      return(names(coef(M1)))
      stop(  )   
    }
    library(multcomp)
    warpbreaks.mc= glht(M1, linfct = as.matrix(glht.linfct.matrix))
    ss = summary(warpbreaks.mc)
    ss2= round(cbind.data.frame("Estimate"=ss$test$coefficients, 
                                "Std. Error"= ss$test$sigma, 
                                "z value" = ss$test$tstat, 
                                "pvalues" = ss$test$pvalues) ,3) 
    
  }
  
  
  ...fixed123456789<<- NULL  
  rm(...fixed123456789, envir = globalenv()) 
  res = list(main.results = list( main.table = main.table, 
                                  comparison = comparison, 
                                  letter= l,
                                  glht.results = ss2 
  ),
  invisible.results = list(data = melt.data, 
                           comparison.formula = comparison.formula,
                           lme.model = M1, 
                           car.Anova = Anova.M1, 
                           emmeans = list(emmeans = emm, 
                                          contrast = comparison.object))                 
  
  )
  class(res) = "Repeat.measurment"
  res
}

###################Examples#######################
# Data  = data.frame(
#   R = 1:1000,
#   Age = abs(rnorm(1000,32,10)),
#   Group = factor(rbinom(1000,3,0.5)),
#   Sex = factor(rbinom(1000,1,0.5)),
#   y1 =  (rnorm(1000)),
#   y2 =  (rnorm(1000)) ,
#   y3 = abs(rnorm(1000))
# )
# 
# 
# MM1 =Repeat.measurment (data =Data, formula = cbind(y1, y2, y3) ~(Age+ Sex+ Group)*Time,
#                         ID = "R",
#                         comparison.formula = ~ Group|Time)
# MM1$main.results$letter
# 
# MM2 =Repeat.measurment (data =Data, formula = cbind(y1, y2, y3) ~(Age+ Sex+ Group)*Time, 
#                         ID = "R",
#                         comparison.formula = ~ Group*Time)
# 
# 
# MM2$main.results
# 
