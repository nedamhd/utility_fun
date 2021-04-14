
Repeat.measurment =  function(data, formula  ){
library(car)
require(heplots)
require(broom)
require(sjstats)
# require(biotools)
DV.names=all.vars(update(formula, . ~ 1))
options(contrasts = c("contr.sum","contr.poly"))

M1 = lm( formula, data = data)
Time = factor(c(DV.names))
Anova.M1 = Anova(M1, idata=data.frame(Time), idesign=~Time, type = "III")
s.Anova.model = summary(Anova.M1, multivariate = F)
table.me<-etasq(Anova.M1, anova = TRUE, type="III" )
main.table<- table.me [,c("test stat","Pr(>F)")]
main.table$F= paste0(round(table.me$`approx F`,2), " (", table.me$`num Df`, ", ", table.me$`den Df`, ")")
main.table<- data.frame("Pillai test"=round(  main.table[,1] ,3),main.table[,3] ,"P.value"=round(  main.table[,2],3))
names(main.table)<- c("Pillai test" ,"F (df1, df2)", "P.value" )
row.names(main.table) = row.names(table.me)
multivariate = main.table

ID.names<-  attr(terms(formula),"term.labels")        # all.vars(update(formula, 1 ~ .))
  sphericity.tests = s.Anova.model$sphericity.tests 
  class(s.Anova.model$univariate.tests) = NULL
  uni.s.Anova.model =  round(as.data.frame( as.matrix(s.Anova.model$univariate.tests))[,-c(1,3)], 3)
  uni.s.Anova.model$name = row.names(s.Anova.model$univariate.tests) 
  
  pval.adjustments = round(s.Anova.model$pval.adjustments, 3)
  class(pval.adjustments) = NULL
  pval.adjustments =as.data.frame( as.matrix(pval.adjustments))[,1:2]
  pval.adjustments$name = row.names( s.Anova.model$pval.adjustments) 
  
 m= base::merge(uni.s.Anova.model, pval.adjustments,by= "name", all = TRUE )
 m$numDf.GG = round(m$`num Df`* as.numeric(as.character(m$`GG eps`)), 2)   
 m$denDf.GG = round(m$`den Df`* as.numeric(as.character(m$`GG eps`)), 2)   
 univariate = data.frame(F.sphericity = paste0(m[,4] , " (", m[,2], ", ", m[,3], ")"), P.sphericity = m[,5], 
                                 F.GG = paste0(m[,4] , " (", m[,8], ", ", m[,9], ")"), P.GG         = m[,7])
                      
 row.names(univariate) = m$name                   
  
 list(multivariate = multivariate, univariate = univariate, sphericity.tests=round(sphericity.tests,3))
 }
 
  # biotools::boxM(data[,DV.names ],c(data$Group))                    
  # bartlettTests(data[,DV.names ],c(data$Group))                   
  # leveneTest(c(data$`ANB-pre` ),as.factor(data$Group*data$Sex))
  # leveneTest(  `FMA-post` ~as.factor(Group), 
  #              data = Data, center=mean )
  



# Repeat.measurment (data=data, formula =   
# cbind(`ANB-pre`, `ANB-post`, `FMA-post`) ~as.factor(Group) )
   
# Data  = data.frame(
#   x1 = abs(rnorm(1000)),
#   x2 = factor(rbinom(1000,1,0.5)),
#   x3 = factor(rbinom(1000,1,0.5)),
#   y1 = abs(rnorm(1000)),
#   y2 = abs(rnorm(1000)),
#   y3 = abs(rnorm(1000))
# )
# 
#  s=Repeat.measurment(data = Data, formula = cbind( y1,y3, y2) ~ x2*x3   )  


               