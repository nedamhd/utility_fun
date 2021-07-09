
Crossover.NoBaseline = function(Data, sequence, R1, R2){
  # sequence: name of sequence variable.
  # R1: name of responce variable in period 1.
  # R2: name of responce variable in period 2.
  
  Data$.sequence =  (Data[,sequence][[1]])
  Data$.R1       =  (Data[,R1][[1]])
  Data$.R2       =  (Data[,R2][[1]])
  Data$.Sum      = .R1 + .R2
  Data$.Diff     = .R1 - .R2
  
 ## utility functions ------  
 source("https://raw.githubusercontent.com/ahadalizadeh/utility_fun/master/Pips.R") 
MeanSD = function(x)  ((2 %f%mean(x, na.rm = TRUE)) %+% ' \u00B1 '  %+% (2 %f%sd(x, na.rm = TRUE)))

 ## Evaluate carry over effect ----
  CarryOverEffect = t.test(.Sum ~ .sequence, data = Data)
  
 ## Evaluate treatment effect ----
  TreatmentEffect = t.test(.Diff ~ .sequence, data = Data)

 ## Evaluate period effect ----
 SE.Diff = TreatmentEffect$stderr
 s= Data %>% group_by(.sequence) %>%  summarise(n= n(),    
                                              m = mean(.Diff, na.rm = TRUE), 
                                              sd = sd(.Diff, na.rm = TRUE))
  
# sqrt(sum((s[,2]-1)*s[,4]*s[,4]) / (sum(s[,2])-2)) * sqrt((1/s[1,2])+(1/s[2,2]))
 PeriodEffect = list()
 PeriodEffect$statistic =  ((s[1,3] + s[2,3])/ SE.Diff)[1,1]
 PeriodEffect$df = sum(s[,2])-2 
 PeriodEffect$p.value = 2 * min(c(1- pt(abs(PeriodEffect$statistic), PeriodEffect$df), 
                                     pt(abs(PeriodEffect$statistic), PeriodEffect$df)))

d= Data %>% group_by(.sequence) %>%  summarise(
  R1 =MeanSD (.R1),  R2 =MeanSD (.R2))
                                               
CarryOverEffectTable = c(t = (2 %f% CarryOverEffect$statistic), df = (2 %f% CarryOverEffect$parameter) , P.value = (3 %f% CarryOverEffect$p.value))                                             
TreatmentEffectTable = c(t = (2 %f% TreatmentEffect$statistic), df = (2 %f% TreatmentEffect$parameter) , P.value = (3 %f% TreatmentEffect$p.value))                                             
PeriodEffectTable    = c(t = (2 %f% PeriodEffect$statistic),    df = (2 %f% PeriodEffect$df),            P.value = (3 %f% PeriodEffect$p.value))                                             

effects = rbind.data.frame("CarryOverEffect" =  CarryOverEffectTable, 
                           "TreatmentEffect" =  TreatmentEffectTable, 
                           "PeriodEffect"    =  PeriodEffectTable)                                              
names(effects)  = c("t", "df", "P value")  
row.names(effects) = c("CarryOver Effect","Treatment Effect","Period Effect")
list(descriptive = d, effects = effects)

  }


 
############################## 
 
Crossover.Baseline = function(Data, sequence, B1, R1, B2, R2){
  # sequence: name of sequence variable.
  # B1: name of basline variable of responce in period 1.
  # R1: name of responce variable in period 1.
  # B2: name of basline variable of responce in period 2.
  # R2: name of responce variable in period 2.
  
  
  
  Data$.sequence =  (Data[,sequence][[1]])
  Data$.B1       =  (Data[,B1][[1]])
  Data$.R1       =  (Data[,R1][[1]])
  Data$.B2       =  (Data[,B2][[1]])
  Data$.R2       =  (Data[,R2][[1]])
  Data$.C1       =  (Data$.B1 - Data$.B2)/2
  Data$.C2       =  (Data$.B1 - Data$.R1 + Data$.B2 - Data$.R2)/2
  Data$.C3       =  (Data$.R2 - Data$.R1)/4
  
  
  ## utility functions ------  
  source("https://raw.githubusercontent.com/ahadalizadeh/utility_fun/master/Pips.R") 
  MeanSD = function(x)  ((2 %f%mean(x, na.rm = TRUE)) %+% ' \u00B1 '  %+% (2 %f%sd(x, na.rm = TRUE)))
  
  ## Evaluate carry over effect ----
  ORDER1.CarryOverEffect = t.test(.C1 ~ .sequence, data = Data)
  ORDER2.CarryOverEffect = t.test(.C2 ~ .sequence, data = Data)
  
  ## Evaluate treatment effect ----
    TreatmentEffect        = t.test(.C3 ~ .sequence, data = Data)
 
  ## Evaluate period effect ----
 
  d= Data %>% group_by(.sequence) %>%  summarise(
    B1 =MeanSD (.B1),  R1 =MeanSD (.R1),
    B2 =MeanSD (.B2),  R2 =MeanSD (.R2))

  ORDER1.CarryOverEffectTable = c(t = (2 %f% ORDER1.CarryOverEffect$statistic), df = (2 %f% ORDER1.CarryOverEffect$parameter) , P.value = (3 %f% ORDER1.CarryOverEffect$p.value))                                             
  ORDER2.CarryOverEffectTable = c(t = (2 %f% ORDER2.CarryOverEffect$statistic), df = (2 %f% ORDER2.CarryOverEffect$parameter) , P.value = (3 %f% ORDER2.CarryOverEffect$p.value))                                             
  TreatmentEffectTable        = c(t = (2 %f% TreatmentEffect$statistic),        df = (2 %f% TreatmentEffect$parameter),         P.value = (3 %f% TreatmentEffect$p.value))                                             
  
  effects = rbind.data.frame("ORDER1.CarryOverEffect" =  ORDER1.CarryOverEffectTable, 
                             "ORDER2.CarryOverEffect" =  ORDER2.CarryOverEffectTable, 
                             "TreatmentEffect"        =  TreatmentEffectTable)                                              
  names(effects)  = c("t", "df", "P value")  
  row.names(effects) = c("ORDER1 of Carry Over Effect","ORDER2 Carry Over Effect","Treatment Effect")
  list(descriptive = d, effects = effects)
  
}

