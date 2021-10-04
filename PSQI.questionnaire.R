PSQI.questionnaire = 
function(data, questions){
  # Pittsburgh Sleep Quality Index
  # questions: name or index of questions 1:18, respectivly.
  if(length(questions) != 18) stop("Number of questions must be exactly 18.")
  for (i in c(6:15,18))  
    if( !all( c(data[[questions[i]]])%in% c(NA,0:3))) stop(paste0("value of questions ", i," must be exactly 0:3"))
 
  
  J = data[,questions]
  names(J) = paste0("q", 1:18)
  data$keifiyat.zehni.khab = J$q18
  
  J$q2.cat = NA  
  J$q2.cat[which( J$q2<=1000)] =  3
  J$q2.cat[which( J$q2<=60)] =  2
  J$q2.cat[which( J$q2<=30)] =  1
  J$q2.cat[which( J$q2<=15)] =  0
  data$Takhir.dar.be.khab.raftan = J$q2.cat + c(J$q6)
  
  data$Tole.moddat.khab = NA 
  data$Tole.moddat.khab[which( J$q2<=1000)] =  0
  data$Tole.moddat.khab[which( J$q2<=7)]    =  1
  data$Tole.moddat.khab[which( J$q2<=6)]    =  2
  data$Tole.moddat.khab[which( J$q2<=5)]    =  3

  
  J$q1.change = J$q1 - 12
  J$q1.change [which(abs(J$q1.change) > 6)] =J$q1[which(abs(J$q1.change) > 6)]
  
  data$Karayi = NA
  data$Karayi[which( (J$q4/abs((J$q1.change ) - J$q3)*100) <=101)] =  0
  data$Karayi[which( (J$q4/abs((J$q1.change ) - J$q3)*100) <=85)] =  1
  data$Karayi[which( (J$q4/abs((J$q1.change ) - J$q3)*100) <=75)] =  2
  data$Karayi[which( (J$q4/abs((J$q1.change ) - J$q3)*100) <=65)] =  3

  data$Ekhtelale.khab = NA
  data$Ekhtelale.khab[which( with(J,q6+q7+q8+q9+q10+q11+q12+q13+q14+q15) <=27)]   =  3
  data$Ekhtelale.khab[which( with(J,q6+q7+q8+q9+q10+q11+q12+q13+q14+q15) <=18)]   =  2
  data$Ekhtelale.khab[which( with(J,q6+q7+q8+q9+q10+q11+q12+q13+q14+q15) <=9)]    =  1
  data$Ekhtelale.khab[which( with(J,q6+q7+q8+q9+q10+q11+q12+q13+q14+q15) <=0)]    =  0

  # data$Ekhtelale.khab = c(cut(with(J,q6+q7+q8+q9+q10+q11+q12+q13+q14+q15), breaks = c(-10,0,9,18,27)))-1
  data$Masraf.daru.khabavar = J$q15 
  J$Ekhtelale.amalkarde.rozane.temp = with(J,q16+q17)
  
  data$Ekhtelale.amalkarde.rozane = NA
  data$Ekhtelale.amalkarde.rozane[which( J$Ekhtelale.amalkarde.rozane.temp <=  6)]   =  3
  data$Ekhtelale.amalkarde.rozane[which( J$Ekhtelale.amalkarde.rozane.temp <=  4)]   =  2
  data$Ekhtelale.amalkarde.rozane[which( J$Ekhtelale.amalkarde.rozane.temp <=  2)]   =  1
  data$Ekhtelale.amalkarde.rozane[which( J$Ekhtelale.amalkarde.rozane.temp <=  0)]   =  0


  data$PSQI = with(data, keifiyat.zehni.khab + 
                     Takhir.dar.be.khab.raftan +
                     Tole.moddat.khab +
                     Karayi +
                     Ekhtelale.khab +
                     Masraf.daru.khabavar +
                     Ekhtelale.amalkarde.rozane
                   
  )
  data
}
