

PSQI.questionnaire <- function(data, questions){
  # Pittsburgh Sleep Quality Index
  # questions: name or index of questions 1:18, respectivly.
  if(length(questions) != 18) stop("Number of questions must be exactly 18.")
  for (i in c(6:15,18))  
    if( !all( c(data[[questions[i]]])%in% c(NA,0:3))) stop(paste0("value of questions ", i," must be exactly 0:3"))
  J = data
  
  names(J[,questions]) = paste0("q", 1:18)
  data$keifiyat.zehni.khab = J$q18
  J$q2.cat = c(cut(J$q2 , breaks = c(0,15,30,60)))-1
  data$Takhir.dar.be.khab.raftan = J$q2.cat + c(J$q6)
  J$q2.cat = NULL
  data$Tole.moddat.khab = 4-c(cut(J$q4 , breaks = c(0,5,6,7,1000))) 
  J$q1.change = J$q1 - 12
  J$q1.change [which(abs(J$q1.change) > 6)] =J$q1[which(abs(J$q1.change) > 6)]
  data$Karayi = (4-c(cut(J$q4/abs((J$q1.change ) - J$q3)*100, breaks = c(0,65,75,85,101))) )
  J$q1.change = NULL
  data$Ekhtelale.khab = c(cut(with(J,q6+q7+q8+q9+q10+q11+q12+q13+q14+q15), breaks = c(-10,0,9,18,27)))-1
  data$Masraf.daru.khabavar = J$q15 
  J$Ekhtelale.amalkarde.rozane.temp = with(J,q16+q17)
  data$Ekhtelale.amalkarde.rozane = c(cut(J$Ekhtelale.amalkarde.rozane.temp, breaks = c(-10,0,2,4,6)))-1
  J$Ekhtelale.amalkarde.rozane.temp = NULL
  
  data$PSQI = with(data, keifiyat.zehni.khab+ 
                     Takhir.dar.be.khab.raftan+
                     Tole.moddat.khab+
                     Karayi+
                     Ekhtelale.khab+
                     Masraf.daru.khabavar+
                     Ekhtelale.amalkarde.rozane
                   
  )
  data
}

# J= PSQI.questionnaire (data=J, questions = paste0("q",1:18))
