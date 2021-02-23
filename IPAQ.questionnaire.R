

IPAQ.questionnaire = function(data, questions){
  
  # Validity of the International Physical Activity Questionnaire
  # questions: name or index of questions:
  # 1 day.intensive
  # 2 minute.intensive
  # 3 day.meduim
  # 4 minute.medium
  # 5 day.walk
  # 6 minute.walk  
  temp = data
  temp= temp [,questions]
  names(temp) = c("day.intensive", "minute.intensive", "day.meduim", 
                  "minute.medium", "day.walk", "minute.walk")
  
  intensive.met = 8
  meduim.met = 4
  walk.met = 3.3
  
  data$IPAQ = with(temp,
                   intensive.met * day.intensive * minute.intensive +
                     meduim.met    * day.meduim    * minute.medium    + 
                     walk.met      * day.walk      * minute.walk)
  data 
}

# J=IPAQ(data = J, questions = c("d.intensive","m.intensive","d.meduim","m.medium",
#                                "d.walk","m.walk" ))