Ferquency  =  function(data, x, group = NULL){
  .x = data[,x]  
  if(!is.null(group)) .group = data[,group] else .group = rep(1, length(.x))
  require(dplyr)
  percentage = function(.x, digits = 2 ) {
    t.x = table(.x)
    p.t.x = round(prop.table(t.x )*100, digits)
    p.t = data.frame(t(paste0(t.x,"(",p.t.x,")")))
    names(p.t) =  names(t.x)
    p.t
  }
  percentage(.x)
  dd= data.frame(t(
    data %>% group_by_at (group )%>%  summarise_at(   x  ,percentage )
  )
  )
  dd
}

cat('
Example:
data = data.frame("sex" = rbinom(1000,1,0.5),
                  "edu"= rbinom(1000,1,0.5), 
                  "job"= rbinom(1000,1,0.5), 
                  "pulp"= rbinom(1000,1,0.5), 
                  "marital"= rbinom(1000,1,0.5))
x = c("sex","edu", "job"  )
group = c("pulp", "marital")
Ferquency  (data, x, group = group) 
')
