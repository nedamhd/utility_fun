Ferquency=
function(data, x = NULL, group = NULL){
  require(dplyr)
  
  if(is.null(x)) {
    data2 = data %>% select_if(is.factor)
    x = names(data2)
    if(!is.null(group)){
      x = x[which(group != x)]
      
      data =  data[,c(x, group)]
      
    }
  }
  
  
  .x = data[,x]  
  if(!is.null(group)) .group = data[,group] else .group = rep(1, length(.x))
  percentage = function(.x, digits = 2 ) {
    t.x = table(.x)
    p.t.x = round(prop.table(t.x )*100, digits)
    p.t = data.frame(t(paste0(t.x," (",p.t.x,")")))
    names(p.t) =  names(t.x)
    p.t
  }
   dd= data.frame(t(
    data %>% group_by_at (group )%>%  summarise_at(   x  ,percentage )
  )
  )
  if(dim(dd)[2] == 1)
    names(dd) = "count_percent"
  
  dd
}
