bar =  function(data, x, z = NULL,xlab= NULL, ylab= "Percent", fill.lab= NULL,
                colorful=TRUE, limits=c(0,1), ggsave = FALSE) {
 library(ggplot2) 
  if(is.null(xlab)) xlab = x
   if(is.null(fill.lab)) fill.lab = z
  
  
  xx = data[,x][[1]]
  zz = rep(1, length(xx))
  if(!is.null(z))   zz = data[,z][[1]]
 
  u.x =  as.vector(unique(xx))
  dum = list()
  for (i in 1:length(u.x)) {
    dum[[i]]    <- ifelse(xx == u.x[i], 1, 0)   
  }
  zz2= rep(zz, time=length(u.x))
  xx2= rep(u.x, time=length(xx))
  dum2 = do.call("c", dum)
  D= na.omit(data.frame(zz2, xx2, dum2))

 if(length(unique(zz))==1){
g= ggplot(D, mapping =  aes(x=factor(xx2), y = dum2,  fill=factor(xx2) )) +
  stat_summary(fun.y=mean, geom="bar" , show.legend = FALSE) +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2, show.legend = FALSE) +
  scale_y_continuous(labels=percent_format(), limits=limits) +
  theme_bw()+labs(x = xlab, y =ylab )
} else {
 g=  ggplot(mapping =  aes(x=factor(xx2), y = dum2 , fill = as.factor(zz2))) +
    stat_summary(fun.y=mean, geom="bar", position="dodge" ) +
     stat_summary(fun.data=mean_cl_boot, geom="errorbar",  
                  position=position_dodge( )) +
    scale_y_continuous(labels=percent_format(), limits=limits) +
    theme_bw()+labs(x = xlab, y =ylab, fill= fill.lab)
}
 if (!isTRUE(colorful)){
     g  =  g +
       scale_fill_grey(start = 0.3 , end = 0.7  )
  
}    
 pg <- ggplot_build(g)$data[[2]][,c("x", "group","y","ymin","ymax")]
pg$ci = paste0(round(pg$y*100,2)," (",  round(pg$ymin*100,2),", ",  round(pg$ymax*100,2),")")
pg$x = rep(u.x, each=length(unique(zz)))
pg$group = rep(unique(zz),  length(u.x))

g$results = pg
if(isTRUE(ggsave))
  ggsave(paste0("Plot.",round(runif(1),3),".jpeg"),   width = 5, height = 5,dpi = 600)
g
}


