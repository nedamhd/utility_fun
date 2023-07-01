 
bar =  function(data, x, z = NULL,xlab= NULL, ylab= "Percent", fill.lab= NULL,
                colorful=TRUE, limits=c(0,1), ggsave = FALSE, file.name = NULL,
                width = 5, height = 5,dpi = 600
                ) {
 library(ggplot2)
  library(scales)
  if(is.null(xlab)) xlab = x
   if(is.null(fill.lab)) fill.lab = z
  
  
  xx = data[,x][[1]]
  # xx = as.factor(xx)
    # if(!is.factor(xx)) stop("x must be factor.")
  
  zz = rep(1, length(xx))
  if(!is.null(z))   zz = data[,z][[1]]
 
  # u.x =  as.vector(unique(xx))
  u.x =  as.vector(levels(xx))
  dum = list()
  xx2 = list()
  for (i in 1:length(u.x)) {
    dum[[i]]    <- ifelse(xx == u.x[i], 1, 0) 
    xx2[[i]]    <- rep(u.x[i], length(xx))
  }
  zz2= rep(zz, time=length(u.x))
  xx2= do.call("c", xx2)
  dum2 = do.call("c", dum)
  D= na.omit(data.frame(zz2, xx2, dum2))
 if(length(unique(zz))==1){
   cat("aaa")
   
g= ggplot(D, mapping =  aes(x=factor(xx2), y = as.numeric(dum2),  fill=factor(xx2) )) +
  stat_summary(fun=mean, geom="bar" , show.legend = FALSE) +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2, show.legend = FALSE) +
  scale_y_continuous(labels=percent_format(), limits=limits) +
  theme_bw()+labs(x = xlab, y =ylab )
cat("bbb")

} else {
 g=  ggplot(mapping =  aes(x=factor(xx2), y = dum2 , fill = as.factor(zz2))) +
    stat_summary(fun=mean, geom="bar", position="dodge" ) +
     stat_summary(fun.data=mean_cl_boot, geom="errorbar",  
                  position=position_dodge( )) +
    # scale_y_continuous(labels=percent_format(), limits=limits) +
    theme_bw()+labs(x = xlab, y =ylab, fill= fill.lab)
}
#  if (!isTRUE(colorful)){
#      g  =  g +
#        scale_fill_grey(start = 0.3 , end = 0.7  )
#   
# }    
#  pg <- ggplot_build(g)$data[[2]][,c("x", "group","y","ymin","ymax")]
# pg$ci = paste0(round(pg$y*100,2)," (",  round(pg$ymin*100,2),", ",  round(pg$ymax*100,2),")")
#  pg$x =    ggplot_build(g)$layout$panel_params[[1]]$x$get_labels()
# pg$group = rep(unique(zz),  length(u.x))
#   g$results = pg

# if(isTRUE(ggsave))
#   file = paste0("Plot.",round(runif(1),3),".jpeg")
#   ggsave(ifelse(is.null(file.name) ,file,file.name),   width = width, height = height,
         # dpi = dpi)
  # list(plot=g, res=pg)
  g
}
 
   

