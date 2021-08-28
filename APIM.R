
APIM = 
# Actor Partner Interdependence Model
function(data, xOwn, xPartner, yOwn, yPartner, labels = c(xOwn, xPartner, yOwn, yPartner)){

# xOwn  Independent variable: First role
# xPartner  Independent variable: Second role
# yOwn  Dependent variable: First role
# yPartner  Dependent variable: Second role
 
  
data$xOwn = data[[xOwn]]
data$xPartner = data[[xPartner]]
data$yOwn = data[[yOwn]]
data$yPartner = data[[yPartner]]
 
 
APIM_D <- '
                        yOwn  ~  a1*xOwn
                        yPartner  ~  a2*xPartner
                        yOwn  ~  p12*xPartner
                        yPartner  ~  p21*xOwn
                        xOwn  ~  mx1*1
                        xPartner  ~  mx2*1
                        yOwn  ~  my1*1
                        yPartner  ~  my2*1
                        xOwn  ~~ vx1*xOwn
                        xPartner  ~~ vx2*xPartner
                        yOwn  ~~ vy1*yOwn
                        yPartner  ~~ vy2*yPartner
                        xPartner  ~~ cx*xOwn
                        yPartner  ~~ cy*yOwn 
  
   
                        # a_diff := a1 - a2
                        # p_diff := p12 - p21
                        # k1     := p12/a1
                        # k2     := p21/a2
                        # k_diff := k1 - k2
                        # i_diff := my1 - my2
                        # a_ave  := (a1 + a2)/2
                        # p_ave  := (p12 + p21)/2
                        # i_ave  := (my1 +my2)/2
                        # sum1   := (p12 + a1)/2
                        # sum2   := (p21 + a2)/2
                        # cont1  := a1 - p12
                        # cont2  := a2 - p21
                        
'
APIM_D2 <- '
                        yOwn  ~  a1*xOwn
                        yPartner  ~  a2*xPartner
                        yOwn  ~  p12*xPartner
                        yPartner  ~  p21*xOwn
                        xOwn  ~  mx1*1
                        xPartner  ~  mx2*1
                        yOwn  ~  my1*1
                        yPartner  ~  my2*1
                        xOwn  ~~ vx1*xOwn
                        xPartner  ~~ vx2*xPartner
                        yOwn  ~~ vy1*yOwn
                        yPartner  ~~ vy2*yPartner
                        xPartner  ~~ cx*xOwn
                        yPartner  ~~ cy*yOwn 
  
   
                        a_diff := a1 - a2
                        p_diff := p12 - p21
                        k1     := p12/a1
                        k2     := p21/a2
                        k_diff := k1 - k2
                        i_diff := my1 - my2
                        a_ave  := (a1 + a2)/2
                        p_ave  := (p12 + p21)/2
                        i_ave  := (my1 +my2)/2
                        sum1   := (p12 + a1)/2
                        sum2   := (p21 + a2)/2
                        cont1  := a1 - p12
                        cont2  := a2 - p21
                        
'

# Estimate the model 
apimd <-  lavaan::sem(APIM_D,fixed.x=FALSE, 
                      data = data, 
                      missing="fiml")
apimd2 <-  lavaan::sem(APIM_D2,fixed.x=FALSE, 
                      data = data, 
                      missing="fiml")
 

###########################
"%f%" <- function(a, b) {
  if(is.matrix(b)) {d = dim(b); m =1}
  if(is.vector(b)) {d = length(b); m =0}
  b =as.vector(b)
  res = c()
  for(i in 1:length(b))
    res[i]=  paste0(sprintf(paste0("%.",a,"f"), b[i]) )              
  if(m==1) res= as.data.frame(matrix(res, nrow =d[1] ,ncol =d[2] )) 
  return(res) 
}
###########################
library(semPlot)
library(lavaan)
library(dplyr)
table2<-parameterEstimates(apimd,standardized=TRUE)%>%as.data.frame()
table2<-table2[(!table2$rhs== "") & (!table2$rhs== table2$lhs),]
table2$pvalue = 3%f%table2$pvalue 

table2$pvalue[which(!table2$pvalue == "0.000")] =
  paste0("P = ",table2$pvalue[which(!table2$pvalue<0.001)])
table2$pvalue[which(table2$pvalue == "0.000")] = "P < 0.001"  
b<- gettextf(' %.2f (%.2f)\n  %s', 
             table2$est, 
             table2$std.all, 
             table2$pvalue
              )
plot.name = gettextf('%s-%s-%s-%s.tif', 
                xOwn, xPartner, yOwn, yPartner
)
tiff(filename = plot.name, width =657 ,height =322 ,
     res  = 1000)
semPaths(apimd, style = "ram", 
             edge.width = 0.4,
             fade = FALSE, 
             edge.color = "black",
             what = "std", 
             edgeLabels = b,
             layout='tree', 
             rotation = 2, 
             intercepts = FALSE, 
             residuals = FALSE,  
             optimizeLatRes = TRUE, 
             curve = 2,  
             nodeLabels=labels[c(3, 4, 1, 2)],
             sizeMan = 18, 
             sizeMan2 = 8,
             # sizeInt = 10,
             # sizeInt2 = 10, 
            edge.label.position = 
              c(0.5, 0.5, 
                0.3, 0.3,
                0.5, 0.5, 
                0.5, 0.5)  ,
          edge.label.cex=1 
              
)
dev.off()

Estimates   = as.data.frame( parameterEstimates(apimd2, standardized = TRUE))
fit.measures = as.data.frame(fitmeasures(apimd ))
note = gettextf('Note: xOwn: %s; xPartner: %s; yOwn: %s; yPartner: %s', 
                xOwn, xPartner, yOwn, yPartner
                  )

Estimates = Estimates %>% 
  mutate_if(is.numeric, round, digits = 3)
fit.measures = fit.measures %>% 
  mutate_if(is.numeric, round, digits = 3)

list(model = apimd2, estimates = Estimates,  fit.measures = fit.measures, note = note)
}
