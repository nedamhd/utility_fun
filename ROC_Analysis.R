ROC_Analysis <- R6::R6Class("ROC_Analysis",
                       public = list(
                          data = NULL,
                          obs = NULL,
                          pred = NULL,
                          result = NULL,
                          spliter = NULL,
                          x.lab = NULL,
                          spliter.lab = NULL,
                          plot = NULL,
                          initialize = function(data , obs, pred, 
                                                x.lab = NULL, spliter =NULL,
                                                spliter.lab = NULL) {
                           self$obs = obs
                           self$pred = pred 
                           self$data = na.omit(data[,c(obs,pred,spliter)])
                           if(!is.null(x.lab)){self$x.lab <- x.lab} else {self$x.lab <- pred}
                           if(!is.null(spliter)){
                              self$spliter <- spliter
                              if(!is.null(spliter.lab)){self$spliter.lab <- spliter.lab} else 
                              {self$spliter.lab <- unique(self$data[[spliter]])}
                           } 
                            # self$plot <- ROCplot()
                          },
                          ROCplot = function(data =  self$data,
                                             x = self$pred,
                                             y = self$obs,
                                             x.lab= self$x.lab, 
                                             spliter =   self$spliter,
                                             spliter.lab = self$spliter.lab ){
                            require(ggplot2); require(verification);require(plotROC)
                            
                            if(is.null(spliter))  {
                              g= list(private$gg.Roc(data = data, x = x, y = y, x.lab = x.lab))
                              names(g)<- paste0(x, collapse = " & ")
                            }
                            
                            if(!is.null(spliter) && (length(x) >1)) {
                              data <- split(data, f = as.factor(data[[spliter]]))
                              s.l <- length(data)
                              g = list()
                              for (i in 1:s.l){
                                g[[i]] <-   private$gg.Roc(data[[i]] ,  x = x, y = y, x.lab = x.lab)
                                cat("Element ", i," of list output is for ",spliter, ": '", names(data)[i],
                                    "' (See names of elements of the list ).\n")  
                               names(g)[i]<- paste0(spliter," - ", names(data)[i])
                                }
                            }
                            
                            if( !is.null(spliter) && (length(x)==1) )     {
                              g= list(private$gg.Roc.1factor(data = data, x = x, y = y,
                                                  x.lab = x.lab, spliter = spliter,spliter.lab = spliter.lab))
                            names(g)<- paste0(x, collapse = " & ")
                              }
                            print(g)
                            self$plot <- g 
                          },
                          ggsave = function(filename=NULL,...) {
                            n= length(self$plot)
                            if(!is.null(filename) && (length(filename) == n)){
                             for (i in 1:n) {
                                ggsave(filename[i], plot=self$plot[[i]],...)
                              }
                            } else {
                              filename= paste0(names(self$plot), ".jpg")
                              cat("The length of filename is not ", n, ". We change to ", 
                                  paste0(filename, collapse = " & "),"\n\n")
                              for (i in 1:n) {
                                ggsave(filename[i], plot=self$plot[[i]],...)
                              }
                            }}
                          ),
                        private = list(
                          cutoff = function(data = NULL, obs, pred) {
                            obs1 <- c(data[, as.character(substitute(obs))])
                            pred1 <- c(data[, as.character(substitute(pred))])
                            
                            aa1= pROC::roc(response = obs1, predictor = pred1)
                            a.yu1= pROC::ci.coords(aa1,x= "best",best.method="youden",best.policy="random",boot.n=10000)
                            a.yu1 = as.data.frame(a.yu1)
                            data.frame(name = deparse(substitute(pred)),
                                       Threshold = paste0(round(a.yu1[1,2],2)," (",round(a.yu1[1,1],3),",",round(a.yu1[1,3],3),")" ),
                                       Specificity = paste0(round(a.yu1[1,5]*100,2)," (",round(a.yu1[1,4]*100,3),",",round(a.yu1[1,6]*100,3),")" ),
                                       Sensitivity = paste0(round(a.yu1[1,8]*100,2)," (",round(a.yu1[1,7]*100,3),",",round(a.yu1[1,9]*100,3),")" )
                            )
                          },
                          myroc.area = function(data = self$data, obs = self$obs, 
                                                pred = self$pred) {
                            # obs:  A binary observation (coded {0, 1 } ).
                            # pred:  A probability prediction on the interval [0,1].
                            
                            if (!is.null(data)) {
                              if (!(is.character(obs) &&
                                    is.character(pred)))
                                stop("obs and pred must be charecter!")
                              pred1 <- c(data[[as.character(pred)]])
                              obs1 <- c(data[[as.character(obs)]])
                              
                            } else {
                              obs1  <- obs
                              pred1 <- pred
                            }
                            name <-  pred
                            
                            g = verification::roc.area(obs1, pred1)
                            di <- "direct"
                            if (g$A < 0.5) {
                              g =   verification::roc.area(obs1,-1 * pred1)
                              di <- "indirect"
                            }
                            as.data.frame(cbind(
                              name = deparse(substitute(pred)) ,
                              round((do.call(cbind, g)), 4),
                              "Direction" = di
                            ))
                            
                          },
                          gg.Roc.1factor = function(data , x , y , x.lab, spliter,spliter.lab ){
                            temp<-  na.omit(data[,c(x,y,spliter)])
                            data<-  temp
                            fe<- unique(temp[[spliter]])
                            fe.n <- length(fe)
                            temp<- split(temp,as.factor(temp[[spliter]]) )
                            data[["variable"]] <- "999"
                              # if(length(x.lab) == fe) cat("Use x.lab for spliter labels.")
                            x.lab2<- c() 
                            for (i in 1:fe.n) {
                              a= as.numeric(as.character( private$myroc.area (temp[[i]], obs =as.character(y),pred = as.character(x))[["A"]]))
                              p= as.numeric(as.character( private$myroc.area (temp[[i]], obs=as.character(y), as.character(x))[["p.value"]]))
                              x.lab2[i] = paste0(spliter.lab[i], "\nAUC = ", round(a,2), ", p = ", round(p,3),"\n")
                              
                              if (p == 0){
                                p <- "p<0.001"
                                x.lab2[i] =   paste0(spliter.lab[i], "\nAUC = ", round(a,2),", ",   p,"\n")
                                
                              }      
                              data[["variable"]][which(data[[spliter]]== fe[i])] <-  x.lab2[i]
                              
                            }
                            
                            
                            
                            ggplot(data=data, aes(d = data[[y]], m = data[[x]], linetype= variable)) + 
                              style_roc(guide = TRUE)+
                              geom_roc(n.cuts = 0, labels = FALSE)+
                              geom_abline(slope = 1, intercept = 0, color = "grey",linetype= "dashed")+
                              labs(colour="",linetype="")+
                              theme( legend.position = "bottom",
                                     axis.text=element_text(face = "bold",size = 12,colour = "black") ,
                                     legend.text=element_text(face = "bold" ,size = 12,colour = "black") ,
                                     axis.title= element_text(face ="bold" ,size = 16,colour = "black"))+
                              theme( legend.key.width = unit(1.5,"cm"))+
                              scale_linetype_manual(values=c("solid","dotted", "dashed",  "dotdash" ,  "longdash" ,  "twodash"))#
                          },
                          gg.Roc = function(data , x , y , x.lab ){
                            fe<- length(x) 
                            temp<- data[,c(x,y)]
                            x.lab2<- c()
                            for (i in 1:fe) {
                              a= as.numeric(as.character( private$myroc.area (data, obs =as.character(y),pred = as.character(x[i]))[["A"]]))
                              p= as.numeric(as.character( private$myroc.area (data, obs=as.character(y), as.character(x[i]))[["p.value"]]))
                              x.lab2[i] = paste0(x.lab[i], "\nAUC = ", round(a,2), ", p = ", round(p,3),"\n")
                              if (p == 0){
                                p <- "p<0.001"
                                x.lab2[i] =   paste0(x.lab[i], "\nAUC = ", round(a,2),", ",   p,"\n")
                              }
                            }
                            names(temp) <- c(x.lab2, y)
                            temp.melt <- reshape2::melt(temp, id.vars=y ,measure.vars = x.lab2)
                            temp.melt$variable <- as.factor(temp.melt$variable)
                            temp.melt[[y]] <- c(temp.melt[[y]]) 
                            
                            
                            
                            
                            ggplot(data=temp.melt, aes(d = temp.melt[[y]], m = value, linetype= variable)) + 
                              style_roc(guide = TRUE)+
                              geom_roc(n.cuts = 0, labels = FALSE)+
                              geom_abline(slope = 1, intercept = 0, color = "grey",linetype= "dashed")+
                              labs(colour="",linetype="")+
                              theme( legend.position = "bottom",
                                     axis.text=element_text(face = "bold",size = 12,colour = "black") ,
                                     legend.text=element_text(face = "bold" ,size = 12,colour = "black") ,
                                     axis.title= element_text(face ="bold" ,size = 16,colour = "black"))+
                              theme( legend.key.width = unit(1.5,"cm"))+
                              scale_linetype_manual(values=c("solid","dotted", "dashed",  "dotdash" ,  "longdash" ,  "twodash"))#
                          }
                         )
)

#R6 CLASS for ROC analysis
# pred: quantitative test(S) [c("Age","TGmgdL","LDLCssPOXase")].
# obs: binary gold standard ("CAD").
# obs.lab: label of preds (c("TG", "LDL")).
# spliter: quvalitative variable ("Sex"). 
# spliter.lab: label of spliter levels (c("male", "female")).
# $ROCplot(): create ROC plots and save in self$plot[[i]].
# $ggsave(): save jpg file. don't need file name. 

# Example
# D<-ROC_Analysis$new(data = CAD.data ,pred = "logTGPOXase" ,obs = y, spliter = "Sex" )
# D$ROCplot()
# D$ggsave(  width = 10)
# D$plot[[1]]
#  