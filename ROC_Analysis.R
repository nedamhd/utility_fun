ROC_Analysis <- R6::R6Class("ROC_Analysis", lock_objects = FALSE, lock_class = FALSE,
                            public         = list(
                              mainData     = NULL,
                              data        = NULL,
                              obs         = NULL,
                              pred        = NULL,
                              result      = data.frame( ),
                              spliter     = NULL,
                              x.lab       = NULL,
                              spliter.lab = NULL,
                              plot        = NULL,
                              line.size   = NULL,
                              Cut.off.points =   NULL,
                              colorful = NULL,
                              initialize = function(data , obs, pred, 
                                                    pred.lab = NULL, spliter =NULL,
                                                    spliter.lab = NULL, 
                                                    line.size = 1,
                                                    colorful = TRUE) {
                                x.lab = pred.lab
                                self$obs = obs
                                self$line.size = line.size 
                                self$colorful = colorful 
                                self$pred = pred 
                                
                                # omit = function(data, obs, pred, spliter =NULL){
                                #   temp = cbind(FlagID... = seq(1,dim()) ,data)
                                # }
                                self$mainData = data
                                self$data =  (data[,c(obs,pred,spliter)]) #na.omit
                                if(!is.null(x.lab)){self$x.lab <- x.lab} else {self$x.lab <- pred}
                                if(!is.null(spliter)){
                                  self$spliter <- spliter
                                  if(!is.null(spliter.lab)){self$spliter.lab <- spliter.lab} else 
                                  {self$spliter.lab <- levels(self$data[[spliter]])}
                                  
                                }
                                # self$result <- private$myroc.area()
                                self$ROCplot()
                                invisible(self)
                              },
                              add = function(data=NULL , obs=NULL, pred, 
                                             pred.lab = NULL, spliter =NULL,
                                             spliter.lab = NULL) {
                                x.lab <- pred.lab
                                if(is.null(obs)) obs =  self$obs
                                self$obs = obs
                                self$pred = pred
                                if(is.null(data)) data =  self$mainData
                                self$data = (data[,c(obs,pred,spliter)])#na.omit
                                if(!is.null(x.lab)){self$x.lab <- x.lab} else {self$x.lab <- pred}
                                if(!is.null(spliter)){
                                  self$spliter <- spliter
                                  if(!is.null(spliter.lab)){self$spliter.lab <- spliter.lab} else 
                                  {self$spliter.lab <- levels(self$data[[spliter]])
                                 }
                                }else {
                                  self$spliter     <- NULL
                                  self$spliter.lab <- NULL  
                                }
                                # self$result <- private$myroc.area()
                                self$ROCplot()
                                invisible(self)
                              },
                              ROCplot = function(data =  self$data,
                                                 x = self$pred,
                                                 y = self$obs,
                                                 x.lab= self$x.lab, 
                                                 spliter =   self$spliter,
                                                 spliter.lab = self$spliter.lab,
                                                 line.size=self$line.size,
                                                 colorful = self$colorful){
                                require(ggplot2); require(verification);require(plotROC)
                                
                                if(is.null(spliter))  {
                                  g= list(private$gg.Roc(data = data, x = x, y = y, x.lab = x.lab, 
                                                         line.size= line.size, colorful = colorful))
                                  private$cutoff(data = data, obs = y, pred = x,  spliter.label = NA) 
                                  ###########
                                  names(g)<- paste0(x, collapse = " & ")
                                  
                                  
                                }
                                
                                if(!is.null(spliter) && (length(x) >1)) {
                                  data <- split(data, f = as.factor(data[[spliter]]))
                                  s.l <- length(data)
                                  g = list()
                                  for (i in 1:s.l){
                                    g[[i]] <-    private$gg.Roc(data=data[[i]] ,  x = x, y = y, x.lab = x.lab, 
                                                                line.size= line.size,
                                                                colorful = colorful)
                                    private$cutoff(data = data[[i]], obs = y, pred = x,
                                                   spliter.label = paste0(spliter,": ", names(data)[i]) )
                                    
                                    cat("Element ", i," of list output is for ",spliter, ": '", names(data)[i],
                                        "' (See names of elements of the list ).\n")
                                    names(g)[i]<- paste0(spliter," - ", names(data)[i])
                                    
                                  }
                                  
                                  # self<<-self
                                  # self<<-FF
                                  di= dim(self$result)[1]
                                  self$result$spliter <- c(self$result$spliter)
                                  self$result[(di- (s.l*length(x))+1):di, "spliter"]<- rep(names(g),each=length(x))
                                }
                                
                                if( !is.null(spliter) && (length(x)==1) )     {
                                  g= list(private$gg.Roc.1factor(data = data, x = x, y = y,
                                                                 x.lab = x.lab, spliter = spliter,spliter.lab = spliter.lab,
                                                                 line.size= line.size,
                                                                 colorful = colorful))
                                  names(g)<- paste0(x)
                                  
                                  data <- split(data, f = as.factor(data[[spliter]]))
                                  s.l <- length(data)
                                  for (i in 1:s.l){
                                    private$cutoff(data = data[[i]], obs = y, pred = x, 
                                                   spliter.label =  paste0(spliter,": ", names(data)[i])) 
                                  }
                                  
                                }
                                
                                
                                print(g)
                                
                                self$plot <- c(self$plot,g) 
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
                                }} ,
                              wd.Table = function(x= self$result,..., filename=NULL, path = ""){
                                if("RDCOMClient" %in% rownames(installed.packages()) == FALSE)  { 
                                  # Sys.setenv("TAR" = "internal") # if you need it.
                                  # devtools::install_github("omegahat/RDCOMClient")
                                  install.packages('RDCOMClient', repos = 'http://www.omegahat.org/R') }
                                R2wd::wdGet(filename,path , method="RDCOMClient")
                                R2wd::wdBody("\n\n")
                                R2wd::wdTable(as.data.frame(x), ...)
                                cat("Done!\n")
                              },
                              write.cb= function(x = self$result, row.names=TRUE, col.names=TRUE, comment=FALSE, text=NULL, ...){ 
                                datafile <- file("clipboard", open='wt')
                                on.exit(close(datafile))
                                if(comment == TRUE)   {
                                  if(is.null(comment(x))) warning("There is no comment for x! first add one by comment(x) = '...'") else
                                    writeLines(comment(x), con=datafile)}
                                write.table(x, file = datafile, sep = "\t", row.names = row.names,
                                            col.names = col.names, ...)
                                if(!is.null(text))   {writeLines(text , con=datafile)}
                              }
                            ),
                            private = list(
                              myroc.area = function(data = self$data, obs = self$obs, 
                                                    pred = self$pred) {
                                # y, obs:  A binary observation (coded {0, 1 }).
                                # x,  pred:  A probability prediction on the interval [0,1].
                                
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
                                result <- as.data.frame(cbind(
                                  name =  pred  ,
                                  round((do.call(cbind, g)), 4),
                                  "Direction" = di
                                ))
                                # cat("Results saved in `self$result`\n")
                                result
                                
                              }, 
                              cutoff = function(data = NULL, obs, pred, spliter.label = NULL) {
                                obs1 <- c(data[[obs]])
                                n.pred = length(pred)
                                for (pr in 1:n.pred) {
                                  
                                  pred1 <- c(data[[pred[pr]]]) 
                                  # print(spliter.label)
                                  aa1= pROC::roc(response = obs1, predictor = pred1)
                                  a.yu1= pROC::ci.coords(aa1,x= "best",best.method="youden",best.policy="random",boot.n=10000)
                                  a.yu1 = as.data.frame(a.yu1)
                                  cut.points = data.frame(spliter = spliter.label,
                                                          name = pred[pr] ,
                                                          Threshold = paste0(round(a.yu1[1,2],2)," (",round(a.yu1[1,1],3),",",round(a.yu1[1,3],3),")" ),
                                                          Specificity = paste0(round(a.yu1[1,5]*100,2)," (",round(a.yu1[1,4]*100,3),",",round(a.yu1[1,6]*100,3),")" ),
                                                          Sensitivity = paste0(round(a.yu1[1,8]*100,2)," (",round(a.yu1[1,7]*100,3),",",round(a.yu1[1,9]*100,3),")" )
                                  )
                                  self$Cut.off.points<-  rbind(self$Cut.off.points,cut.points)
                                }
                              },
                              gg.Roc.1factor = function(data , x , y , x.lab, spliter,spliter.lab, line.size, colorful ){
                                temp<-  na.omit(data[,c(x,y,spliter)])
                                data<-  temp
                                if(!is.factor(temp[[spliter]])) stop("spliter must be factor.")
                                fe<- levels(temp[[spliter]])
                                fe.n <- length(fe)
                                temp<- split(temp,temp[[spliter]]) 
                                names(temp)
                                data[["variable"]] <- "999"
                                # if(length(x.lab) == fe) cat("Use x.lab for spliter labels.")
                                x.lab2<- c() 
                                x.lab3<- c() 
                                data[["variable2"]]=NA
                                for (i in 1:fe.n) {
                                  self$result <- rbind(self$result, cbind(spliter = paste0(spliter, " - ",  spliter.lab[i]),
                                                                          private$myroc.area (temp[[i]], obs =as.character(y),pred = as.character(x)), stringsAsFactors =FALSE))
                                  a= as.numeric(as.character( private$myroc.area (temp[[i]], obs =as.character(y),pred = as.character(x))[["A"]]))
                                  # dir<<-   private$myroc.area (temp[[i]], obs =as.character(y),pred = as.character(x))[["Direction"]] 
                                  p= as.numeric(as.character( private$myroc.area (temp[[i]], obs=as.character(y), as.character(x))[["p.value"]]))
                                  x.lab2[i] = paste0(spliter.lab[i], "\nAUC = ", round(a,2), ", p = ", round(p,3),"\n")
                                  
                                  if (p < 0.0009999999999){
                                    p <- "p<0.001"
                                    x.lab2[i] =   paste0(spliter.lab[i], "\nAUC = ", round(a,2),", ",   p,"\n")
                                    # data[["variable"]][i] =  i  
                                    
                                  }      
                                    data[["variable2"]][which(as.character(data[[spliter]])== names(temp)[i])] <-  x.lab2[i]
                                  # if (dir == "indirect") data[[x]] <- -1*data[[x]]
                                }
                                  # data[["variable2"]]= factor(  data[["category"]],
                                  #                                levels = levels( data[["category"]]), 
                                  #                                labels=x.lab2)  
                                  cat("The spliter.lab is:\n",spliter.lab, "\nsplite file names are:\n",
                                  names(temp),"\n")
                                  data12<<-data
                                
                                g=    ggplot(data=data, aes(d = .data[[y]], m = .data[[x]],
                                                            color= variable2,
                                                            linetype= variable2)) + 
                                  style_roc(guide = TRUE)+
                                  geom_roc(n.cuts = 0, labels = FALSE, size = line.size)+
                                  geom_abline(slope = 1, intercept = 0, color = "grey",linetype= "dashed")+
                                  labs(colour="",linetype="")+
                                  theme( legend.position = "bottom",
                                         axis.text=element_text(face = "bold",size = 12,colour = "black") ,
                                         legend.text=element_text(face = "bold" ,size = 12,colour = "black") ,
                                         axis.title= element_text(face ="bold" ,size = 16,colour = "black"),
                                         title =element_text(face ="bold" ,size = 14,colour = "black"))+
                                  
                                  theme( legend.key.width = unit(1.25,"cm")) +
                                   scale_linetype_manual(values=c("solid", "dashed",  "dotted","dotdash" ,  "longdash" ,  "twodash"))#
                                
                                if(isFALSE(colorful)){
                                  g =  g +
                                    scale_colour_grey(start = 0.01 , end = 0.01 )  } 
                                return(g)  
                                
                              },
                              gg.Roc = function(data , x , y , x.lab, line.size, colorful ){
                                fe<- length(x) 
                                temp<- data[,c(x,y)]
                                
                                x.lab2<- c()
                                for (i in 1:fe) {
                                  self$result <- rbind(self$result, cbind(spliter = "NO SPLIT",
                                                                          private$myroc.area (data, obs =as.character(y),pred = as.character(x[i])), stringsAsFactors =FALSE))
                                  a= as.numeric(as.character(private$myroc.area (data, obs =as.character(y),pred = as.character(x[i]))[["A"]]))
                                  # dir<<-  private$myroc.area (data, obs =as.character(y),pred = as.character(x[i]))[["Direction"]] 
                                  p= as.numeric(as.character( private$myroc.area (data, obs=as.character(y), as.character(x[i]))[["p.value"]]))
                                  x.lab2[i] = paste0(x.lab[i], "\nAUC = ", round(a,2), ", p = ", round(p,3),"\n")
                                  if (p <0.000999999999999){
                                    p <- "p<0.001"
                                    x.lab2[i] =   paste0(x.lab[i], "\nAUC = ", round(a,2),", ",   p,"\n")
                                  }
                                  # if (dir == "indirect") temp[[x.lab2[i]]]<- -1*temp[[x.lab2[i]]]
                                  #####################################################                     
                                  # private$cutoff(data = data, obs = as.character(y), pred =as.character(x[i])) 
                                }
                                names(temp) <- c(x.lab2, y)
                                temp.melt <- reshape2::melt(temp, id.vars=y ,measure.vars = x.lab2)
                                temp.melt$variable <- as.factor(temp.melt$variable)
                                temp.melt[[y]] <- c(temp.melt[[y]]) 
                                
                                
                                
                                
                                g=  ggplot(data=temp.melt, aes(d = temp.melt[[y]], m = value, 
                                                               color= variable,
                                                               linetype= variable)) + 
                                  style_roc(guide = TRUE)+
                                  geom_roc(n.cuts = 0, labels = FALSE, size = line.size)+
                                  geom_abline(slope = 1, intercept = 0, color = "grey",linetype= "dashed")+
                                  labs(colour="",linetype="")+
                                  theme( legend.position = "bottom",
                                         axis.text=element_text(face = "bold",size = 12,colour = "black") ,
                                         legend.text=element_text(face = "bold" ,size = 12,colour = "black") ,
                                         axis.title= element_text(face ="bold" ,size = 16,colour = "black"),
                                         title =element_text(face ="bold" ,size = 14,colour = "black"))+
                                  theme( legend.key.width = unit(1.25,"cm"))+
                                  scale_linetype_manual(values=c("solid","dashed", "dotted",  "dotdash" ,  "longdash" ,  "twodash"))#
                                if(isFALSE(colorful)){
                                  g =  g +
                                    scale_colour_grey(start = 0.01 , end = 0.01 )  }
                                return(g)  
                                
                              }
                            )
)




# Example

# data = data.frame(TG =rnorm(1000),
#                   LDL = rnorm(1000),
#                   HDL = rnorm(1000),
#                   CAD = rbinom(1000,1,0.5),
#                   Sex = rbinom(1000,1,0.5),
#                   Smoking =rbinom(1000,1,0.5) 
#            )
# 
#  D<-ROC_Analysis$new(data = data,
#                      pred = "TG",
#                      obs = "CAD", 
#                      pred.lab = c("TG Horm"),
#                      spliter = "Sex",
#                      spliter.lab = c("M", "F")
#                      ) 
# D$result
#  
# D$add(data =  data, 
#       pred = c("LDL","HDL") ,
#       obs = "CAD", 
#       spliter = "Smoking",
#       spliter.lab = c("M", "F")
#       )
# D$result
# 
# D$add(data =  data, 
#       pred = c("LDL","HDL") ,
#       obs = "CAD" 
# )
# D$result 
# 
# D$add(data =  data, 
#       pred = c("LDL") ,
#       obs = "CAD"
# )
# D$result 
# 
# D$ggsave(  width = 5, height=5)
# names(D$plot) 
#  
#  
