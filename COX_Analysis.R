
####################
Cox_Analysis <- R6::R6Class("Cox_Analysis", lock_objects = FALSE, lock_class = FALSE,
                            public         = list(
                              data        = NULL,
                              n.model     = 1,
                              # main.table  = data.frame( ),
                              result      = list( ),
                              models = list(),
                              PH      = list( ),
                              # plot        = NULL,
                              initialize = function(data, formula){
                                self$data      = data
                                self$family    = family
                                n.model        = 1
                                "%+%" <- function(x,y) paste0(x,y)
                                require(survival)
                                m1 <- coxph(formula = formula, data = data)
                                self$models[[n.model]] <- m1
                                self$PH[[n.model]] <-    cox.zph(m1 )#, transform = "rank"
                                m1.p = summary(m1)$coefficients[,5]
                                m1.ci = summary(m1)$conf.int[,-2] 

                                if(length(m1.p)==1) {
                                Re=data.frame (HR= round(m1.ci[1],round),
                                               CI95= paste0("(",round(m1.ci[2],round),",",round(m1.ci[3],round),")"),
                                               P.value=round(m1.p,round+1))
                                names(Re) =names(Re)%+% " " %+% n.model
                                row.names(Re)= names(m1$coefficients)

                               } else{
                                 Re=data.frame (HR= round(m1.ci[,1],round),
                                                CI95= paste0("(",round(m1.ci[,2],round),",",round(m1.ci[,3],round),")"),
                                                P.value=round(m1.p,round+1))
                                 names(Re) =names(Re)%+% " " %+% n.model
                                 row.names(Re)= names(m1$coefficients)
                                 
                               }
                               self$result$main.table <- Re
                                            
                              },
                              add = function(data = NULL, 
                                             formula, 
                                              m = self$result$main.table, 
                                             n.model = self$n.model + 1 ){
                                if(is.null(data)) data = self$data
                                self$n.model = n.model 
                                "%+%" <- function(x,y) paste0(x,y)
                                
                                require(survival)
                                m1 <- coxph(formula = formula, data = data)
                                self$models[[n.model]] <- m1
                                
                                self$PH[[n.model]] <-  cox.zph(m1 ) #, transform = "rank"
                                m1.ci = summary(m1)$conf.int[,-2]
                                m1.p = summary(m1)$coefficients[,5]
                                if(length(m1.p)==1) {
                                  Re=data.frame (HR= round(m1.ci[1],round),
                                                 CI95= paste0("(",round(m1.ci[2],round),",",round(m1.ci[3],round),")"),
                                                 P.value=round(m1.p,round+1))
                                  names(Re) =names(Re)%+% " " %+% n.model
                                  row.names(Re)= names(m1$coefficients)
                                  
                                } else{
                                  Re=data.frame (HR= round(m1.ci[,1],round),
                                                 CI95= paste0("(",round(m1.ci[,2],round),",",round(m1.ci[,3],round),")"),
                                                 P.value=round(m1.p,round+1))
                                  names(Re) =names(Re)%+% " " %+% n.model
                                  row.names(Re)= names(m1$coefficients)
                                  
                                  
                                }
                                  
                                
                                m = base::merge(m,Re, by="row.names",all=TRUE, suffixes = c("",""), sort=FALSE)
                                row.names(m) <- m$Row.names
                                m$Row.names <- NULL
                                self$result$main.table <- m
                              },
                              wd.Table =
                                function(x= self$result$main.table,..., filename=NULL, path = ""){
                                  caption = "-  Calculated by Cox proportional hazards model."
                                  if(!any(self$sepration))   caption= ""
                                  if("RDCOMClient" %in% rownames(installed.packages()) == FALSE)  { 
                                    # Sys.setenv("TAR" = "internal") # if you need it.
                                    # devtools::install_github("omegahat/RDCOMClient")
                                    install.packages('RDCOMClient', repos = 'http://www.omegahat.org/R') }
                                  R2wd::wdGet(filename,path , method="RDCOMClient")
                                  R2wd::wdBody("\n\n")
                                  R2wd::wdTable(as.data.frame(x),caption = caption, ...)
                                  cat("Done!\n")
                                },
                              write.cb=
                                function(x = self$result$main.table, 
                                         row.names=TRUE, col.names=TRUE, 
                                         comment=FALSE, 
                                         text=NULL, ...){ 
                                  text = " - Calculated by Cox proportional hazards model."
                                  if(!any(self$sepration))   text = ""
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
                            
                            private = list()
                            )

