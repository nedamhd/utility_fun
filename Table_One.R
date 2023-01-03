#TODO: add median and IQR to table for nanparametric variables.

Table_One <-   R6::R6Class(
  "table.One",
  public =  list(
    data                 = NULL,
    group                = NULL,
    deps.quantitative    = NULL,
    deps.qualitative     = NULL,
    wilcox               = FALSE,
    shapiro              = TRUE,
    results              = list(),
    initialize          = function(data, group, 
                                   deps.quantitative=NULL, 
                                   deps.qualitative=NULL, 
                                   wilcox = FALSE, shapiro  = TRUE ) {
      self$data = data 
      self$shapiro = shapiro
      self$group = group
      self$deps.quantitative = deps.quantitative
      self$deps.qualitative = deps.qualitative
      self$wilcox = wilcox
      if(!is.null(deps.quantitative)) self$add.quantitative()
      if(!is.null(deps.qualitative)) self$add.qualitative()
    },
    
    add.quantitative = function(data = NULL, 
                                group = NULL, 
                                deps.quantitative = NULL, 
                                wilcox = NULL, shapiro  = NULL) {
      
      if(is.null(data))               data                = self$data
      if(is.null(group))              group               = self$group
      if(is.null(deps.quantitative))  deps.quantitative   = self$deps.quantitative
      if(is.null(wilcox))             wilcox              = self$wilcox
      if(is.null(shapiro))            shapiro             = self$shapiro
      
      private$ttest(data = data, 
                    group = group, 
                    deps = deps.quantitative, 
                    wilcox = wilcox,  shapiro =  shapiro )
    },
    add.qualitative = function(data = NULL, 
                               group = NULL, 
                               deps.qualitative  = NULL
    ) {
      y = group
      x = deps.qualitative
      if(is.null(data)) data = self$data 
      if(is.null(y))       y = self$group
      if(is.null(x))       x = self$deps.qualitative
      
      for (i in 1:length(x)) {
        private$chisqTest(data, x[i], y)
      }
      cat("Number of ", y,"for ", paste0(names( table(data[,y])), collapse = " and "),"are ", 
          paste0(table(data[,y]), collapse = " and "), "\n")
    } ,
    combine = function() {
      self$results$combined <- 
        as.data.frame(rbind(self$results$quantitative, self$results$qualitative))
    }
    ,
    
    wd.Table =
      function(x= NULL,..., filename=NULL, path = ""){
        if(is.null(x))
          x <- as.data.frame(rbind(self$results$quantitative, self$results$qualitative))
        if("RDCOMClient" %in% rownames(installed.packages()) == FALSE)  { 
          # Sys.setenv("TAR" = "internal") # if you need it.
          # devtools::install_github("omegahat/RDCOMClient")
          install.packages('RDCOMClient', repos = 'http://www.omegahat.org/R') }
        R2wd::wdGet(filename,path , method="RDCOMClient")
        R2wd::wdBody("\n\n")
        R2wd::wdTable(as.data.frame(x), ...)
        cat("Done!\n")
      }
  ),
  private  = list(
    result.for.plot = data.frame(),
    ttest     = function(data, group, deps, wilcox = FALSE, shapiro = TRUE ) {
      if(!(length(deps)== length(wilcox) | length(wilcox) == 1 ))
        stop("length of wilcox must be the same as deps or one!")
      # self$data = data 
      # self$group = group
      # self$deps = deps
      # `self$data` contains the data
      # `self$options` contains the options
      # `self$results` contains the results object (to populate)
      # table <- self$results$Table
      group.value <- c(data[[group]])
      group.level<- sort(unique(group.value))
      if(length(group.level) != 2) stop("level of group must be 2.")
      i = 0
      for (dep in  deps) {
        if(length(deps)== length(wilcox)) {i = i +1} else {i =1}
        d= data[[dep]]
        if(isTRUE(shapiro)){
        G1.shapiro <- shapiro.test(d[which(group.value==group.level[1])])$p.value
        G2.shapiro <- shapiro.test(d[which(group.value==group.level[2])])$p.value
        shapiro    <- (G1.shapiro > 0.05) & (G2.shapiro > 0.05)
        }  
        
        formula <- paste0(dep, "~",  group)
        formula <- as.formula(formula)
        results <- t.test(formula, data =  data)
        test.r <- "t-test" 
        if(isTRUE(wilcox[i]))  {results <- wilcox.test(formula, data =  data); test.r <- "Mann-Whitney U" } 
        if(isTRUE(shapiro))    {results <- wilcox.test(formula, data =  data); test.r <- "Mann-Whitney U" }  
        # self$results$text$setContent(table)
        result.for.plot <- list(data.frame( 
          name =  dep,
          group = group.level,
          rbind(
            private$descriptive(d[group.value==group.level[1]]),
            private$descriptive(d[group.value==group.level[2]]))))
        
        result.for.plot <- append(result.for.plot,
                                  private$result.for.plot
        )
        names(result.for.plot)[1]<- dep
        private$result.for.plot <-result.for.plot
        
        
        if (test.r ==  "t-test"){
        Total = paste0(sprintf("%.2f", round(mean(d,na.rm=TRUE),2)), 
                       ' \u00B1 ',sprintf("%.2f",round(sd(d,na.rm=TRUE),2)) ) 
        
        
        var1 = paste0(sprintf("%.2f", round(mean(d[group.value==group.level[1]],na.rm=TRUE),2)), 
                      ' \u00B1 ',sprintf("%.2f",round(sd(d[group.value==group.level[1]],na.rm=TRUE),2)) ) 
        var2 = paste0(sprintf("%.2f", round(mean(d[group.value==group.level[2]],na.rm=TRUE),2)), 
                      ' \u00B1 ',sprintf("%.2f",round(sd(d[group.value==group.level[2]],na.rm=TRUE),2)) ) 
         
        dep = paste0(dep,"; Mean \u00B1 SD " )
        }
        
        
         if (test.r ==  "Mann-Whitney U"){
            Total = paste0(sprintf("%.2f", round(median(d,na.rm=TRUE),2)), 
                              ' (',
                           sprintf("%.2f",round(quantile(d,p= 0.25, na.rm=TRUE),2)),
                            ", ",
                           sprintf("%.2f",round(quantile(d,p= 0.75, na.rm=TRUE),2)),
                           ")"
                              ) 
            var1 = paste0(sprintf("%.2f", round(median(d[group.value==group.level[1]],na.rm=TRUE),2)), 
                          ' (',
                          sprintf("%.2f",round(quantile(d[group.value==group.level[1]],p= 0.25, na.rm=TRUE),2)),
                          ", ",
                          sprintf("%.2f",round(quantile(d[group.value==group.level[1]],p= 0.75, na.rm=TRUE),2)),
                          ")"
            )

            var2 = paste0(sprintf("%.2f", round(median(d[group.value==group.level[2]],na.rm=TRUE),2)), 
                            ' (',
                            sprintf("%.2f",round(quantile(d[group.value==group.level[2]],p= 0.25, na.rm=TRUE),2)),
                             ", ",
                            sprintf("%.2f",round(quantile(d[group.value==group.level[2]],p= 0.75, na.rm=TRUE),2)),
                            ")"
              )
              
            dep = paste0(dep,"; Median (IQR)" )
             
         }  
        
        
        
        
        values =c(   
          name = dep,
          level = NA,
          Total =  Total,
          var1 =  var1,
          var2 =  var2,
           p = sprintf("%.3f",round(results$p.value,3)),
          test = test.r)
        names(values)[4:5] <- group.level 
        self$results$quantitative <- rbind(self$results$quantitative,values)
        row.names(self$results$quantitative) <- NULL
      }
    },
    chisqTest = function(data=NULL, x, y){
      # y: Column variable.
      # x: row variable.
      deps.qualitative = x
      cat(x," and ", y, " done!\n")
      x <- c(data[[x]])
      y <- c(data[[y]])
      
      
      t <- table(x ,y)
      tt= apply(t,1, sum) 
      p.tt= prop.table(tt) 
      s.tt= paste0(tt," (",round(p.tt*100,2),"%)")
      p.t <- prop.table(t,2)
      s.t <- matrix(paste0(t," (", round(p.t*100,2), "%)"),nrow = dim(t)[1])
      "%f%" <- function(a, b) paste0(sprintf(paste0("%.",b,"f"), a) )              
      s.t <-cbind(s.t, chisq.test(x,y, simulate.p.value = TRUE)$p.value %f% 3)
      row.names(s.t)<-  NULL  
      colnames(s.t)<-  c(colnames(t), "p")
      s.t <- cbind(total=s.tt,s.t )
      s.t <- cbind(name = deps.qualitative, level =  row.names(t) , s.t, test = "Chi-squared")
      
      self$results$qualitative <- rbind(self$results$qualitative,s.t)
    },
    descriptive = function(x, alpha = 0.05) {
      
      n = sum(!is.na(x))
      m = mean(x, na.rm = TRUE)
      s = sd(x, na.rm = TRUE)
      se = s/sqrt(n)
      l.ci =  se*qnorm(1-(alpha/2))
      u.ci =  se*qnorm(1-(alpha/2))
      med =median(x, na.rm = TRUE)
      q1 =quantile(x, p = 0.25, na.rm = TRUE)
      q3 =quantile(x, p = 0.75, na.rm = TRUE)
      
      data.frame(n, m, s, se, l.ci, u.ci, alpha, med, q1 ,q3)
    }
  )
)


# D<-  Table_One$new(data = data, group =  "HbA1c.Cat8", 
#                    deps.qualitative = c("Sex", "Metforminuse","Sulfonylureause",
#                                         "Statinuse"),
#                    deps.quantitative =
#                      c("AIPlogTGHDLC","HOMAIR", 
#                        "PON1activity", "Age","BMI" ))
# # D$deps.quantitative()
# # D$add.qualitative()
# # D$add.qualitative(deps.qualitative = "rs115.Cat")
# D$combine()
# D$wd.Table()
# D$results
