

multinomial = function(data, formula, 
                       ref = NULL,
                       newdata = NULL,
                       # newdata = data.frame(EBV.PCR = 0,
                       #                       Fresh_FFPE = 0
                       #                       , age.Cat =  levels(Data$age.Cat)) 
                       ggplot.mapping = NULL ,
                       #aes(x = age.Cat, y = probability, colour = as.factor(EBV.PCR), group = age.Cat)                        
                       bayesian = FALSE,
                       OR = TRUE
){
  
  # ref: refrence group of DV
  # newdata:   
    
    
    Data = data 
    if(!(exists("%f%", envir = globalenv()) & exists("%+%", envir = globalenv()))){
      source("https://raw.githubusercontent.com/ahadalizadeh/utility_fun/master/utility_fun.R")
    }
    
    require(nnet)
    require(ggplot2)
    require(reshape2)
    
    ################# change ref ---------
    if(!is.null(ref)){
      DV =   all.vars(update.formula(formula, .~1)  )
      Data$DV = Data[[DV]]
      Data[,"DV2"] <- relevel(as.factor(Data$DV), ref = ref)
      formula = update.formula(formula, DV2~.) 
    }
    ################## run model   ---------
    test <- multinom(formula = formula, data = Data)
    z <- summary(test)$coefficients/summary(test)$standard.errors
    p <- (1 - pnorm(abs(z), 0, 1)) * 2
    res= (2 %f% exp(coef(test)))%+% " (" %+%("3.P" %f% p) %+% ")"
    row.names(res) = row.names(p)
    colnames(res) = colnames(p)
    
    ################# new data  ------------
    plot = NULL
    bayesian.plot  = NULL
    pp.write = NULL
    lpp = NULL
    if(!is.null(newdata)){
      
      pp.write <- cbind(newdata, predict(test, newdata = newdata, type = "probs", se = TRUE))
    }
    
    if(!is.null(ggplot.mapping)){
      if(is.null(newdata)){
        stop("ggplot needs to use `newdata` arqument.")}
      
      lpp <- melt(pp.write, id.vars =names(newdata), value.name = "probability")
      plot= ggplot(lpp, mapping = ggplot.mapping) + 
        geom_line() +
        facet_grid(variable ~ ., scales = "free")
      print(plot)
    }
    
    
    co = NULL
    results.mnl = NULL
    summary.mnl = NULL
    if(bayesian == TRUE){
      library(UPG)
      Date.temp =  model.matrix( formula, Data)
      y =  model.frame( formula, Data)[[1]]
      y =  as.factor(y)
      
       # if(is.factor(y)) stop("dependent variable (y) must be factor.")
      X = cbind(1, Date.temp[,-1])
      if (is.null(ref)) {
        tt = table(y)
        ref = names(tt[which.max(tt)])
      }
      pos.bl = which(levels(y) == ref)
      new.lvls = c(levels(y)[-pos.bl], ref)
      y.mnl = factor(y, levels = new.lvls)
      groups = levels(y.mnl)
      
      
      results.mnl <- UPG(y = y, 
                          X = X,
                          model = 'mnl',
                          verbose = TRUE, 
                          baseline = ref)
      summary.mnl =  summary(results.mnl)
      colQuantile_2.5 = function(x) apply(x,c(2,3), quantile, p = 0.025)
      colQuantile_97.5 = function(x) apply(x,c(2,3), quantile, p = 0.975)
      
      coMean = colMeans(  (results.mnl$posterior$beta))
      co2.5 = colQuantile_2.5( (results.mnl$posterior$beta ))
      co97.5 = colQuantile_97.5( (results.mnl$posterior$beta ))
      
      if(OR == TRUE){
      coMean = colMeans( exp(results.mnl$posterior$beta))
      co2.5 = colQuantile_2.5(exp(results.mnl$posterior$beta ))
      co97.5 = colQuantile_97.5(exp(results.mnl$posterior$beta ))
      }
      co= (2 %f% coMean)%+% " (" %+%(2 %f% co2.5) %+% ", " %+% (2 %f% co97.5)%+% ")"
      
      
      rownames(co) = colnames(X)
      colnames(co) = groups
      
      ########################
      
      plot.UPG    = function(x         = NULL,
                             ...,
                             sort      = FALSE,           # sort coefficients by average effect size
                             names     = NULL,            # provide names for variables, alternatively
                             groups    = NULL,            # provide names for groups except baseline
                             xlab      = NULL,            # provide x axis label
                             ylab      = NULL,            # provide y axis label
                             q         = c(0.025, 0.975), # credible intervals
                             include   = NULL,              # which variables to include? default:all (numeric vector)
                             OR        = TRUE
      ){
        
        
        
        if(is.null(include)) include = 2:ncol(x$inputs$X)
        if(is.null(names))   names = colnames(x$inputs$X[,include,drop=F])
        if(is.null(names))   names = paste0("Variable", 1:ncol(x$inputs$X[,include,drop=F]))
        if(length(names) != length(include)) stop("Number of provided variable names does not match number of included variables.")
        
        
        #create some global variables such that r cmd check is happy
        variable = iter = value = NULL
        
        c.point  = apply(x$posterior$beta[,include,,drop=F], c(2,3), mean)
        c.upper  = apply(x$posterior$beta[,include,,drop=F], c(2,3), quantile, q[2])
        c.lower  = apply(x$posterior$beta[,include,,drop=F], c(2,3), quantile, q[1])
        
        
        if(OR == TRUE){
          c.point  = apply(exp(x$posterior$beta[,include,,drop=F]), c(2,3), mean)
          c.upper  = apply(exp(x$posterior$beta[,include,,drop=F]), c(2,3), quantile, q[2])
          c.lower  = apply(exp(x$posterior$beta[,include,,drop=F]), c(2,3), quantile, q[1])
        } 
        
        
        
        if(nrow(c.point) == 1){
          c.upper = matrix(c.upper,nrow=1); 
          c.lower = matrix(c.lower,nrow=1)}
        
        if(is.null(groups)) groups = paste(x$posterior$groups[-ncol(c.point)])
        if(length(groups) != ncol(c.point)-1) stop("Wrong number of group names supplied. Need K-1 names where K is the number of choices.")
        
        
        #kick baseline
        c.upper = c.upper[,-ncol(c.upper),drop=F]
        c.lower = c.lower[,-ncol(c.lower),drop=F]
        c.point = c.point[,-ncol(c.point),drop=F]
        
        #add variable names
        c.upper = data.frame(c.upper, names)
        c.lower = data.frame(c.lower, names)
        c.point = data.frame(c.point, names)
        
        #add group names
        colnames(c.upper) = colnames(c.lower) = colnames(c.point) = c(groups, "names")
        
        #add measurement
        c.upper$measure   = "c.upper"
        c.lower$measure   = "c.lower"
        c.point$measure   = "c.point"
        
        plot.df = rbind(c.upper,c.lower,c.point)
        plot.df = reshape2::melt(plot.df, id.vars = c("names","measure"))
        plot.df = reshape2::dcast(plot.df, "names + variable ~ measure")
        
        #sorting (bit more complicated in MNL, I use average point estimate over all groups)
        if(length(names)>1){
          if(sort){
            
            average       = aggregate(plot.df$c.point, by=list(plot.df$names), FUN=mean)
            lvls          = unique(plot.df$names)
            plot.df$names = factor(plot.df$names, levels = lvls[order(average$x)])
            
            
          }
        }
        
        if(!sort){
          
          # plot in order of appearance in X
          plot.df$names = factor(plot.df$names, levels = rev(names))
          
        }
        
        #axis labeling, take care b/c of coord_flip
        if(is.null(ylab)) ylab = ""
        if(is.null(xlab)) xlab = "Posterior Estimate"
        
        
        final =  ggplot(plot.df, aes(x=names, y=c.point, shape = variable, group = variable)) +
          geom_errorbar(aes(x=names, ymin=c.lower, ymax=c.upper, group = variable),
                        position = position_dodge(width=.5),
                        width=0, col="grey60") +
          geom_point(position = position_dodge(width=.5)) +
          theme_bw()   +
          coord_flip() +
          xlab(ylab)     +
          ylab(xlab) +
          theme(legend.position = "bottom",
                legend.title = element_blank())
        
        if(!isTRUE(OR))  
          final = final + geom_hline(yintercept = 0, col="red",lty=2)  
        
        
        if(isTRUE(OR))  
          final = final +  geom_hline(yintercept = 1, col="red",lty=2) +        
          geom_text(mapping = aes (x = 0.6, y = 1), 
                    label = "OR = 1", color = "red")+ 
          labs(y = "Posterior Estimate of OR \n(95% Credible interval)")
        
        
        return(final)
      }
      
      
      bayesian.plot  = plot.UPG(x=results.mnl, OR = OR)
      
      
    }


#h=multinomial (data = Data, 
 #            formula = Pathology ~ EBV.PCR +
 #              HPV_16.PCR+
  #             HPV_18+
  #             Fresh_FFPE + 
  #             age, 
  #           ref = "Chronic Cervicitis" ,
    #         # newdata =  data.frame(EBV.PCR = c(0:1, 0:1),
  #           #                       Fresh_FFPE = c(0,0,1,1), 
   #          #                    age =  35), 
      #       bayesian = TRUE,
             
  #     # ggplot.mapping = 
  #     #   aes(x = as.factor(Fresh_FFPE), 
  #     #       y = probability,
  #     #       colour = as.factor(EBV.PCR), 
   #    #       group = as.factor(Fresh_FFPE)
   #          # )                        
#)
