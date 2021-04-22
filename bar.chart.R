bar.chart <-
  function(data = NULL,
           x = NULL,
           y = NULL,
           z = NULL,           
           x.main.lab = NULL,
           x.text.lab = NULL,
           y.main.lab = NULL,
           z.main.lab  = NULL,
           z.text.lab = NULL,
           alpha = 0.05,
           transformation = function(b) return(b),
           type = c("mean.ci", "median.quan", "mean.sd")[1],
           p.label = NULL,
           # p value labels
           p.algorithm.labels = NULL ,
           # type 2 of p value labels based on letters
           adjust = NULL ,
           colorful = TRUE,
           width = 0.7,
           width.errorbar =0.1,
           width.dodge = 0.8,
           main.title = NULL,
           distance = NULL,
           plot.adjust = 40,
           font = 0,
           ANOVA_table = NULL,
           Repeat.measurment = NULL,
           report.p.algorithm.labeling = TRUE) {
    x.lab   =   x.text.lab
    z.lab   =   z.main.lab
    y.lab   =   y.main.lab
    
    # if(is.null(z) & is.null(x) & is.null(y))
    # if(is.null(x.text.lab)){
    #    levels(data[,x])
    #   x.text.lab = x
    ##############################for  ANOVA_table##################
    
    if (!is.null(ANOVA_table)) {
      if (class(ANOVA_table)[1] != "ANOVA_table")
        stop("The class of ANOVA_table is not correct!")
      
      x  = ANOVA_table$group
      y =  ANOVA_table$deps.quantitative
      if(length(y) > 1) stop("The length of deps.quantitative must be one.")
      
      data = ANOVA_table$data
      test = ANOVA_table$results$Test
      if(is.null(y.lab))
        y.lab = y
      
      
      if(test == "KruskalWallis"){
        type = c("mean.ci", "median.quan", "mean.sd")[2]
        y.lab = paste0(y.lab,"\n[Median (IQR)]")
      } else {
        y.lab = paste0(y.lab,"\n[Mean (95% CI)]")
      }
      
      z    = NULL
      if (report.p.algorithm.labeling)
        p.algorithm.labels = D$.__enclos_env__$private$result.for.plot$label
      
      
    }
    
    ########################### for Repeat.measurment #######
    if (!is.null(Repeat.measurment)) {
      if (class(Repeat.measurment)[1] != "Repeat.measurment")
        stop("The class of Repeat.measurment is not correct!")
      
      x                  =   "Time"
      y                  =   "value"
      data               =   Repeat.measurment$invisible.results$data
      comparison.formula =   Repeat.measurment$invisible.results$comparison.formula
      IV.names = all.vars(comparison.formula)
      IV.names = IV.names[which(IV.names != "Time")]
      if(length(IV.names) > 1) stop("'bar.chart' just compateble with two variable in comparison.formula.")
      z = IV.names
      
      
      if(is.null(y.lab))
        y.lab = y
      
      y.lab = paste0(y.lab,"\n[Mean (95% CI)]")
      
      if (report.p.algorithm.labeling){
        p.algorithm.labels = Repeat.measurment$main.results$letter
        p.algorithm.labels=  c( t(p.algorithm.labels))
      } 
      
    }
    
    
    ###########################   
    
    label <-  p.label
    
    if (!is.null(y)) {
      if (length(x) != 1)
        stop(
          "x can be only one factor(for other type, remove y
          and enter the vector of quantitative varibles by x.)"
        )
      temp.w =  na.omit(data[, c(x, y, z)])
      if (!is.factor(temp.w[[x]]))
        stop("x must be factor")
      
      temp.w.name =  names(temp.w)
      
      
      if (!is.null(z)) {
        names(temp.w) = c("variable", "value", "z.lab")
      } else {
        names(temp.w) = c("variable", "value")
      }
      
      x.name = temp.w.name[1]
      y.name = temp.w.name[2]
      z.name = temp.w.name[3]
      if (is.null(x.main.lab))
        x.main.lab = x.name
      if (is.null(z.lab) & !is.null(z))
        z.lab = z.name
      if (is.null(y.lab))
        y.lab = y.name
      data.melt = temp.w
    }
    
    ##########################################    
    
    if (!type %in%  c("mean.ci", "median.quan", "mean.sd"))
      stop("Type must be  c(\"mean.ci\", \"median.quan\",\"mean.sd\")")
    
    if (length(z)  > 1)
      stop("The length of z must be zero or one.")
    
    if (!is.null(z))
      if (!is.factor(data[[z]]))
        stop("z must be factor")
    
    if (is.null(label))
      label = rep(c(""), length(x.name))
    
    if(is.null(x.text.lab))
      x.text.lab = x
    
    if (is.null(y) & !is.null(z)) {
      
      x.name <- as.character(substitute(x))[-1]
      z.name <- as.character(substitute(z))
      data <- data[, c(x.name, z.name)]
      if (is.null(x.main.lab))
        x.main.lab = x.name
      if (is.null(z.lab))
        z.lab = z.name
      
      names(data) <- c(x.main.lab, z.lab)
      x.name = x.main.lab
      z.name = z.lab
      data.melt <- reshape2::melt(data = data,
                                  id.vars =  z.name,
                                  measure.vars = x.name)
      names(data.melt)[1] <- "z.lab"
    }
    ################################
    
    data.melt$value =  transformation(data.melt$value)
    
    
    
    data.melt <- na.omit(data.melt)
    require(dplyr)
    
    if (type == "mean.ci")
    {
      if (is.null(z.lab))
        summray_data <-  data.melt  %>% group_by(variable) %>%
          summarise(
            ymax = mean(value, na.rm = TRUE) + (qnorm(1 - (alpha / 2)) * sd(value, na.rm = TRUE)) / sqrt(length(na.omit(value))),
            ymin = mean(value, na.rm = TRUE) - (qnorm(1 - (alpha / 2)) * sd(value, na.rm = TRUE)) / sqrt(length(na.omit(value))),
            y = mean(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            .groups = 'drop'
          )
      if (!is.null(z.lab))
        summray_data <-
          data.melt  %>% group_by(variable, z.lab) %>%
          summarise(
            ymax = mean(value, na.rm = TRUE) + (qnorm(1 - (alpha / 2)) * sd(value, na.rm = TRUE)) / sqrt(length(na.omit(value))),
            ymin = mean(value, na.rm = TRUE) - (qnorm(1 - (alpha / 2)) * sd(value, na.rm = TRUE)) / sqrt(length(na.omit(value))),
            y = mean(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            .groups = 'drop'
          )
      
    }
    
    ################################   
    
    if (type == "mean.sd") {
      if (is.null(z.lab))
        summray_data <-  data.melt  %>% group_by(variable) %>%
          summarise(
            ymax = mean(value, na.rm = TRUE) +   sd(value, na.rm = TRUE),
            ymin = mean(value, na.rm = TRUE) -   sd(value, na.rm = TRUE),
            y = mean(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            .groups = 'drop'
          )
      
      if (!is.null(z.lab))
        summray_data <-
          data.melt  %>% group_by(variable, z.lab) %>%
          summarise(
            ymax = mean(value, na.rm = TRUE) +   sd(value, na.rm = TRUE),
            ymin = mean(value, na.rm = TRUE) -   sd(value, na.rm = TRUE),
            y = mean(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            .groups = 'drop'
          )
    }
    ###########################
    
    if (type == "median.quan") {
      if (is.null(z.lab))
        summray_data <-  data.melt  %>% group_by(variable) %>%
          summarise(
            ymax = quantile(value, probs = 0.75, na.rm = TRUE),
            ymin = quantile(value, probs = 0.25, na.rm = TRUE),
            y = median(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            .groups = 'drop'
          )
      if (!is.null(z.lab))
        summray_data <-
          data.melt  %>% group_by(variable, z.lab) %>%
          summarise(
            ymax = quantile(value, probs = 0.75, na.rm = TRUE),
            ymin = quantile(value, probs = 0.25, na.rm = TRUE),
            y = median(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            .groups = 'drop'
          )
    }
    
    
    
    
    m <-  summray_data  %>% group_by(variable) %>%
      summarise(max = max(ymax, na.rm = TRUE), .groups = 'drop')
    height <- m$max
    if (is.null(z))
      height <- max(height)
    if (!is.null(z)) {
      data.melt$z.lab <- as.factor(data.melt$z.lab)
      summray_data$z.lab <- as.factor(summray_data$z.lab)
    }
    
    require(ggplot2)
    require(grid)
    
    
    
    if (!is.null(z.lab)) {
      p = ggplot() +
        stat_summary(
          geom = "bar",
          fun  = "mean",
          mapping = aes(
            x = variable,
            y = value,
            color =  z.lab,
            fill =  z.lab
          ),
          data = data.melt,
          position = position_dodge(width = width.dodge),
          na.rm = TRUE,
          width = width,
          alpha = 1
        ) +
        geom_errorbar(
          data = summray_data,
          mapping = aes(
            ymax = ymax,
            ymin = ymin,
            width = width.errorbar,
            
            y = y,
            x = variable,
            linetype = z.lab ,
          ),
          size = 1.1,
          show.legend = FALSE,
          # color = "black",
          position = position_dodge(width = width.dodge)
        ) +
        theme_classic() +
        theme(
          panel.background = element_blank() ,
          panel.grid = element_blank(),
          axis.line = element_line(size = 1),
          axis.text = element_text(
            size = 12 + font,
            face = "bold",
            color = "black"
          ),
          axis.title  = element_text(
            size = 12 + font,
            face = "bold",
            color = "black"
          ),
          legend.text = element_text(
            size = 12 + font,
            face = "bold",
            color = "black"
          ),
          legend.title = element_text(
            size = 12 + font,
            face = "bold",
            color = "black"
          ),
          plot.title = element_text(
            size = 16 + font,
            face = "bold",
            color = "black"
          )
          
          
        )  +
        labs(
          x =  x.main.lab ,
          y = y.lab,
          color = z.lab,
          fill = z.lab,
          shape = z.lab,
          title =  main.title
        ) +
        scale_y_continuous(#expand = c(0.0, 0.5),
          # breaks = round(seq(
          #     min(data.melt[["value"]], na.rm = TRUE),
          #     max(data.melt[["value"]], na.rm = TRUE),
          #   length.out = 5
          # ) - .009, 2)) +
        )+
        # scale_x_discrete(labels=x.lab)+
        # scale_fill_discrete(labels=z.text.lab)+
        # scale_color_discrete(labels=z.text.lab)+
        scale_linetype_manual(values = rep(1, 8)) +
        guides(color = guide_legend(override.aes = list(size = 5)))
      
      if(!is.null(x.lab))
        p = p + scale_x_discrete(labels=x.lab) 
      
      if (isTRUE(colorful)) 
        if(!is.null(z.text.lab))
          p = p + scale_fill_discrete(labels=z.text.lab)+
            scale_color_discrete(labels=z.text.lab)
        
        if (!isTRUE(colorful)){
          if(!is.null(z.text.lab))
            p =  p +
              scale_colour_grey(start = 0.3 , end = 0.7,labels=z.text.lab ) +
              scale_fill_grey(start = 0.3 , end = 0.7,labels=z.text.lab )
          
          if(is.null(z.text.lab))
            p =  p +
              scale_colour_grey(start = 0.3 , end = 0.7 ) +
              scale_fill_grey(start = 0.3 , end = 0.7)
          
        } 
        
        
        
        
        
        
        base.of.y = ggplot_build(p)$layout$panel_params[[1]]$y.range
        if (is.null(adjust))
          adjust = (abs(base.of.y[2]) + abs(base.of.y[1])) / plot.adjust
        
        if (is.null(distance))
          distance = (abs(base.of.y[2]) + abs(base.of.y[1])) / plot.adjust
        
        xx <- ggplot_build(p)$data[[2]]$x
        xstart = xx [(1:length(xx)) %% 2 == 0]
        xend = xx [(1:length(xx)) %% 2 != 0]
        
        d1 = cbind(
          xstart,
          xend,
          ystart = c(height + adjust + distance),
          yend = c(height + adjust + distance)
        )   %>% as.data.frame()
        d2 = cbind(
          xstart,
          xend,
          ystart = c(height + distance),
          yend = c(height + adjust + distance)
        )   %>% as.data.frame()
        
      }
      
      #########################
      
      
      if (is.null(z.lab)) {
        
        
        p <- ggplot() +
          stat_summary(
            geom = "bar",
            fun  = "mean",
            mapping = aes(x = variable,
                          y = value,
                          # fill =  z.lab
            ),                        
            color =  "black",
            fill =  "black",
            
            data = data.melt,
            position = position_dodge(width = width.dodge),
            na.rm = TRUE,
            width = width,
            alpha = 1
          )  +
          
          geom_errorbar(
            data = summray_data,
            mapping = aes(
              ymax = ymax,
              ymin = ymin,
              width = width.errorbar,
              
              y = y,
              x = variable,
              # linetype = z.lab ,
            ),
            size = 1.1,
            show.legend = FALSE,
            # color = "black",
            position = position_dodge(width = width.dodge)
          ) +
          
          theme_classic() +
          theme(
            panel.background = element_blank() ,
            panel.grid = element_blank(),
            axis.line = element_line(size = 1.25),
            axis.text = element_text(
              size = 12 + font,
              face = "bold",
              color = "black"
            ),
            axis.title  = element_text(
              size = 12 + font,
              face = "bold",
              color = "black"
            ),
            legend.text = element_text(
              size = 12 + font,
              face = "bold",
              color = "black"
            ),
            legend.title = element_text(
              size = 12 + font,
              face = "bold",
              color = "black"
            ),
            plot.title = element_text(
              size = 16 + font,
              face = "bold",
              color = "black"
            )
            
            
          )  +
          labs(x =  x.main.lab ,
               y = y.lab,
               # color = z.lab,
               # fill = z.lab,
               # shape = z.lab,
               title =  main.title) +
          # scale_y_continuous(#expand = c(0.0, 0.5),) +
          scale_linetype_manual(values = rep(1, 8)) +
          scale_x_discrete(labels=x.lab)+
          guides(color = guide_legend(override.aes = list(size = 5)))
        
        
        
        if (!isTRUE(colorful))
          p =  p +
            scale_colour_grey(start = 0.3 , end = 0.7) +
            scale_fill_grey(start = 0.3 , end = 0.7)
        
        
        
        base.of.y = ggplot_build(p)$layout$panel_params[[1]]$y.range
        if (is.null(adjust))
          adjust = (abs(base.of.y[2]) + abs(base.of.y[1])) / plot.adjust
        
        if (is.null(distance))
          distance = (abs(base.of.y[2]) + abs(base.of.y[1])) / plot.adjust
        
        xx <- ggplot_build(p)$data[[2]]$x
        
        
        
      }
      
      
      ########################################
      
      if (!is.null(p.algorithm.labels)) {
        if (length(p.algorithm.labels) != dim(summray_data)[1])
          stop(paste0(
            "The length of p.algorithm.labels is not equall to ",
            dim(summray_data)[1]
          ))
        summray_data$xx  = xx
        summray_data$p.algorithm.labels  = p.algorithm.labels
        summray_data <- summray_data
        
        p = p + geom_text(
          data = summray_data,
          mapping = aes(
            x = xx,
            y =  ymax + 2 * adjust + distance  ,
            label = p.algorithm.labels
          ),
          show.legend = FALSE ,
          color = "black",
          size = 3 + font
        )
        
        
      }
      
      
      
      
      
      
      ######################
      if (!is.null(p.label))
        if (!is.null(z)) {
          if (length(p.label) != dim(d1)[1])
            stop(paste0("The length of p.label is not equall to ", dim(d1)[1]))
          
          p =  p +
            geom_segment(
              data = d1,
              mapping = aes(
                x = xstart,
                xend = xend,
                y = ystart,
                yend = yend
              ),
              show.legend = FALSE,
              size = 1.1,
              color = "black"
            ) +
            geom_segment(
              data = d2,
              mapping = aes(
                x = xstart,
                xend = xstart,
                y = ystart,
                yend = yend
              ),
              show.legend = FALSE,
              size = 1.1,
              color = "black"
            ) +
            geom_segment(
              data = d2,
              mapping = aes(
                x = xend,
                xend = xend,
                y = ystart,
                yend = yend
              ),
              show.legend = FALSE,
              size = 1.1,
              color = "black"
            ) +
            geom_text(
              data = d1,
              mapping = aes(
                x = (xstart + xend) / 2,
                y = height + 3 * adjust + distance ,
                label = label
              ),
              show.legend = FALSE ,
              color = "black",
              size = 3 + font
            )
        }
      p$result <- summray_data
      p
    }
   
    
    


########################################Example##############################

# data = data.frame(
#     x= factor(rbinom(1000,4,0.5)),
#     y= abs(rnorm(1000)) ,
#     x3= abs(rnorm(1000)) ,
#     x2= abs(rnorm(1000)) ,
#     z= factor(rbinom(1000,1,0.5)),
#     z1= factor(0))
# 
# ####################### for one x (factor) and one y(quantitative)
# bar.chart  (data,
#            x =  c("x"),
#            y=  "y",
#            z = NULL,
#            x.main.lab = NULL,
#            x.text.lab = NULL,
#            y.main.lab = NULL,
#            z.main.lab = NULL,
#            z.text.lab = NULL,
#            alpha = 0.05,
#            # transformation = FALSE,
#            type = c("mean.ci", "median.quan","mean.sd")[1],
#            p.label = letters[1:5], # p value labels
#              p.algorithm.labels = letters[1:5] , # type 2 of p value labels based on letters
#            adjust = NULL ,
#            colorful = TRUE,
#            main.title = NULL,
#            distance = NULL,
#            font=0)
# 
# ####################### for multiple x (quantitative) as y and z as factor
# bar.chart  (data,
#             x =  c("x3", "x2", "y"),
#             y=  NULL,
#             z = "z",
#             x.main.lab = NULL,
#             x.text.lab = c("x3", "x2", "y"),
#             y.main.lab = NULL,
#             z.main.lab = NULL,
#             z.text.lab = NULL, 
#             alpha = 0.05,
#             # transformation = FALSE,
#             type = c("mean.ci", "median.quan","mean.sd")[1],
#             p.label = letters[5:7], # p value labels
#             p.algorithm.labels = letters[1:6] , # type 2 of p value labels based on letters
#             adjust = NULL ,
#             colorful = TRUE,
#             main.title = NULL,
#             distance = NULL,
#             font=0)
# # 
# #
# # ####################### for one x (factor), one y (quantitative) and  z as factor
# LETTERS[1:5],
# bar.chart  (data,
#             x =  "x",
#             y=  "y",
#             z = "z",
#             x.main.lab = NULL,
#             x.text.lab = NULL,
#             y.main.lab = NULL,
#             z.main.lab = NULL,
#             z.text.lab = NULL, 
#          
#             alpha = 0.05,
#             # transformation = FALSE,
#             type = c("mean.ci", "median.quan","mean.sd")[1],
#             p.label = letters[1:5], # p value labels
#             p.algorithm.labels = letters[1:10] , # type 2 of p value labels based on letters
#             adjust = NULL ,
#             colorful = FALSE,
#             main.title = NULL,
#             distance = NULL,
#             font=0)
# #
#
#
#
#
#
# ################################## for ANOVA_table
#
# Data  = data.frame(
#   R = 1:1000,
#   Age = abs(rnorm(1000,32,10)),
#   Group = factor(rbinom(1000,3,0.5)+1),
#   Sex = factor(rbinom(1000,1,0.5)),
#   y1 =  (rnorm(1000)),
#   y2 =  (rnorm(1000)) ,
#   y3 = abs(rnorm(1000))
# )
# D<-  ANOVA_table$new(data = Data, group =  "Group",
#                      deps.quantitative = c("y1"))
# 
# bar.chart  (
#   # data,
#   # x =  c("x"),
#   # y=  "y",
#   # z = NULL,
#   x.main.lab = NULL,
#   x.text.lab = NULL,
#   y.main.lab = NULL,
#   z.main.lab = NULL,
#   z.text.lab = NULL, 
#   alpha = 0.05,
#   # transformation = FALSE,
#   type = c("mean.ci", "median.quan","mean.sd")[1],
#   p.label = NULL,   #letters[1:5], # p value labels
#   p.algorithm.labels = NULL,  #letters[1:5] , # type 2 of p value labels based on letters
#   adjust = NULL ,
#   colorful = TRUE,
#   main.title = NULL,
#   distance = NULL,
#   font=0,
#   ANOVA_table = D,
#   report.p.algorithm.labeling = TRUE
# )

# ################################## for Repeat.measurment
#
# Data  = data.frame(
#   R = 1:1000,
#   Age = abs(rnorm(1000,32,10)),
#   Group = factor(rbinom(1000,3,0.5)),
#   Sex = factor(rbinom(1000,1,0.5)),
#   y1 =  (rnorm(1000)),
#   y2 =  (rnorm(1000)) ,
#   y3 = abs(rnorm(1000))
# )
# 
# 
# MM1 =Repeat.measurment (data =Data, formula = cbind(y1, y2, y3) ~(Age+ Sex+ Group)*Time,
#                         ID = "R",
#                         comparison.formula = ~ Group|Time)
# 
#   bar.chart  (
#   # data,
#   # x =  c("x"),
#   # y=  "y",
#   # z = NULL,
#   x.text.lab =  NULL,
#   y.main.lab = "Salam",
#   z.main.lab = "ASD",
#   z.text.lab = letters[1:4],
#   x.main.lab = "Time (h)",
#   alpha = 0.05,
#   # transformation = FALSE,
#   type = c("mean.ci", "median.quan","mean.sd")[3],
#   p.label = NULL,   #letters[1:5], # p value labels
#   p.algorithm.labels = NULL,  #letters[1:5] , # type 2 of p value labels based on letters
#   adjust = NULL ,
#   colorful = FALSE,
#   main.title = NULL,
#   distance = NULL,
#   font=0,
#   ANOVA_table = NULL,
#   Repeat.measurment = MM1,
#   report.p.algorithm.labeling = TRUE
# )
 
