# source("https://raw.githubusercontent.com/ahadalizadeh/utility_fun/master/bar.chart.R")

bar.chart <-
  function(data = NULL,
           x = NULL,
           y= NULL,
           z = NULL,
           x.lab = NULL,
           x.main.lab = NULL,
           y.lab = NULL,
           z.lab  = NULL,
           alpha = 0.05,
           transformation = function(b) return(b),
           type = c("mean.ci", "median.quan","mean.sd")[1],
           p.label = NULL, # p value labels
           p.algorithm.labels = NULL , # type 2 of p value labels based on letters
           adjust = NULL ,
           colorful = TRUE,
           main.title = NULL,
           distance = NULL,
           font=0,
           ANOVA_table = NULL,
           report.p.algorithm.labeling.for.ANOVA_table = TRUE
           ) {
    
 ##############################for  ANOVA_table##################
   
    if(!is.null(ANOVA_table)){
      if(class(ANOVA_table)[1]!="ANOVA_table")
        stop("The class of ANOVA_table is not correct!")
        
    x  = ANOVA_table$group
    y =  ANOVA_table$deps.quantitative
    data = ANOVA_table$data
    z    = NULL
    if(report.p.algorithm.labeling.for.ANOVA_table)
       p.algorithm.labels = D$.__enclos_env__$private$result.for.plot$label

    
    }
    
###########################    
    
    
    
    
    label <-  p.label
    
    if(!is.null(y)){
      if (length(x) != 1)
        stop("x can be only one factor(for other type, remove y 
             and enter the vector of quantitative varibles by x.)") 
      temp.w =  na.omit(data[,c(x,y,z)])
      if (!is.factor(temp.w[[x]]))
        stop("x must be factor")
      
      temp.w.name =  names(temp.w)
      
      
      if(!is.null(z)){
      names(temp.w) = c("variable", "value", "z.lab")
      } else {
      names(temp.w) = c("variable", "value")
     }
      
      x.name = temp.w.name[1] 
      y.name = temp.w.name[2] 
      z.name = temp.w.name[3] 
      if (is.null(x.lab))
        x.lab = x.name
      if (is.null(z.lab) & !is.null(z))
        z.lab = z.name 
      if (is.null(y.lab))
        y.lab = y.name
      data.melt = temp.w
    }  
    
    if (!type %in%  c("mean.ci", "median.quan", "mean.sd"))
      stop("Type must be  c(\"mean.ci\", \"median.quan\",\"mean.sd\")")
    
    if (length(z)  > 1)
      stop("The length of z must be zero or one.")
    
    if(!is.null(z))
      if (!is.factor(data[[z]]))
        stop("z must be factor")
    
    if (is.null(label))
      label = rep(c(""), length(x.name))
    
    if(is.null(y) & !is.null(z)){
      x.name <- as.character(substitute(x))[-1]
      z.name <- as.character(substitute(z))
      data <- data[, c(x.name, z.name)]
      if (is.null(x.lab))
        x.lab = x.name
      if (is.null(z.lab))
        z.lab = z.name
      
      names(data) <- c(x.lab, z.lab)
      x.name = x.lab
      z.name = z.lab
      data.melt <- reshape2::melt(data = data,
                                  id.vars =  z.name,
                                  measure.vars = x.name)
      names(data.melt)[1] <- "z.lab"
      
    }
    
    data.melt$value =  transformation(data.melt$value)
    
    
    
    data.melt <- na.omit(data.melt)
    require(dplyr)
    
    if (type == "mean.ci")
    {
      if(is.null(z.lab)) 
        summray_data <-  data.melt  %>% group_by(variable) %>%
          summarise(
            ymax = mean(value, na.rm = TRUE) + (qnorm(1 - (alpha / 2)) * sd(value, na.rm = TRUE)) / length(na.omit(value)),
            ymin = mean(value, na.rm = TRUE) - (qnorm(1 - (alpha / 2)) * sd(value, na.rm = TRUE)) / length(na.omit(value)),
            y = mean(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE), .groups = 'drop'
          )
      if(!is.null(z.lab)) 
        summray_data <-  data.melt  %>% group_by(variable, z.lab) %>%
          summarise(
            ymax = mean(value, na.rm = TRUE) + (qnorm(1 - (alpha / 2)) * sd(value, na.rm = TRUE)) / length(na.omit(value)),
            ymin = mean(value, na.rm = TRUE) - (qnorm(1 - (alpha / 2)) * sd(value, na.rm = TRUE)) / length(na.omit(value)),
            y = mean(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE), .groups = 'drop'
          )
      
    }
    
    
    if (type == "mean.sd"){
      if(is.null(z.lab)) 
        summray_data <-  data.melt  %>% group_by(variable) %>%
          summarise(
            ymax = mean(value, na.rm = TRUE) +   sd(value, na.rm = TRUE),
            ymin = mean(value, na.rm = TRUE) -   sd(value, na.rm = TRUE),
            y = mean(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE), .groups = 'drop'
          )
      
      if(!is.null(z.lab)) 
        summray_data <-  data.melt  %>% group_by(variable, z.lab) %>%
          summarise(
            ymax = mean(value, na.rm = TRUE) +   sd(value, na.rm = TRUE),
            ymin = mean(value, na.rm = TRUE) -   sd(value, na.rm = TRUE),
            y = mean(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE), .groups = 'drop'
          )
    }
    
    if (type == "median.quan"){
      if(is.null(z.lab)) 
        summray_data <-  data.melt  %>% group_by(variable) %>%
          summarise(
            ymax = quantile(value, probs = 0.75, na.rm = TRUE),
            ymin = quantile(value, probs = 0.25, na.rm = TRUE),
            y = median(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE), .groups = 'drop'
          )
      if(!is.null(z.lab)) 
        summray_data <-  data.melt  %>% group_by(variable, z.lab) %>%
          summarise(
            ymax = quantile(value, probs = 0.75, na.rm = TRUE),
            ymin = quantile(value, probs = 0.25, na.rm = TRUE),
            y = median(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE), .groups = 'drop'
          )
    }
    
    
    
    
    m <-  summray_data  %>% group_by(variable) %>%
      summarise(max = max(ymax, na.rm = TRUE), .groups = 'drop')
    height <- m$max
    if(is.null(z))
      height <- max(height )
    if(!is.null(z)){
      data.melt$z.lab <- as.factor(data.melt$z.lab)
      summray_data$z.lab <- as.factor(summray_data$z.lab)
    }
    
    require(ggplot2)
    require(grid)
    # if(is.null(z.lab))
  
    
    
    if(!is.null(z.lab)){

    p = ggplot() +
      # geom_point(
      #   mapping = aes(
      #     x = variable,
      #     y = value,
      #     shape = as.factor(z.lab),
      #     color = as.factor(z.lab),
      #     fill = as.factor(z.lab)
      #   ),
      #   data = data.melt,
      #   position = position_jitterdodge(),
      #   na.rm = TRUE,
    #   size = 2.5,
    #   alpha = 1
    # ) +
    stat_summary( geom = "bar", fun  = "mean",
                  mapping = aes(
                    x = variable,
                    y = value,
                    color =  z.lab,  
                    fill =  z.lab 
                  ),
                  data = data.melt,
                  position = position_dodge(width = .4),
                  na.rm = TRUE,
                  width = 0.35,  
                  alpha = 1)+
      
      # geom_bar(
      #     mapping = aes(
      #       x = variable,
      #       y = value,
      #       shape = as.factor(z.lab),
      #       color = as.factor(z.lab),
      #       fill = as.factor(z.lab)
      #     ),
      #     data = data.melt,
      #     position = position_dodge(width = .75),
    #     na.rm = TRUE,
    #     # size = 2.5,
    #     alpha = 1
    #   )+
    geom_errorbar(
      data = summray_data,
      mapping = aes(
        ymax = ymax,
        ymin = ymin,
        width = 0.1,
        
        y = y,
        x = variable,
        linetype = z.lab ,
      ),
      size = 1.1,
      show.legend = FALSE,
      # color = "black",
      position = position_dodge(  width = .4 )
    ) +
      # geom_errorbar(
      #   data = summray_data,
      #   mapping = aes(
      #     ymax = y,
      #     ymin = y,
      #     width = 0.2,
      #     y = y,
      #     x = variable,
      #     linetype = as.factor(z.lab) ,
      #     
      #   ),
    #   size = 1.1,
    #   show.legend = FALSE,
    #   position = position_dodge(width = .75),
    #   # color = "black"
    # ) +
    
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
          size = 14 + font,
          face = "bold",
          color = "black"
        ),
        legend.title = element_text(
          size = 16 + font,
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
        # ) - .009, 2)
      ) +
      scale_linetype_manual(values = rep(1, 8)) +
      guides(color = guide_legend(override.aes = list(size = 5)))
      
    
    
    if (!isTRUE(colorful))
      p =  p +
      scale_colour_grey(start = 0.3 , end = 0.7) +
      scale_fill_grey(start = 0.3 , end = 0.7)
    
    
    
    base.of.y = ggplot_build(p)$layout$panel_params[[1]]$y.range
    if (is.null(adjust))
      adjust = (abs(base.of.y[2]) + abs(base.of.y[1])) / 40
    
    if (is.null(distance))
      distance = (abs(base.of.y[2]) + abs(base.of.y[1])) / 40
    
    xx <- ggplot_build(p)$data[[2]]$x
    xstart = xx [(1:length(xx)) %% 2 == 0]
    xend = xx [(1:length(xx)) %% 2 != 0]
    
    d1 = cbind(xstart,
               xend,
               ystart = c(height + adjust + distance),
               yend = c(height + adjust + distance))   %>% as.data.frame()
    d2 = cbind(xstart,
               xend,
               ystart = c(height + distance),
               yend = c(height + adjust + distance))   %>% as.data.frame()
  
    }    
      
  #########################  
    
    
    
    
    if(is.null(z.lab)){
      p<<- ggplot() +
        stat_summary( geom = "bar", fun  = "mean",
                      mapping = aes(
                        x = variable,
                        y = value,
                        # color =  z.lab,  
                        # fill =  z.lab 
                      ),
                      data = data.melt,
                      position = position_dodge(width = .4),
                      na.rm = TRUE,
                      width = 0.35,  
                      alpha = 1)+
        
        geom_errorbar(
          data = summray_data,
          mapping = aes(
            ymax = ymax,
            ymin = ymin,
            width = 0.1,
            
            y = y,
            x = variable,
            # linetype = z.lab ,
          ),
          size = 1.1,
          show.legend = FALSE,
          # color = "black",
          position = position_dodge(  width = .4 )
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
            size = 14 + font,
            face = "bold",
            color = "black"
          ),
          legend.title = element_text(
            size = 16 + font,
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
          # color = z.lab,
          # fill = z.lab,
          # shape = z.lab,
          title =  main.title
        ) +
        scale_y_continuous(#expand = c(0.0, 0.5),
        ) +
        scale_linetype_manual(values = rep(1, 8)) +
        guides(color = guide_legend(override.aes = list(size = 5)))
  
      
      
      if (!isTRUE(colorful))
        p =  p +
          scale_colour_grey(start = 0.3 , end = 0.7) +
          scale_fill_grey(start = 0.3 , end = 0.7)
      
      
      
      base.of.y = ggplot_build(p)$layout$panel_params[[1]]$y.range
      if (is.null(adjust))
        adjust = (abs(base.of.y[2]) + abs(base.of.y[1])) / 40
      
      if (is.null(distance))
        distance = (abs(base.of.y[2]) + abs(base.of.y[1])) / 40
      
      xx <- ggplot_build(p)$data[[2]]$x  
      
      
      
        } 
    

    ########################################
    
  if(!is.null(p.algorithm.labels)){
    if(length(p.algorithm.labels) != dim(summray_data)[1])
       stop(paste0("The length of p.algorithm.labels is not equall to ", dim(summray_data)[1]))
    summray_data$xx  = xx  
    summray_data$p.algorithm.labels  = p.algorithm.labels  
    summray_data <<- summray_data
 
  p = p + geom_text(
      data = summray_data,
      mapping = aes(
        x = xx,
        y =  ymax + 1 * adjust   ,
        label = p.algorithm.labels 
      ),
      show.legend = FALSE ,
      color = "black",
      size =3 + font
    )   
    
    
  }   
    
    
    
    
    
    
 ######################   
    
    
    
    if(!is.null(p.label)) 
      if(!is.null(z)) {
        
      
      if(length(p.label) != dim(d1)[1])
        stop(paste0("The length of p.label is not equall to ", dim(d1)[1]))
    
      p=  p +
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
          size =3 + font
        )
    }
    p$result <- summray_data
    p
  }
# v1$Group = factor(v1$Group, levels=c(0,1), 
#                   labels = c("Control", "Patient") )
# 
# v1$rs132.Cat = factor(v1$rs132.Cat, levels=c(0,1), 
#                  labels = c("TT", "CT+CC") )
#   
# ( gg= bar.chart (
#     v1,
#     x = c("HOMAIR", "HOMABCF"),
#     x.lab = c("nonHDL-C/HDL-C", "nonHDL-C/PON1"),
#     z = "Group",
#     log.transformation = FALSE,
#     type = "mean.ci",
#     y.lab = "log(Values) \n[Error bars: Median and IQR]",
#     p.label =NULL, #c("P=0.221", "P<0.001"),
#       colorful = FALSE,
#     z.lab  = "  ",
#     main.title = "A)",
#     font = -2,
#      # adjust = 2,
#       # distance = 14,
#   ))
# ggsave("g.jpg",gg, height = 3,width = 5) 
# 
# ( gg= bar.chart (
#   data = v1,
#   x = "rs132.Cat" ,
#   x.main.lab = "rs13266634",
#   y = "HOMAIR",
#   x.lab = c("TT", "CT+CC"),
#   z = "Group",
#   log.transformation = FALSE,
#   type = "median.quan",
#   y.lab = "HOMA-IR \n[Error bars: 95% CI]",
#   p.label = c("P=0.221", "P<0.001"),
#   colorful = FALSE,
#   z.lab  = "  ",
#   main.title = "A)",
#   font = 2,
#   # adjust = 2,
#   # distance = 14,
# )
# )
########################################Example##############################

data = data.frame(
    x= factor(rbinom(1000,4,0.5)),
    y= abs(rnorm(1000)) ,
    x3= abs(rnorm(1000)) ,
    x2= abs(rnorm(1000)) ,
    z= factor(rbinom(1000,1,0.5)),
    z1= factor(0))

####################### for one x (factor) and one y(quantitative)
bar.chart  (data,
           x =  c("x"),
           y=  "y",
           z = NULL,
           x.lab = NULL,
           x.main.lab = NULL,
           y.lab = NULL,
           z.lab  = NULL,
           alpha = 0.05,
           # transformation = FALSE,
           type = c("mean.ci", "median.quan","mean.sd")[1],
           p.label = letters[1:5], # p value labels
             p.algorithm.labels = letters[1:5] , # type 2 of p value labels based on letters
           adjust = NULL ,
           colorful = TRUE,
           main.title = NULL,
           distance = NULL,
           font=0)
  
####################### for multiple x (quantitative) as y and z as factor
bar.chart  (data,
            x =  c("x3", "x2"),
            y=  NULL,
            z = "z",
            x.lab = NULL,
            x.main.lab = NULL,
            y.lab = NULL,
            z.lab  = NULL,
            alpha = 0.05,
            # transformation = FALSE,
            type = c("mean.ci", "median.quan","mean.sd")[1],
            p.label = letters[5:6], # p value labels
            p.algorithm.labels = letters[1:4] , # type 2 of p value labels based on letters
            adjust = NULL ,
            colorful = TRUE,
            main.title = NULL,
            distance = NULL,
            font=0)


####################### for one x (factor), one y (quantitative) and  z as factor
bar.chart  (data,
            x =  "x",
            y=  "y",
            z = "z",
            x.lab = NULL,
            x.main.lab = NULL,
            y.lab = NULL,
            z.lab  = NULL,
            alpha = 0.05,
            # transformation = FALSE,
            type = c("mean.ci", "median.quan","mean.sd")[1],
            p.label = letters[1:5], # p value labels
            p.algorithm.labels = letters[1:10] , # type 2 of p value labels based on letters
            adjust = NULL ,
            colorful = TRUE,
            main.title = NULL,
            distance = NULL,
            font=0)






################################## for ANOVA_table

bar.chart  (
  # data,
  # x =  c("x"),
  # y=  "y",
  # z = NULL,
  x.lab = NULL,
  x.main.lab = NULL,
  y.lab = NULL,
  z.lab  = NULL,
  alpha = 0.05,
  # transformation = FALSE,
  type = c("mean.ci", "median.quan","mean.sd")[2],
  p.label = NULL,   #letters[1:5], # p value labels
  p.algorithm.labels = NULL,  #letters[1:5] , # type 2 of p value labels based on letters
  adjust = NULL ,
  colorful = TRUE,
  main.title = NULL,
  distance = NULL,
  font=0,
  ANOVA_table = D,
  report.p.algorithm.labeling.for.ANOVA_table = TRUE
)
