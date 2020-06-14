dot.plot <-
  function(data,
           x,
           z,
           x.lab = NULL,
           y.lab = "Value (95% CI)",
           z.lab  = NULL,
           alpha = 0.05,
           #breaks=ggplot2::waiver(),
           # limits = NULL
           log.transformation = FALSE,
           type = c("mean.ci", "median.quan")[1],
           label = NULL,
           adjust = NULL ,
           colorful = TRUE,
           main.title = NULL) {
    x.name <- as.character(substitute(x))[-1]
    c("mean.ci", "median.quan")
    if (!type %in%  c("mean.ci", "median.quan"))
      stop("Type must be  c(\"mean.ci\", \"median.quan\")")
    if (length(z) != 1)
      stop("The length of z must be one.")
    if (!is.factor(data[[z]]))
      stop("z must be factor")
    if (is.null(label))
      label = rep(c(""), length(x.name))
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
    if (isTRUE(log.transformation)) {
      data.melt$value =  log(data.melt$value)
    }
    
    
    data.melt <- na.omit(data.melt)
    require(dplyr)
    
    if (type == "mean.ci")
      summray_data <-  data.melt  %>% group_by(variable, z.lab) %>%
      summarise(
        ymax = mean(value, na.rm = TRUE) + qnorm(1 - (alpha / 2)) * sd(value, na.rm = TRUE),
        ymin = mean(value, na.rm = TRUE) - qnorm(1 - (alpha / 2)) * sd(value, na.rm = TRUE),
        y = mean(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE)
      )
    
    if (type == "median.quan")
      summray_data <-  data.melt  %>% group_by(variable, z.lab) %>%
      summarise(
        ymax = quantile(value, probs = 0.75, na.rm = TRUE),
        ymin = quantile(value, probs = 0.25, na.rm = TRUE),
        y = median(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE)
      )
    
    m <-  data.melt  %>% group_by(variable) %>%
      summarise(max = max(value, na.rm = TRUE))
    height <- m$max
    
    require(ggplot2)
    #library(ggthemes)
    library(grid)
    #library(ggmap)
    p = ggplot() +
      geom_point(
        mapping = aes(
          x = variable,
          y = value,
          shape = as.factor(z.lab),
          color = as.factor(z.lab),
          fill = as.factor(z.lab)
        ),
        data = data.melt,
        position = position_jitterdodge(),
        na.rm = TRUE,
        size = 2.5,
        alpha = 1
      ) +
      
      
      geom_errorbar(
        data = summray_data,
        mapping = aes(
          ymax = ymax,
          ymin = ymin,
          width = 0.1,
          
          y = y,
          x = variable,
          linetype = as.factor(z.lab) ,
        ),
        size = 1.1,
        show.legend = FALSE,
        # color = "black",
        position = position_dodge(width = .75)
      ) +
      geom_errorbar(
        data = summray_data,
        mapping = aes(
          ymax = y,
          ymin = y,
          width = 0.2,
          y = y,
          x = variable,
          linetype = as.factor(z.lab) ,
          
        ),
        size = 1.1,
        show.legend = FALSE,
        position = position_dodge(width = .75),
        # color = "black"
      ) +
      
      theme_classic() +
      theme(
        panel.background = element_blank() ,
        panel.grid = element_blank(),
        axis.line = element_line(size = 1),
        axis.text = element_text(
          size = 12,
          face = "bold",
          color = "black"
        ),
        axis.title  = element_text(
          size = 14,
          face = "bold",
          color = "black"
        ),
        legend.text = element_text(
          size = 14,
          face = "bold",
          color = "black"
        ),
        legend.title = element_text(
          size = 16,
          face = "bold",
          color = "black"
        ),
        plot.title = element_text(
          size = 17,
          face = "bold",
          color = "black"
        )
        
        
      )  +
      labs(
        x = "",
        y = y.lab,
        color = z.lab,
        fill = z.lab,
        shape = z.lab,
        title =  main.title
      ) +
      scale_y_continuous(expand = c(0.0, 0.1),
                         breaks = round(seq(
                           from = min(data.melt$value, na.rm = TRUE),
                           to = max(data.melt$value, na.rm = TRUE),
                           length.out = 5
                         ) - .009, 2)) +
      scale_linetype_manual(values = rep(1, 8)) +
      guides(color = guide_legend(override.aes = list(size = 5)))
    
    if (!isTRUE(colorful))
      p =  p +
      scale_colour_grey(start = 0.3 , end = 0.7) +
      scale_fill_grey(start = 0.3 , end = 0.7)
    
    
     if(is.null(label) ){
  
    base.of.y = ggplot_build(p)$layout$panel_params[[1]]$y.range
    if (is.null(adjust))
      adjust = (abs(base.of.y[2]) + abs(base.of.y[1])) / 40
    
    xx <- ggplot_build(p)$data[[3]]$x
    xstart = xx [(1:length(xx)) %% 2 == 0]
    xend = xx [(1:length(xx)) %% 2 != 0]
    d1 = cbind(xstart,
               xend,
               ystart = c(height + adjust),
               yend = c(height + adjust))   %>% as.data.frame()
    d2 = cbind(xstart,
               xend,
               ystart = c(height),
               yend = c(height + adjust))   %>% as.data.frame()
    
    p +
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
          y = height + 2 * adjust ,
          label = label
        ),
        show.legend = FALSE ,
        color = "black",
        size = 5
      )
       }
 }

 
  # plot.dot (
  #   rs1,
  #   x = c("nonHDLC_HDLCmgdL", "nonHDLCAREase"),
  #   x.lab = c("nonHDL-C/HDL-C", "nonHDL-C/PON1"),
  #   z = "CAD",
  #   log.transformation = TRUE,
  #   type = "median.quan",
  #   y.lab = "log(Values) \n[Error bars: Median and IQR]",
  #   label = c("P=0.221", "P<0.001"),
  #   colorful = TRUE,
  #   z.lab  = "  ",
  #   main.title = "A)"
  # )
 
