

letter.creator<- function(interction.formula,model=MM1){
  formula=interction.formula
  if(!(class(model)=="Manova" | class(model)=="Anova")) 
    stop('The class of object is not "Manova" or "Anova"')
  require(multcompView)                       
  y= all.vars(update(formula, . ~ 1))
  flev= all.vars(update(formula, 1 ~ .))
  inter.term<-attr(terms(formula),"term.labels")
  
  ddd<-multcompLetters(
    model[["Tukeu.tests"]][[y]][[inter.term]][,"p adj"]
  )
  
  
  names( ddd$Letters)
  na=names( ddd$Letters)
  ddd2=do.call(rbind, strsplit( na, ":"))
  colnames(ddd2) <- flev
  ddd2=cbind(G=names( ddd$Letters),ddd2,labels=ddd$Letters)
  ddd2= as.data.frame(ddd2)
  ddd2}

table.m<-function(data,interction.formula, model,type="CI",
                  y.lab=NULL,ancillary.data=NULL,flev=NULL){
  if(is.null(ancillary.data)){
    if(!(class(model)=="Manova" | class(model)=="Anova")) 
      stop('The class of object is not "Manova" or "Anova"')
    
    formula=interction.formula
    y= all.vars(update(formula, . ~ 1))
    flev= all.vars(update(formula, 1 ~ .))
    
    vv1<-summary.me(data,interction.formula=interction.formula)
    vv2<-letter.creator(interction.formula,model=model)
    vv<- merge(vv1,vv2,x.by= flev,y.by= flev)} else { vv<-ancillary.data}
  
  vv<-as.data.frame(vv)
  if(type=="CI")  MSD<-paste0( round(vv[,"Mean"],2), " (",round(vv[,"Mean"]-vv[,"CI"],2),",",
                               round(vv[,"Mean"]+vv[,"CI"],2), ")", vv[,"labels"])
  if(type=="SD") MSD<-paste0( round(vv[,"Mean"],2), "\u00B1",round(vv[,"SD"],3), vv[,"labels"])
  if(type=="SE") MSD<-paste0( round(vv[,"Mean"],2), "\u00B1",round(vv[,"SE"],3), vv[,"labels"])
  
  
  y.lab<-ifelse(is.null(   y.lab),y,y.lab)
  da<-data.frame(cbind(vv[,flev],MSD))
  names(da)[length(flev)+1]<- y.lab
  list(main.table=da, result=vv)
}

summary.me<- function(data,interction.formula){ 
    formula=interction.formula
    y= all.vars(update(formula, . ~ 1))
    flev= all.vars(update(formula, 1 ~ .))
    x<-data[,flev]
    if(length(flev)==1) x<-list(x)
    yy=data[,y]
    se = function(x) sd(x,na.rm=TRUE)/ sqrt(sum(!is.na(x))) 
    summa<- function(x) c(length=sum(!is.na(x)), mean=mean(x,na.rm=TRUE),
                          sd= sd(x,na.rm=TRUE),se=se(x),ci=1.96*se(x))
    summary_data <-  aggregate(yy,   by=(x) ,FUN=  summa)
    if(length(flev)==1){
      names(summary_data)[2] <- "x"
      summary_data<- cbind(level=summary_data$Group.1,summary_data$x)}
    if(length(flev)!=1){
      names(summary_data)[length(flev)+1] <- "x"
      summary_data<- cbind(level=summary_data[,c(flev)],summary_data$x)
    }
    colnames(summary_data)<-      c(flev,"Length","Mean","SD","SE","CI")
    summary_data
  } 
 
my.ggplot.manova<- function(model,
                            type="CI",
                            x,
                            fill = NULL,
                            grid = NULL,
                            grid.labels = NULL,
                            y.lab = NULL,
                            x.lab = NULL,
                            fill.lab = NULL,
                            fill.labels = NULL,
                            upper.dist = NULL,
                            wi.bl = FALSE,
                            erroebar.title = FALSE 
                               ){
  if(class(model)!="Main.Manova") stop('The class of object is not "Main.Manova"')
  summary_data<- model$table$result
  require(ggplot2) 
  y=all.vars(update(model$interaction.formula, . ~ 1))
  
  #labeling
  x.lab= ifelse(!is.null(x.lab), x.lab, x)
  y.lab= ifelse(!is.null(y.lab), y.lab, y)
  if(erroebar.title) y.lab= paste0(y.lab,"\n[Error bars: 95% CI]")
  # fill.lab=ifelse(!is.null(fill.lab),  fill.lab,fill)
  y="Mean" 
  #data
  .x=factor(summary_data[[x]])
  .y=summary_data[[y]]
  # .fill= factor(summary_data[[fill]])
  # if(! is.null(fill.labels)) levels(.fill)<- fill.labels
  # .grid= factor(summary_data[[grid]] )
  #  if(! is.null(grid.labels)) levels(.grid)<- grid.labels
  .type= summary_data[[type]]
  .labels= factor(summary_data[["labels"]])
  .ymax = .y  + .type
  .ymin = .y  - .type
  if(is.null(upper.dist)) upper.dist<-  min(.ymax,na.rm = TRUE )/25
  
  if(is.null(fill)){
    m<-ggplot(summary_data,aes(x=.x, y=.y))+
      stat_summary(
        fun = identity,
        geom="bar",size=1,
        position=position_dodge())+  
      # facet_grid(.~ factor(.grid))+
      geom_errorbar(aes(ymax =.ymax  ,ymin = .ymin ),
                    width=0.1, colour="black",position=position_dodge(width=0.9))+
      theme_bw (base_size=18)+
      geom_text(aes(
        y = .y  + .type+upper.dist, 
        label = .labels),
        position=position_dodge(width=0.9))+
      labs(x=x.lab,y=y.lab)+
      theme(panel.grid = element_blank())
    
    print(m)}
  
  if(!is.null(fill)){
    fill.lab=ifelse(!is.null(fill.lab),  fill.lab,fill)
    .fill= factor(summary_data[[fill]])
    if(! is.null(fill.labels)) levels(.fill)<- fill.labels
    
    g <- ggplot(summary_data,aes(x=.x, y=.y, fill=.fill))+
      stat_summary(aes(fill=.fill),
                   fun = identity,
                   geom="bar",size=1,
                   position=position_dodge())+  
      # facet_grid(.~ factor(.grid))+
      geom_errorbar(aes(#fill=.fill,
        ymax =.ymax  ,
        ymin = .ymin ),
        width=0.1, colour="black",
        position=position_dodge(width=0.9))+
      theme_bw (base_size=18)+
      geom_text(aes(#fill=.fill,
        y = .y  + .type+upper.dist, 
        label = .labels),
        position=position_dodge(width=0.9))+
      labs(x=x.lab,y=y.lab,fill=fill.lab)+
      theme(panel.grid = element_blank())
    
    
    if(is.null(grid)){
      
      g2<- g+scale_fill_grey( start =0)
      ifelse(isTRUE(wi.bl),print(g2),print(g))}
    
    if(!is.null(fill)){
      if(!is.null(grid)){
        .grid= factor(summary_data[[grid]] )
        if(! is.null(grid.labels)) levels(.grid)<- grid.labels
        
        k= g+facet_grid(.~ factor(.grid))
        
        k2<- k+scale_fill_grey( start =0)
        ifelse(isTRUE(wi.bl),print(k2),print(k))
        
      }
    }
    
  }}



  my.ggplot.manova.line <- function(model,
                                 type="CI",
                                 x,
                                 x.lab=NULL,
                                 y.lab=NULL,
                                 fill=NULL,
                                 fill.lab=NULL,
                                 fill.labels=NULL,
                                 shape=NULL,
                                 shape.lab=NULL,
                                 grid=NULL,
                                 grid.labels=NULL,
                                 upper.dist=NULL,
                                 wi.bl=FALSE,
                                 erroebar=FALSE,
                                 erroebar.title=FALSE,
                                 ancillary.data=NULL
                                   ){
  if(is.null(ancillary.data)){
    if(class(model)!="Main.Manova") stop('The class of object is not "Main.Manova"')
    summary_data<- model$table$result} else summary_data<-  ancillary.data
    
    
    
    
    #labeling
    
    y="Mean" 
    #data
    .x=factor(summary_data[[x]])
    .y=summary_data[[y]]
    x.lab=ifelse(!is.null(x.lab),  x.lab,x)
    y.lab= ifelse(!is.null(y.lab),  y.lab,y)
    .type= summary_data[[type]]
    .labels= factor(summary_data[["labels"]])
    .ymax = .y  + .type
    .ymin = .y  - .type
    fill.lab=ifelse(!is.null(fill.lab),  fill.lab,fill)
    .fill = NULL
    if(!is.null(fill)) .fill= summary_data[[fill]] 
    if(! is.null(fill.labels)) levels(.fill)<- fill.labels
    
    if(is.null(upper.dist)) upper.dist<-  min(.ymax,na.rm = TRUE )/25
    
    
    require(ggplot2)
    if(is.null(shape)){
      g5<-ggplot(summary_data,aes(x=factor(.x),  y=.y,colour  =.fill ,
                                  group  =.fill,shape=.fill,linetype=.fill))
      #
      
      gg=g5+
        stat_summary(
          fun = identity, geom="line",size=1)+
        stat_summary(aes( shape  =.fill),
                     fun = identity, geom="point",size=2)+
        theme_bw (base_size=18)+labs(x=x.lab,y=y.lab,shape=fill.lab,
                                     colour  =fill.lab,linetype=fill.lab
        )
      # +
      #   geom_text(aes(group  =.fill,y = .y   + upper.dist,
      #                 label = .labels),show.legend   = FALSE)
      
      
      
      if(isTRUE(erroebar)) { 
        gg=  gg+geom_errorbar(aes(group=.fill,ymax =.ymax  ,ymin = .ymin ))+
          geom_text(aes(group=.fill,y = .y   + .type+upper.dist,
                        label = .labels),show.legend   = FALSE)} else
                          gg=  gg+geom_text(aes(y = .y   + upper.dist,
                                                label = .labels),show.legend   = FALSE,size=5)                  
      
    }
    
    
    
    if(!is.null(shape)){
      g5<-ggplot(summary_data,aes(x=factor(.x),  
                                  y=.y,colour  =(.fill) ,
                                  shape=.shape))#
      
      shape.lab=ifelse(!is.null(shape.lab),  shape.lab,shape)
      .shape= factor(summary_data[[shape]])
      gg=g5+
        stat_summary(aes( group  =interaction(.fill,.shape),
                          colour=as.factor(.fill)),
                     fun.y = identity,
                     geom="line",size=1,
        )+ 
        stat_summary(aes(shape=.shape,colour=as.factor(.fill)
        ),#
        fun.y = identity,
        geom="point",size=3)+
        
        
        geom_errorbar(aes(group=interaction(.fill,.shape),ymax =.ymax  ,ymin = .ymin ),
        )+ #colour="black"  width=0.1
        theme_bw (base_size=18)+
        geom_text(aes(group=interaction(.fill,.shape),
                      y = .y   + .type+upper.dist,
                      label = .labels),show.legend   = FALSE)+
        
        labs(x=x.lab,y=y.lab,fill=fill.lab,shape=shape.lab)
      
    }
    
    if(!is.null(grid)){
      .grid= factor(summary_data[[grid]] )
      if(! is.null(grid.labels)) levels(.grid)<- grid.labels
      gg= gg+facet_grid(.~ factor(.grid))}
    # gg<-gg+ 
    gg }


Anova.fun<- function(data, formula, compare = NULL , by = NULL){ 
  # formula: main model frmula
  # by: condition on it in posthoc analysis
  library(car)
  library(heplots)
  library(broom)
  library(sjstats)
  
  
  M1<- lm( formula,data=data)
  options(contrasts = c("contr.sum", "contr.poly"))
  Anova.M1<- Anova(M1, type="III")  # Type III tests
  s.Anova.M1<- summary(M1)
  # p.Anova.M1<-print(Anova.M1)
  
  conditional_posthoc = NULL
  if(!is.null(by)){
    if(is.null(compare)) stop("Please define `compare`!")
    conts = emmeans::emmeans( M1, as.formula(paste0("pairwise ~",compare))  , by = by) 
    conditional_posthoc= as.data.frame(conts$contrasts) 
  }    
  DV.names=all.vars(update(formula, . ~ 1))
  ID.names<-  attr(terms(formula),"term.labels")        # all.vars(update(formula, 1 ~ .))
  
  formulas<-   paste0(DV.names ,"~",paste0(ID.names, collapse  = "+") )
  M2<- list()
  Anova.M2<-list()
  Tuk.M2<-list()
  for(i in 1:length(DV.names)) M2[[i]]<- aov(as.formula(  formulas [i]),data=data)
  for(i in 1:length(DV.names)) Anova.M2[[i]]<- Anova(M2[[i]], type="III")
  for(i in 1:length(DV.names)) Tuk.M2[[i]]<- TukeyHSD(M2[[i]])
  names(Tuk.M2)<- DV.names
  aa<-list()
  for(i in 1:length(DV.names)){
    Anova.M2[[i]][,"Df.r"]<-Anova.M2[[i]][length(ID.names)+2,2]
    aa[[i]]<-Anova.M2[[i]][1:(length(ID.names)+1),c(2,3,4,5)]
    aa[[i]]<-as.data.frame(aa[[i]])
    aa[[i]][,"F"] <-paste0(round(aa[[i]][,2],2)," (", aa[[i]][,1],"," ,aa[[i]][,4], ")"   )
  }
  
  # main.table<- table.me[,c(3,7)]
  # main.table<- data.frame(round(  main.table[,1] ,2),round(  main.table[,2],3))
  # name.main.table<- c("Pillai test" , "P.value" )
  for(i in 1:length(DV.names)){
    main.table<- data.frame( aa[[i]][,c(5)],"P Value"=  round(aa[[i]][,c(3)],3))
    name.main.table<- c(paste0("F.Value.",DV.names[i]),paste0("P.value.",DV.names[i]))
    names(main.table)<-name.main.table
  }
  row.names(main.table)<-c("(Intercept)",ID.names)
  ######
  xx=list(Anova.Table=main.table,Tukeu.tests=Tuk.M2,
          conditional_posthoc=conditional_posthoc, data = data)
  class(xx)<- "Anova"
  xx}

Manova.fun<- function(data, formula){ 
  # formula: main model frmula
  require(car)
  require(heplots)
  # devtools::install_github("strengejacke/sjstats")
  # devtools::install_github("tidymodels/broom")
  require(broom)
  require(sjstats)
  DV.names=all.vars(update(formula, . ~ 1))
  if(length(DV.names)==1) stop("Number of dependent variables must be more than one (e.g. cbind(y1,y2)~x). ")
  
  options(contrasts = c("contr.sum", "contr.poly"))
  M1<- lm( formula,data=data)
  Anova.M1<- Anova(M1, type="III")  # Type III tests
  # p.Anova.M1<-print(Anova.M1)
  table.me<-etasq(  Anova.M1, anova = TRUE, type="III" )
  ID.names<-  attr(terms(formula),"term.labels")        # all.vars(update(formula, 1 ~ .))
  
  formulas<-   paste0(DV.names ,"~",paste0(ID.names, collapse  = "+") )
  M2<- list()
  Anova.M2<-list()
  Tuk.M2<-list()
  eta<-list()
  for(i in 1:length(DV.names)) M2[[i]]<- aov(as.formula(  formulas [i]),data=data)
  for(i in 1:length(DV.names)) Anova.M2[[i]]<- Anova(M2[[i]], type="III")
  for(i in 1:length(DV.names)) eta[[i]]<-   round(etasq(  M2[[i]], anova = TRUE, type="III" ), 3) 
  
  for(i in 1:length(DV.names)) Tuk.M2[[i]]<- TukeyHSD(M2[[i]])
  names(Tuk.M2)<- DV.names
  aa<-list()
  for(i in 1:length(DV.names)){
    Anova.M2[[i]][,"Df.r"]<-Anova.M2[[i]][length(ID.names)+2,2]
    aa[[i]]<-Anova.M2[[i]][1:(length(ID.names)+1),c(2,3,4,5)]
    aa[[i]]<-as.data.frame(aa[[i]])
    aa[[i]][,"F"] <-paste0(round(aa[[i]][,2],2)," (", aa[[i]][,1],"," ,aa[[i]][,4], ")"   )
  }
  main.table<- table.me [,c("eta^2","Pr(>F)")]
  main.table$F= paste0(round(table.me$`approx F`,2), " (", table.me$`num Df`, ", ", table.me$`den Df`, ")")
  main.table<- data.frame("Pillai test"=round(  main.table[,1] ,3),main.table[,3] ,"P.value"=round(  main.table[,2],3))
  name.main.table<- c("Pillai test" ,"F (df1, df2)", "P.value" )
  for(i in 1:length(DV.names)){
    main.table<- data.frame(main.table,eta[[i]][-dim(eta[[i]])[1],1], aa[[i]][,c(5)],"P Value"=  round(aa[[i]][,c(3)],3))
    name.main.table<- c(name.main.table, paste0("Partial.eta^2.",DV.names[i]), 
                        paste0("F.Value.",DV.names[i]),paste0("P.value.",DV.names[i]))
    names(main.table)<-name.main.table
  }
  row.names(main.table)<-c("(Intercept)",ID.names)
  ######
  xx=list(Manova.Table=main.table,Tukeu.tests=Tuk.M2)
  class(xx)<- "Manova"
  xx}

Main.Manova<- function(formula,data,interaction.formula,type="CI",y.lab=NULL,compare = NULL, by=NULL, ...) { 
  DV.names=all.vars(update(formula, . ~ 1))
if(length(DV.names) == 1) {
   My.Manova<-Anova.fun(formula=formula,data, compare = compare, by = by,...)
}else {
   My.Manova<-Manova.fun(data,formula=formula,...)
}
table<- table.m(data,interction.formula=interaction.formula, model=My.Manova,type=type,y.lab=y.lab)

Main.Manova=list(My.Manova=My.Manova,
                 table=table, 
                 formula=formula, 
                 interaction.formula=interaction.formula, compare = compare, by = by)
class(Main.Manova)<-"Main.Manova"
Main.Manova
}


Data  = data.frame(
  x1 = abs(rnorm(1000)),
  x2 = factor(rbinom(1000,1,0.5)),
  x3 = factor(rbinom(1000,1,0.5)),
  y1 = abs(rnorm(1000)),
  y2 = abs(rnorm(1000)),
  y3 = abs(rnorm(1000))
)
 
# s=Manova.fun(data = Data, formula = cbind( y1,y3, y2) ~ x2*x3   )  
# s=Anova.fun(data = Data, formula = y2 ~ x2*x3   )  
# model=Main.Manova(formula,data = Data,interaction.formula = y3  ~ x2:x3 ,
#                   type="CI",y.lab=NULL,
#                   compare = NULL, by=NULL)
# model$My.Manova$Manova.Table
# 
# my.ggplot.manova(model=model, x="x2", fill = "x3")
# my.ggplot.manova.line(model=model, x="x2", fill = "x3")





