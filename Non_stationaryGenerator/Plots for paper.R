#Load historical data downloaded in 2018
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/DataLoad/Loading%20data%20downloaded%20in%202018.R')
#Load Synthetic Precipitation data
source("https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/DataLoad/Loading%20Synthetic%20Precipitation%20Data.R")
#Temperature of NYC
MonthT=MonthT_all %>% filter(Loc=="NYC",Emission=="A2")


in2mm=.254
library(ggplot2)
theme_Result= theme(panel.background = element_rect(fill = "white"),
                    panel.grid.major = element_line(colour = "light grey"),
                    panel.border = element_rect(colour="black",fill=NA),
                    line = element_line(colour = "black", size = 0.5, linetype = 1),
                    axis.text.x = element_text(size=14,colour='black'),
                    axis.text.y = element_text(size=14,colour='black'),
                    axis.title.y = element_text(size = 18),
                    axis.title.x = element_text(size = 18),
                    plot.title = element_text(size = 10),
                    legend.text=element_text(size=18),
                    legend.title=element_text(size=18,face='plain'),
                    legend.position='bottom',
                    legend.direction = "horizontal",
                    strip.text.x = element_text(size=14),
                    strip.text.y = element_text(size=14))

ImgPath="\\\\swv.cae.drexel.edu\\group\\Working Publications\\Ziwen\\Climate Change\\Images/"

# Duration vs EPC --------------
Raw_dt_Evt %>% 
    filter(abs(Sum_Press_Delta)<1000,
           Dur<150) %>% 
    mutate(type=ifelse(Sum_Precip>0,"Rain","Dry")) %>% 
    ggplot(aes(x=Sum_Press_Delta,y=Dur,color=type))+
    geom_density_2d(aes(linetype=type),size=1)+
    scale_color_discrete("",guide=guide_legend(keywidth=2))+
    scale_linetype("",guide=guide_legend(keywidth=2))+
    labs(x="Event Pressure Change (hPa)",y="Duration (hr)")+
    theme_Result+
    theme(legend.position = c(0.25, 0.12),
          legend.key = element_rect( fill = "white"))

ggsave(file=glue(ImgPath,"PCE Dur vs EPC on Rain types.jpg"), width=6,height=4)
    
# Seasonal POP vs AMT, His vs Syn -------
Raw_dt_Evt %>% 
    mutate(MonT=(MonT%/%1)*1,
           PCEtype=ifelse(Sum_Press_Delta>=0,"InPCE","DePCE")) %>% 
    group_by(PCEtype,MonT,Season) %>% 
    summarise(POP=sum(Sum_Precip>0)/n()) %>% 
    mutate(type="Historic",
           runid=NA)->HisPOP_PCE

SyncPress_Perd %>% 
    mutate(MonT=(SynMonT%/%1)*1,
           PCEtype=ifelse(Sum_Press_Delta>=0,"InPCE","DePCE")) %>% 
    group_by(PCEtype,MonT,Season,runid) %>% 
    summarise(POP=sum(Sum_Precip>0)/n()) %>% 
    mutate(type="Synthetic")->SynPOP_PCE


#get the smoothed data values
SynPOP_PCE %>% 
    ggplot(aes(x=(MonT),y=POP,color=PCEtype))+
    geom_smooth(aes(linetype=type),se=F,method="loess")+
    facet_wrap(.~Season) ->plt1 
plt1=ggplot_build(plt1)

SynPOP_PCE %>% 
    group_by(MonT,Season,type,PCEtype) %>% 
    summarise(maxPOP=max(POP,na.rm=T),
              minPOP=min(POP,na.rm=T),
              meanPOP=mean(POP,na.rm=T)) %>% 
    ggplot(aes(x=MonT,color=PCEtype))+
    geom_smooth(aes(y = minPOP),se=F)+
    geom_smooth(aes(y=maxPOP),se=F)+
    facet_wrap(.~Season)->plt
plt=ggplot_build(plt)

HisPOP_PCE %>% 
    ggplot(aes(x=MonT,y=POP,color=PCEtype))+
    geom_point()+
    geom_smooth(se=F)+
    facet_wrap(.~Season)->plt2
plt2=ggplot_build(plt2)

#Plot
full_join(
    #Add smoothed lower bounds of ribbon
    plt$data[[1]] %>% 
        select(x,y,PANEL,group) %>% 
        spread(group,y) %>% 
        rename(DePCE_min=3,InPCE_min=4) ,
    #Add smoothed upper bounds of ribbon
    plt$data[[2]] %>% 
        select(x,y,PANEL,group) %>% 
        spread(group,y) %>% 
        rename(DePCE_max=3,InPCE_max=4) ,
    by=c("x","PANEL")) %>% 
    #add smoothed fit of different PCE type POP
    full_join(plt1$data[[1]] %>% 
                  select(x,y,PANEL,group) %>% 
                  spread(group,y) %>% 
                  rename(DePCE_fit=3,InPCE_fit=4),
              by=c("x","PANEL")) %>%
    mutate(type="Synthetic") %>% 
    #Add Historical data points
    full_join(plt2$data[[1]] %>%
                  select(x,y,PANEL,group) %>%
                  spread(group,y) %>%
                  rename(HisDePCE=3,HisInPCE=4) %>% 
                  mutate(type="Historic points"),
              by=c("x","PANEL","type")) %>%
    #Add Historical fitting lines
    full_join(plt2$data[[2]] %>%
                  select(x,y,PANEL,group) %>%
                  spread(group,y) %>%
                  rename(HisDePCE_fit=3,HisInPCE_fit=4) %>% 
                  mutate(type="Historic lines"),
              by=c("x","PANEL","type")) %>%
    left_join(data.frame(PANEL=factor(1:4),
                         Season=c("Fall (SON)","Spring (MAM)", "Summer (JJA)","Winter (DJF)")),
              by=c("PANEL")) %>% 
    mutate(Season=factor(Season,levels=c("Spring (MAM)", "Summer (JJA)", "Fall (SON)","Winter (DJF)"))) %>%
    ggplot(aes(x=x,group=type))+
    geom_ribbon(aes(ymin=DePCE_min,ymax=DePCE_max),fill="grey80",alpha=0.4)+
    geom_ribbon(aes(ymin=InPCE_min,ymax=InPCE_max),fill="grey80",alpha=0.4)+
    geom_line(aes(y=DePCE_min,color="DePCE"),alpha=0.3)+
    geom_line(aes(y=DePCE_max,color="DePCE"),alpha=0.3)+
    geom_line(aes(y=InPCE_min,color="InPCE"),alpha=0.3)+
    geom_line(aes(y=InPCE_max,color="InPCE"),alpha=0.3)+
    geom_line(aes(y=DePCE_fit,color="DePCE",linetype="Synthetic",size="DePCE"))+
    geom_line(aes(y=InPCE_fit,color="InPCE",linetype="Synthetic",size="InPCE"))+
    geom_point(aes(y=HisDePCE,color="DePCE"),alpha=0.4,show.legend = F)+
    geom_point(aes(y=HisInPCE,color="InPCE"),alpha=0.4,show.legend = F)+
    geom_line(aes(y=HisDePCE_fit,color="DePCE",linetype="Historic",size="DePCE"))+
    geom_line(aes(y=HisInPCE_fit,color="InPCE",linetype="Historic",size="InPCE"))+
    facet_wrap(.~Season)+
    scale_size_manual("",values=c(2,1),guide=guide_legend(keywidth=3))+
    scale_color_discrete("",guide=guide_legend(keywidth=3))+
    scale_linetype("",guide=guide_legend(keywidth=3))+
    labs(x="Average Monthly Temperature (AMT) (°C)",y="POP")+
    scale_y_continuous(labels = scales::percent)+
    theme_Result+
    theme(legend.key=element_blank())

ggsave(file=glue(ImgPath,"Seasonal PCE POP  His vs Syn.jpg"), width=10,height=6)

# PD vs AMT contours Syn vs His------------
library(ggExtra)


Raw_dt_Evt %>% 
    mutate(Sum_Precip=as.numeric(Sum_Precip),
           Sum_Press_Delta=as.numeric(Sum_Press_Delta),
           MonT=as.numeric(MonT),
           PCEType=ifelse(Sum_Press_Delta<0,"DePCE","InPCE"),
           SynHis="Historic") %>% 
    filter(Sum_Precip>0.01) %>% 
    select(Sum_Precip,Sum_Press_Delta,MonT,PCEType,SynHis) %>% 
    bind_rows(
        SyncPress_Perd %>% 
            mutate(MonT=SynMonT,
                   PCEType=ifelse(Sum_Press_Delta<0,"DePCE","InPCE"),
                   SynHis="Synthetic") %>% 
            filter(Sum_Precip>0.01) %>% 
            select(Sum_Precip,Sum_Press_Delta,MonT,PCEType,SynHis)
    ) %>% 
    
    ggplot(aes(x=MonT,y=Sum_Precip,color=PCEType))+
    stat_density_2d(aes(linetype=SynHis,size=SynHis),bins=4)+
    facet_grid(.~PCEType)+
    scale_linetype("")+
    scale_y_log10(breaks=c(0.5,1,5,10,50,100))+
    scale_size_manual("",values=c(0.5,1))+
    scale_color_discrete("")+
    scale_shape("")+
    ylab('Precipitation Depth (PD) (mm) in log scale')+
    xlab(expression(paste("Average Monthly Temperature (AMT) (",degree,"C)")))+
    theme_Result+
    theme(legend.key.width=unit(3,"line"),
          legend.key = element_rect(color="transparent",fill="white"))

ggsave(file=glue(ImgPath,"PD vs AMT on PCE types and His vs Syn.jpg"), width=10,height=6)

# His, Syn POP and PD vs EPC--------------------------------------

require(gridExtra)

SyncPress_Perd %>% 
    mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>%
    mutate(Evt_Press_chng=round(Sum_Press_Delta/20)*20,
           RainEvt=St_Rain>0,
           HisSync=Yr<=2018) %>% 
    group_by(Evt_Press_chng,HisSync) %>% 
    summarise(RainProb=sum(RainEvt,na.rm=TRUE)/n(),
              samplesize=n()) %>% 
    filter(!HisSync) %>% 
    ggplot(aes(x=Evt_Press_chng,y=RainProb))+
    geom_area(size=2,fill="grey")+
    geom_smooth(aes(group=Evt_Press_chng>0,linetype="Synthetic"),se=F)+
    geom_smooth(data=Raw_dt_Evt %>% 
                    mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>%
                    mutate(Evt_Press_chng=round(Sum_Press_Delta/80)*80,
                           RainEvt=St_Rain>0) %>% 
                    group_by(Evt_Press_chng) %>% 
                    summarise(RainProb=sum(RainEvt,na.rm=TRUE)/n()),
                aes(x=Evt_Press_chng,y=RainProb,group=Evt_Press_chng>0,linetype="Historic"),se=F)+
    scale_y_continuous(labels = scales::percent,breaks = c(0,0.5,1))+
    scale_linetype("")+
    ylab('POP')+
    xlab('Event Pressure Change (hPa)')+
    xlim(-1200,1100)+
    theme_Result +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = c(0.7, 0.8),
          legend.key = element_rect(color="transparent",fill="white"))->P_area


SyncPress_Perd %>% 
    mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta),
           Sum_Precip=as.numeric(Sum_Precip)) %>%
    filter(Sum_Precip>0) %>% 
    select(Sum_Press_Delta,Sum_Precip) %>% 
    ggplot(aes(x=Sum_Press_Delta,y=Sum_Precip))+
    stat_density_2d(aes(fill=(..level..*100)),geom='polygon',color="white",bins=6)+
    geom_point(alpha=0.2,size=0.7)+
    stat_smooth(aes(group = 1, linetype = "Synthetic",size="Synthetic"), method = "loess", se = FALSE,color="black")+
    geom_smooth(data=Raw_dt_Evt %>%
                    filter(Sum_Precip>0) %>%
                    select(Sum_Press_Delta,Sum_Precip),
                aes(group = 1, linetype = "Historic",size="Historic"), method = "loess", se = FALSE,color="black")+
    scale_y_log10()+
    scale_linetype_manual("",values=c(1,2))+
    scale_size_manual("",values=c(1,2))+
    scale_fill_gradient("Density %",low=alpha("#CCCCCC", 0.3),high=alpha("#5C5A5A",0.3))+
    guides(fill = guide_colorbar(barwidth = 14),
           linetype=guide_legend(keywidth = 4,nrow=2,byrow=TRUE,override.aes=list(fill="white")),
           size=guide_legend(keywidth = 4,nrow=2,byrow=TRUE,override.aes=list(fill="white")))+
    ylab('Precipitation Depth (PD) (mm) in log scale')+
    xlab('Event Pressure Change (EPC) (hPa)')+
    xlim(-1200,1100)+
    theme_Result-> P_den


gA <- ggplotGrob(P_area)
gB <- ggplotGrob(P_den)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA, gB, layout_matrix = rbind(c(1),c(2),c(2),c(2)))

g <- arrangeGrob(gA, gB, layout_matrix = rbind(c(1),c(2),c(2),c(2))) #generates g
ggsave(file=glue(ImgPath,"PCE vs Rain Prob and Depth.jpg"),g, width=10,height=10)

# temperature observations vs tempreature projection ---------------

MonthT %>% 
    filter(Model=="MIROC",year(Date)>2034) %>% 
    mutate(Season = as.character(cut(
        month(Date),
        c(2, 5, 8, 11),
        c("Spring (MAM)", "Summer (JJA)", "Fall (SON)")
    ))) %>%
    replace_na(list(Season = "Winter (DJF)")) %>% 
    mutate(Season=factor(Season,levels=c("Spring (MAM)", "Summer (JJA)", "Fall (SON)","Winter (DJF)"))) %>% 
    mutate(Year2069=ifelse(year(Date)>2069,"2070~2099","2035~2069"))->MonT_plot

Raw_dt %>% 
    mutate(Season = as.character(cut(
        month(Time),
        c(2, 5, 8, 11),
        c("Spring (MAM)", "Summer (JJA)", "Fall (SON)")
    ))) %>%
    replace_na(list(Season = "Winter (DJF)")) %>% 
    mutate(Season=factor(Season,levels=c("Spring (MAM)", "Summer (JJA)", "Fall (SON)","Winter (DJF)"))) %>% 
    mutate(Temp=as.numeric(Temp)) %>% 
    filter(#Loc=="NYC",
           Temp<999) %>% 
    mutate(Yr=year(Time),Mon=month(Time)) %>% 
    group_by(Yr,Season) %>% 
    summarise(
        MonT=mean(Temp,na.rm=T)) %>% 
    ggplot(aes(x=Season,y=MonT)) +
    geom_boxplot(aes(color="Historic"),outlier.shape = NA)+
    geom_boxplot(data=MonT_plot,aes(x=Season,y=MonT,color=Year2069),alpha=0.5,outlier.shape = NA)+
    labs(x="",y="Average Monthly Temperature (AMT) (°C)")+
    scale_color_manual("",values=c(2,3,1))+
    #scale_linetype("")+
    theme_Result+
    theme(axis.title.x=element_blank())

ggsave(file=glue(ImgPath,"Seasonal Temperature His vs GCM.jpg"), width=6,height=6)

# Seasonal precipitation ---------------------
Raw_dt_Evt %>%
    filter(Press_NA<7
           ,Loc=="NYC"
           ) %>% 
    mutate(Season = as.character(cut(
        month(St),
        c(2, 5, 8, 11),
        c("Spring (MAM)", "Summer (JJA)", "Fall (SON)")
    ))) %>%
    replace_na(list(Season = "Winter (DJF)")) %>% 
    mutate(Season=factor(Season,levels=c("Spring (MAM)", "Summer (JJA)", "Fall (SON)","Winter (DJF)"))) %>% 
    group_by(Season,Yr,Loc) %>% 
    summarise(SnP=sum(as.numeric(Sum_Precip))) %>% 
    ggplot(aes(x=Season,y=SnP))+
    geom_boxplot(aes(color="Historic"),outlier.shape = NA)+
    geom_boxplot(data=SyncPrecip[[3]] %>% filter(Yr>2034,Yr<2100) %>% mutate(Pro_perd=ifelse(Yr>2069,"2070~2099","2035~2069"))
                 ,aes(x=Season,y=SnP,color=Pro_perd),alpha=0.4,width=0.6,outlier.shape = NA)+
    scale_color_manual("",values=c(2,3,1))+
    scale_fill_manual("",guide=F,values =c("#E69F00", "#56B4E9","white") )+
    ylab("Precipitation Depth (mm)")+
    ylim(0,500)+
    theme_Result+
    theme(axis.title.x=element_blank())

ggsave(file=glue(ImgPath,"Seasonal Preciptation His vs GCM.jpg"), width=6,height=6)



# Seasonal PD Diff in Syn  --------------------

SyncPress_Perd %>%  
    filter(Yr>2035) %>% 
    mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta),
           Sum_Precip=as.numeric(Sum_Precip),
           HisSync=ifelse(Yr<=2069,"2035~2069","2070~2099")) %>%
    filter(Sum_Precip>0) %>% 
    select(Sum_Press_Delta,Sum_Precip,HisSync,Season) %>%  
    mutate(Season=factor(Season,levels=c("Spring (MAM)", "Summer (JJA)", "Fall (SON)","Winter (DJF)"))) %>%
    ggplot(aes(x=Sum_Press_Delta,y=Sum_Precip*.254,color=HisSync))+
    stat_density_2d(aes(fill=(..level..*100)),geom='polygon',color="white",bins=5)+
    #geom_point(alpha=0.2,size=0.4)+
    geom_smooth(aes(linetype=HisSync,size=HisSync), method = "loess",  se = FALSE)+
    geom_smooth(data=Raw_dt_Evt %>% 
                    select(Sum_Press_Delta,Sum_Precip,Season) %>% 
                    filter(Sum_Precip>0) %>% 
                    mutate(Season=factor(Season,levels=c("Spring (MAM)", "Summer (JJA)", "Fall (SON)","Winter (DJF)"))),
                aes(linetype="Historic",size="Historic",color="Historic"),method = "loess",se=F)+
    scale_y_log10()+
    scale_fill_gradient("Density %",low=alpha("#CCCCCC", 0.3),high=alpha("#5C5A5A",0.3),breaks=c(0.03,0.06,0.09,0.12,0.14))+
    scale_color_manual("",values=c(2,3,1),guide=guide_legend(override.aes = list(size=3,alpha=0.9)))+
    scale_linetype_manual("",values=c(2,2,1),guide="none")+
    scale_size_manual("",values=c(1,1.5,0.5))+
    facet_wrap(.~Season)+
    guides(color=guide_legend(keywidth = 3,nrow=2,byrow=TRUE,override.aes=list(fill="white")),
           fill = guide_colorbar(barwidth = 18))+
    ylab('Precipitation Depth (PD) (mm) in log scale')+
    xlab('Event Pressure Change (EPC) (hPa)')+
    xlim(-1200,1100)+
    theme_Result+
    theme(strip.text.x = element_text(size=12))

ggsave(file=glue(ImgPath,"Seasonal PCE vs Depth.jpg"), width=12,height=10)


# Rain percentile vs AMT and EPC ------------------

SyncPress_Perd %>%
    filter(year(StTime)>2034) %>% 
    mutate(
        Sum_Press_Delta = as.numeric(Sum_Press_Delta) %/% 30 * 30,
        #EPC_int = as.numeric(EPC_int),
        MonT = as.numeric(SynMonT) %/% 1 * 1,
        Dur = as.numeric(Dur),
        Sum_Precip = as.numeric(Sum_Precip),
        type=ifelse(year(StTime)>2069,"2070~2099","2035~2069")
    ) %>%
    mutate(
        MonT1 = MonT - 1,
        MonT2 = MonT + 1,
        Sum_Press_Delta1 = Sum_Press_Delta - 30,
        Sum_Press_Delta2 = Sum_Press_Delta + 30
    ) %>%
    filter(Sum_Precip > 0) %>%
    select(
        MonT,
        MonT1,
        MonT2,
        Sum_Press_Delta,
        Sum_Press_Delta1,
        Sum_Press_Delta2,
        Season,
        Sum_Precip,
        type
    ) %>%
    gather(
        MonT_C,
        MonT,
        -Sum_Press_Delta,
        -Sum_Press_Delta1,
        -Sum_Press_Delta2,
        -Season,
        -Sum_Precip,
        -type
    ) %>%
    gather(Sum_Press_Delta_C,
           Sum_Press_Delta,
           -MonT_C,
           -MonT,
           -Season,
           -Sum_Precip,
           -type) %>%
    group_by(MonT, Sum_Press_Delta, Season,type) %>%
    summarise(
        `PD Percentile 95%` = quantile(Sum_Precip, probs = 0.95),
        `PD Percentile 75%` = quantile(Sum_Precip, probs = 0.75),
        `PD Percentile 50%` = quantile(Sum_Precip, probs = 0.50)
    ) %>%
    gather(Qntile, Sum_Precip, -MonT, -Sum_Press_Delta, -Season,-type) %>%
    mutate(Sum_Press_C = as.character(cut(
        Sum_Press_Delta,
        c(-2000, -500, 0, 500, 2000),
        c(
            "-2000 hPa ~ -500 hPa",
            "-500 hPa ~ 0 hPa",
            "0 hPa ~ 500 hPa",
            "500 hPa ~ 2000 hPa"
        )
    ))) -> Df4Plot_Syn


Raw_dt_Evt %>%
    filter(Press_NA<7) %>% 
    mutate(Season = as.character(cut(
        month(St),
        c(2, 5, 8, 11),
        c("Spring (MAM)", "Summer (JJA)", "Fall (SON)")
    ))) %>%
    replace_na(list(Season = "Winter (DJF)")) %>%
    mutate(
        Sum_Press_Delta = as.numeric(Sum_Press_Delta) %/% 30 * 30,
        #EPC_int = as.numeric(EPC_int),
        MonT = as.numeric(MonT) %/% 1 * 1,
        Dur = as.numeric(Dur),
        Sum_Precip = as.numeric(Sum_Precip)
    ) %>%
    mutate(
        MonT1 = MonT - 1,
        MonT2 = MonT + 1,
        Sum_Press_Delta1 = Sum_Press_Delta - 30,
        Sum_Press_Delta2 = Sum_Press_Delta + 30
    ) %>%
    filter(Sum_Precip > 0) %>%
    select(
        MonT,
        MonT1,
        MonT2,
        Sum_Press_Delta,
        Sum_Press_Delta1,
        Sum_Press_Delta2,
        Season,
        Sum_Precip
    ) %>%
    gather(
        MonT_C,
        MonT,
        -Sum_Press_Delta,
        -Sum_Press_Delta1,
        -Sum_Press_Delta2,
        -Season,
        -Sum_Precip
    ) %>%
    gather(Sum_Press_Delta_C,
           Sum_Press_Delta,
           -MonT_C,
           -MonT,
           -Season,
           -Sum_Precip) %>%
    group_by(MonT, Sum_Press_Delta, Season) %>%
    summarise(
        `PD Percentile 95%` = quantile(Sum_Precip, probs = 0.95),
        `PD Percentile 75%` = quantile(Sum_Precip, probs = 0.75),
        `PD Percentile 50%` = quantile(Sum_Precip, probs = 0.50)
    ) %>%
    gather(Qntile, Sum_Precip, -MonT, -Sum_Press_Delta, -Season) %>%
    mutate(Sum_Press_C = as.character(cut(
        Sum_Press_Delta,
        c(-2000, -500, 0, 500, 2000),
        c(
            "-2000 hPa ~ -500 hPa",
            "-500 hPa ~ 0 hPa",
            "0 hPa ~ 500 hPa",
            "500 hPa ~ 2000 hPa"
        )
    ))) %>% 
    mutate(type="Historic")-> Df4Plot_His


Df4Plot=rbind(Df4Plot_His,Df4Plot_Syn) %>% 
    mutate(type=factor(type,levels = c("2070~2099","2035~2069","Historic")))

# -2000 hPa ~ -500 hPa category
Df4Plot %>%
    filter(Sum_Press_C=="-2000 hPa ~ -500 hPa") %>% 
{
    df = .
    df %>%
        mutate(Season = "All Season") -> df1
    
    ggplot(data = df, aes(x = MonT, y = Sum_Precip
                          , color = Season)) +
        geom_point(alpha = 0.3, size = 0.7) +
        stat_smooth(se = F, size = 1) +
        geom_smooth(
            data = df1,
            aes(x = MonT, y = Sum_Precip),
            se = F,
            linetype = "longdash"
        ) +
        geom_abline(slope=0.07,intercept = 0,size=0.5,color="darkgray",linetype="dashed")+
        geom_abline(slope=0.14,intercept = -2.5,size=0.5,color="darkgray",linetype="dashed")+
        annotate("text", x = -5, y = 0.5, label = "7%")+
        annotate("text", x = 9, y = 0.2, label = "14%")+
        facet_grid(type ~ Qntile) +
        labs(x = expression(paste(
            "Average Monthly Temperature (AMT) (", degree, "C)"
        )),
        y = "Precipitation Depth (PD) (mm)") +
        #scale_color_discrete("")+
        scale_color_manual(
            "",
            breaks = c(
                "All Season",
                "Fall (SON)",
                "Spring (MAM)",
                "Summer (JJA)",
                "Winter (DJF)"
            ),
            values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00")
        ) +
        ylim(0,150)+
        scale_y_log10()+
        labs(title="EPC -2000 hPa ~ -500 hPa")+
        theme_Result
        
}


ggsave(file=glue(ImgPath,"PD percentiles on -2000 hPa ~ -500 hPa, His vs Syn.jpg"), width=12,height=6)



# 95% PD category
Df4Plot %>% 
    filter(Qntile=="PD Percentile 95%") %>% 
    {
        df = .
        df %>%
            mutate(Season = "All Season") -> df1
        
        ggplot(data = df, aes(x = MonT, y = Sum_Precip
                              , color = Season)) +
            geom_point(alpha = 0.3, size = 0.7) +
            stat_smooth(se = F, size = 1) +
            geom_smooth(
                data = df1,
                aes(x = MonT, y = Sum_Precip),
                se = F,
                linetype = "longdash"
            ) +
            geom_abline(slope=0.07,intercept = 0,size=0.5,color="darkgray",linetype="dashed")+
            geom_abline(slope=0.14,intercept = -2.5,size=0.5,color="darkgray",linetype="dashed")+
            annotate("text", x = -5, y = 0.5, label = "7%")+
            annotate("text", x = 9, y = 0.2, label = "14%")+
            facet_grid(type ~ Sum_Press_C) +
            labs(x = expression(paste(
                "Average Monthly Temperature (AMT) (", degree, "C)"
            )),
            y = "Precipitation Depth (PD) (mm)") +
            #scale_color_discrete("")+
            scale_color_manual(
                "",
                breaks = c(
                    "All Season",
                    "Fall (SON)",
                    "Spring (MAM)",
                    "Summer (JJA)",
                    "Winter (DJF)"
                ),
                values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00")
            ) +
            ylim(0,150)+
            scale_y_log10()+
            labs(title="PD 95% percentile")+
            theme_Result
    }

ggsave(file=glue(ImgPath,"Pressure change ranges on PD 95 percentile, His vs Syn.jpg"), width=12,height=6)


# Hisogram of PCE and Rainy PCE on EPC---------------------
SyncPress_Perd %>% 
    mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>% 
    filter(abs(Sum_Press_Delta)>5.5) %>% 
    mutate(PCEType=ifelse(St_Rain>0,'Rainy PCEs','All PCEs')) %>% 
    ggplot()+
    geom_density(aes(Sum_Press_Delta,linetype=PCEType,fill=PCEType),alpha=0.6, size=1)+
    scale_fill_discrete("")+
    scale_linetype_manual("",values=c("dotted","solid"))+
    labs(x='Event Pressure Change (EPC) (hPa)',y='Density')+
    theme_Result

ggsave(file=glue(ImgPath,"Histogram of PCE and Rain PCE on EPC.jpg"), width=10,height=6)

# Return Period His vs Syn ----------------

#1 hour
Raw_dt %>% 
    filter(Loc=="NYC") %>% 
    group_by(Yr=year(Time)) %>% 
    summarise(Precip=max(Precip)) %>%
    pull(Precip) %>% 
    fevd %>% 
    return.level(return.period = c(2:50)) %>% 
    .[1:49] %>% 
    data.frame %>% 
    rownames_to_column %>% 
    mutate(type="Historic") ->His_rtn

SyncPrecip[[1]] %>% 
    #mutate(type=ifelse(Yr>2069,"2070~2099","2018~2069")) %>% 
    mutate(type="Synthetic") %>% 
    select(HrP_max,runid,type) %>% 
    group_by(runid,type) %>% 
    nest() %>% 
    mutate(models = map(data,function(x) x %>% pull(HrP_max) %>% fevd %>% return.level(return.period = c(2:50)))) %>% 
    mutate(retnP=map(models,function(x) x[1:49] %>% data.frame %>% rownames_to_column)) %>% 
    mutate(retnP=map2(retnP,type,function(x,y) x %>% mutate(type=y))) %>% 
    pull(retnP) %>% 
    bind_rows() -> Syn_rtn

Rtn_1hr=bind_rows(His_rtn,Syn_rtn)
colnames(Rtn_1hr)=c("ReturnP","PD","type")

Rtn_1hr %>%
    filter(PD<500) %>% 
    mutate(ReturnP=as.numeric(ReturnP)) %>% 
    group_by(type,ReturnP) %>% 
    summarise(maxPD=max(PD,na.rm=T),
              meanPD=mean(PD,na.rm=T),
              minPD=min(PD,na.rm=T)) %>% 
    ggplot(aes(x=(ReturnP),color=type))+
    geom_point(aes(y=meanPD))+
    geom_smooth(aes(y=meanPD),se=F)+
    geom_smooth(aes(y=minPD),se=F,linetype="dashed")+
    geom_smooth(aes(y=maxPD),se=F,linetype="dashed")