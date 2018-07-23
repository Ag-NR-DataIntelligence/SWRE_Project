

#Precip Evt Separation function for 5 min interval

Precip_Evt_Sep= function(dt,IntE_P=4)
    #dt:       data of time and rain
    #IntE_P:   Inter event period 
    #           (time step based your time interval
    #            For example: in a 5 min time interval series
    #            a 4 hour inter event period is corresponding to
    #            48 for IntE_P)
{
    #The header of time and rain should be
    # Time    Precip
    
    dt %>% 
        #select(Time,Precip) %>% 
        replace_na(list(Precip=0)) %>% 
        mutate(Cum_Precip_4hr_L=roll_sum(Precip,IntE_P+1,align='left',fill=0)-Precip,
               Cum_Precip_4hr_R=roll_sum(Precip,IntE_P+1,align='right',fill=0)-Precip) %>% 
        #Start of Rain
        mutate(StR=ifelse((Cum_Precip_4hr_R==0 & Precip>0),1,0)) %>% 
        #Start of Dry
        mutate(StD=ifelse(lag(Cum_Precip_4hr_L)==0 & lag(Precip)>0,1,0)) %>% 
        replace_na(list(StR=0,StD=0)) %>% 
        mutate(Evt_lab=StR+StD) %>% 
        mutate(Evt_lab=cumsum(Evt_lab)) %>% 
        select(-Cum_Precip_4hr_L,-Cum_Precip_4hr_R) %>% 
        return
}


# Convert Climate data into pressure events series------------
Get_Press_Evt_lab=function(Dt) 
{
    Dt %>% 
        arrange(Time)%>%
        mutate(Mon=month(Time)) %>% 
        # Roll average over 24 hours
        mutate(Press_Evt_lab=ifelse(SLP_chng.av*lag(SLP_chng.av)<=0 & lag(SLP_chng.av)!=0,1,0)) %>% 
        mutate(Press_Evt_lab=ifelse(is.na(Press_Evt_lab),0,Press_Evt_lab)) %>% 
        mutate(Press_Evt_lab=cumsum(Press_Evt_lab)) %>% 
        return
}


# summarise measures for pressure change events
Get_Press_Evt=function(Dt)
{
    Dt %>% 
        mutate(Mon=month(Time),
               Yr=year(Time)) %>% 
        group_by(Yr,Mon) %>% 
        summarise(MonT=mean(Temp,na.rm=T))->Dt_MonT
    
    Dt %>% 
        filter(Press_Evt_lab>0,
               Press_Evt_lab<max(Press_Evt_lab)) %>% 
        group_by(Press_Evt_lab,Loc) %>% 
        summarise(St=min(Time),
                  End=max(Time),
                  Dur=as.numeric(max(Time)-min(Time),units='hours')+1,
                  Sum_Press_Delta=sum(SLP_chng.av),
                  Press_NA=sum(is.na(SLP)),
                  Temp_NA=sum(is.na(Temp)),
                  Sum_Precip=sum(Precip,na.rm=T),
                  St_Rain=sum(StR),
                  St_Dry=sum(StD)) %>% 
        filter(Dur<1000) %>% #take events with more than 10000 hours as gap
        mutate(Yr=year(St),
               Mon=month(St)) %>% 
        left_join(.,Dt_MonT,by=c('Yr'='Yr','Mon'='Mon')) %>% 
        ungroup %>% 
        arrange(St) %>% 
        mutate(Dur_lag1=lag(Dur),
               Press_Delta_lag1=lag(Sum_Press_Delta)) %>% 
        
        return
}




# Read Synthetic Precip

ReadSynPrecip_summary=function(n)
{
    read_rds(glue("temp\\SynDt{n}.rds"))[[2]] %>% 
        select(SyncTime,Precip,Loc,Press_Evt_lab,MonT) %>% 
        rename(Time=SyncTime) %>% 
        arrange(Time) ->x
    
    x %>% 
        mutate(Yr=year(Time)) %>% 
        group_by(Yr) %>% 
        summarise(HrP_max=max(Precip,na.rm=T)) %>% 
        mutate(runid=n)->HrPrecip
    
    x %>% 
        mutate(DtP=c(roll_sum(Precip,24,align = 'left'),rep(NA,23)),
               Yr=year(Time)) %>% 
        group_by(Yr) %>% 
        summarise(DtP_max=max(DtP,na.rm=T))%>% 
        mutate(runid=n)->DtPrecip
    
    x  %>% 
        mutate(Season = as.character(cut(
            month(Time),
            c(2, 5, 8, 11),
            c("Spring (MAM)", "Summer (JJA)", "Fall (SON)")
        )),
        Yr=year(Time)) %>% 
        replace_na(list(Season = "Winter (DJF)")) %>%
        group_by(Yr,Season) %>% 
        summarise(SnP=sum(Precip,na.rm=T))%>% 
        mutate(runid=n)->SnPrecip
    
    x %>% 
        mutate(Mon=month(Time),
               Yr=year(Time)) %>% 
        group_by(Yr,Mon) %>% 
        summarise(Mon_P=sum(Precip,na.rm=T),
                  MonT=mean(MonT)) %>% 
        mutate(runid=n)-> Mon_P
    
    list(HrPrecip,DtPrecip,SnPrecip,Mon_P) %>% 
        return
}


ReadSynPressPerd=function(n) 
{
    read_rds(glue("temp\\SynDt{n}.rds"))[[2]] %>% 
        select(SyncTime,Precip,Loc,Press_Evt_lab,Press_Evt,MonT) %>% 
        rename(Time=SyncTime,SynMonT=MonT,NewPress_perd=Press_Evt,HisPressEvt_lab=Press_Evt_lab) %>% 
        arrange(Time) %>% 
        Precip_Evt_Sep(IntE_P = 4) %>% 
        #Get new pressure events
        group_by(NewPress_perd) %>% 
        summarise(Sum_Precip=sum(Precip),
                  StTime=min(Time),
                  St_Rain=sum(StR),
                  St_Dry=sum(StD),
                  Loc=min(Loc),
                  HisPressEvt_lab=as.numeric(min(HisPressEvt_lab)),
                  SynMonT=min(SynMonT)) %>%
        left_join(Raw_dt_Evt %>% select(Loc,Press_Evt_lab,Sum_Press_Delta,Dur),by=c("Loc"="Loc","HisPressEvt_lab"="Press_Evt_lab")) %>% 
        mutate(Yr=year(StTime),
               Mon=month(StTime)) %>% 
        mutate(Season = as.character(cut(
            month(StTime),
            c(2, 5, 8, 11),
            c("Spring (MAM)", "Summer (JJA)", "Fall (SON)")
        ))) %>%
        replace_na(list(Season = "Winter (DJF)")) %>% 
        mutate(runid=n)
}