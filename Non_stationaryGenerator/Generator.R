SyncP_Generate=function(
                        TempWidth = 3, #degree
                        TimeWidth= 45, #day
                        GCM='MIROC',
                        FinalYear = ymd('2099-12-31'))
{
    # Main Code ---------------------------------------------------------------
    
    
    
    PerdType = -1 #Positive:1 Negative:-1
    
    #MonthT=PHLT.Month 
    
    StTime=ymd_hms("2012-01-01 00:00:00")
    
    SynTime=StTime
    Press_Perd.syn=data.frame(Press_Evt_lab=NULL
                              ,Loc=NULL
                              ,Sum_Press_Delta=NULL
                              ,Press_Delta_lag1=NULL
                              ,Dur=NULL
                              ,Dur_lag1=NULL
                              ,MonT=NULL
                              ,HisSt=NULL
                              ,SynSt=NULL)
    
    
    Raw_dt_Evt %<>% 
        mutate(St_yday=yday(St))
    # Loop to generate new events -----------------------
    
    repeat
    {
        # Get window parameters ( number of months, and temperature width)
        #Monthlst=NearestMonths(month(SynTime),Time Window)
        MonT_pro=MonthT %>% 
            filter(year(Date)==year(SynTime), 
                   month(Date)==month(SynTime),
                   Model==GCM) %>% 
            .$MonT
        
        
        repeat
        {
            Sync_yday=yday(SynTime)
            Raw_dt_Evt %>% 
                mutate(St_yday=abs(St_yday-Sync_yday)) %>% 
                filter(!data.table::between(St_yday,TimeWidth, 365-TimeWidth),
                       between(MonT, MonT_pro- TempWidth,MonT_pro+TempWidth),
                       PerdType*Sum_Press_Delta>=0) -> evts_pool
            
            if (nrow(evts_pool)>25) {break
            #Adjust time before temperature
            } else {
                #Adjust temperaure window only
                TempWidth=TempWidth+1
                #Adjust time window before temperature
            #     if (TimeWidth==3) {TempWidth=TempWidth+1
            #     } else {TimeWidth=TimeWidth+0.5}
             }
        }
        
        lagDur=tail(Press_Perd.syn,1)$Dur
        lagPress_Delta=tail(Press_Perd.syn,1)$Sum_Press_Delta
        
        evts_pool %>% 
        {
            dt=.
            
            if(length(FindKnearest(lagPress_Delta,dt$Press_Delta_lag1))==0) 
            { dt} else {
                dt %>% 
                    filter(Press_Delta_lag1 %in% FindKnearest(lagPress_Delta,.$Press_Delta_lag1)) 
            }
            
        } %>% 
            sample_n(.,1) %>% 
            mutate(MonT=MonT_pro) %>% 
            select(Press_Evt_lab,Loc,Sum_Press_Delta,Press_Delta_lag1,Dur,Dur_lag1,MonT,St) %>%
            rename(HisSt=St) %>% 
            mutate(SynSt=SynTime) %>% 
            rbind(Press_Perd.syn,.)->Press_Perd.syn
        
        if ((Press_Perd.syn %>%
             tail(.,1) %>%
             filter(data.table::between(abs(yday(HisSt)-yday(SynSt)),TimeWidth,365-TimeWidth)) %>%
             nrow)>0) break
        
        # Update Sync Time
        SynTime=SynTime+hours(Press_Perd.syn %>% tail(1) %>% .$Dur)
        PerdType=PerdType*-1
        
        
        if (SynTime>FinalYear) break
    }
    
    
    
    # compile precipitation data from events
    Precip.syn=data.frame(Time=NULL,
                          Temp=NULL,
                          DewPt=NULL,
                          SLP=NULL,
                          Precip=NULL,
                          Loc=NULL,
                          HisPressEvt_lab=NULL)
    for (i in 1:nrow(Press_Perd.syn))
    {
        
        Raw_dt_Evt %>% 
            filter(Loc==Press_Perd.syn$Loc[i],
                   Press_Evt_lab==Press_Perd.syn$Press_Evt_lab[i])->Evt
        
        Raw_dt %>% 
            filter(between(Time,Evt$St,Evt$End),
                   Loc==Evt$Loc) %>% 
            mutate(Press_Evt_lab=Evt$Press_Evt_lab) %>%
            select(Time,Temp,DewPt,SLP,Precip,Loc,Press_Evt_lab) %>% 
            rename(HisPressEvt_lab=Press_Evt_lab) %>% 
            rbind(Precip.syn,.)->Precip.syn
    }
    
    Precip.syn %<>% 
        mutate(SyncTime=StTime+hours(row_number()-1))
    
    list(Press_Perd.syn,Precip.syn) %>% return
    
    
}
