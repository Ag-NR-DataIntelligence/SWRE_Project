SyncP_Generate=function(
    TempWidth = 3, #degree
    TimeWidth=15, #day
    GCM='MIROC',
    FinalYear = ymd('2099-12-31'))
{
    # Main Code ---------------------------------------------------------------
    
    
    
    PerdType = -1 #InPCE:1 DePCE:-1
    
    StTime=ymd_hms("2012-01-01 00:00:00")
    
    SynTime=StTime
    Press_Perd.syn=data.frame(Press_Evt_lab=NULL
                              ,Loc=NULL
                              ,SclPress_delta=NULL
                              ,SclPress_delta_lag=NULL
                              ,SclDur=NULL
                              ,SclDur_lag=NULL
                              ,Dur=NULL
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
        
        Temp_adj=0
        repeat
        {
            Sync_yday=yday(SynTime)
            Raw_dt_Evt %>% 
                mutate(St_yday=abs(St_yday-Sync_yday)) %>% 
                filter(!data.table::between(St_yday,TimeWidth, 365-TimeWidth),
                       between(MonT, MonT_pro- (TempWidth+Temp_adj),MonT_pro+(TempWidth+Temp_adj)),
                       PerdType*Sum_Press_Delta>=0) -> evts_pool
            
            if (nrow(evts_pool)>25) {break
            } else {
                #Adjust temperaure window only
                Temp_adj=Temp_adj+1
            }
        }
        
        lagDur=tail(Press_Perd.syn,1)$SclDur
        lagPress_Delta=tail(Press_Perd.syn,1)$SclPress_delta
        
        evts_pool %>% 
        {
            if (length(lagDur)==0)
            {.} else {
                # Use distance combined pressure change and duration
                dt=. 
                dt %>% 
                    mutate(Dis=sqrt(((SclPress_delta_lag-lagPress_Delta))^2+((SclDur_lag-lagDur))^2)) %>% 
                    arrange(Dis) %>% 
                    filter(row_number()<sqrt(n())) }
        } %>% 
            sample_n(.,1) %>% 
            mutate(MonT=MonT_pro) %>% 
            select(Press_Evt_lab,Loc,SclPress_delta,SclPress_delta_lag,SclDur,SclDur_lag,Dur,MonT,St) %>%
            rename(HisSt=St) %>% 
            mutate(SynSt=SynTime) %>% 
            rbind(Press_Perd.syn,.)->Press_Perd.syn
        
        
        # Update Sync Time
        SynTime=SynTime+hours(Press_Perd.syn %>% tail(1) %>% .$Dur)
        PerdType=PerdType*-1
        
        
        if (SynTime>FinalYear) break
    }
    
    
    
    # compile precipitation data from events
    Press_Perd.syn %>% 
        mutate(Press_Evt=row_number()) %>% 
        left_join(Raw_dt %>% select(Time,Press_Evt_lab,Loc,Precip,SLP.av),by=c("Press_Evt_lab"="Press_Evt_lab","Loc"="Loc")) %>% 
        arrange(Press_Evt,Time) %>% 
        mutate(SyncTime=StTime+hours(row_number()-1)) ->Precip.syn
    
    Precip.syn %<>% 
        mutate(SyncTime=StTime+hours(row_number()-1))
    
    list(Press_Perd.syn,Precip.syn) %>% return
    
    
}
