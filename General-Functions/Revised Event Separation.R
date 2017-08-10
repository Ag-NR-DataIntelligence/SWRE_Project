
Precip_Evt_Sep= function(dt,IntE_P)
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
