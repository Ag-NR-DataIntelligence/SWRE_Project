source('https://raw.githubusercontent.com/ZyuAFD/OptiRTCCode/master/Project/Library%20and%20Style%20Load.R')
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/DataLoad/ReadNYCClimateData.R')
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/DataLoad/ReadBOSClimateData.R')
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/DataLoad/ReadPHLClimateData.R')


BOS$Loc='BOS'
NYC$Loc='NYC'
PHL$Loc='PHL'


# Convert Climate data into pressure events series
Get_Press_Evt=function(Dt) 
{
  Dt %>% 
    mutate(Mon=month(Time),
           Yr=year(Time)) %>% 
    group_by(Yr,Mon) %>% 
    summarise(MonT=mean(Temp,na.rm=T))->Dt_MonT
  
  
  Dt %>% 
    mutate(SLP_sm=SLP-lag(SLP,24),
           Mon=month(Time)) %>% 
    # Roll average over 24 hours
    mutate(Press_Evt_lab=ifelse(SLP_sm*lag(SLP_sm)<=0 & lag(SLP_sm)!=0,1,0)) %>% 
    filter(!is.na(Press_Evt_lab)) %>% 
    mutate(Press_Evt_lab=cumsum(Press_Evt_lab)) %>% 
    filter(Press_Evt_lab>0,
           Press_Evt_lab<max(Press_Evt_lab)) %>% 
    group_by(Press_Evt_lab,Loc) %>% 
    summarise(St=min(Time),
              End=max(Time),
              Dur=as.numeric(max(Time)-min(Time),units='hours')+1,
              Sum_Press_Delta=sum(SLP_sm),
              Sum_Precip=sum(Precip,na.rm=T)) %>% 
    mutate(Yr=year(St),
           Mon=month(St)) %>% 
    left_join(.,Dt_MonT,by=c('Yr'='Yr','Mon'='Mon')) %>% 
    ungroup %>% 
    arrange(St) %>% 
    mutate(Dur_lag1=lag(Press_Evt_lab),
           Press_Delta_lag1=lag(Sum_Press_Delta)) %>% 
    filter(Dur<10000) %>% #take events with more than 10000 hours as gap
    return
}


BOS_Press_Evt=Get_Press_Evt(BOS)
NYC_Press_Evt=Get_Press_Evt(NYC)
PHL_Press_Evt=Get_Press_Evt(PHL)


Raw_dt=rbind(BOS,NYC,PHL) %>% 
  mutate(Precip=ifelse(is.na(Precip),0,Precip))

rm(BOS,NYC,PHL)

Raw_dt_Evt=rbind(BOS_Press_Evt,NYC_Press_Evt,PHL_Press_Evt)

rm(BOS_Press_Evt,NYC_Press_Evt,PHL_Press_Evt)

# Climate change monthly Temperature data
path='\\\\SWV.cae.drexel.edu\\ziwen\\Research\\Precipitation analysis\\Data\\Data with climate change\\Monthly Temp\\'
file='Philadelphia Temperature A2.csv'
MonthT=read.csv(paste0(path,file),sep=',',header=T,stringsAsFactors = F) %>% 
  mutate(Date=mdy(Date)) %>% 
  gather(.,Model,MonT,-Date)
