source('https://raw.githubusercontent.com/ZyuAFD/OptiRTCCode/master/Project/Library%20and%20Style%20Load.R')
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/DataLoad/ReadNYCClimateData.R')
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/DataLoad/ReadBOSClimateData.R')
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/DataLoad/ReadPHLClimateData.R')

#Precip Evt Separation function for 5 min interval

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





rm(Col_Nm,
   BOS_Clim,
   BOS_Precip,
   Dt_Rng,
   NYC_LGA_Clim,
   NYC_LGA_Precip,
   PHL_Clim,
   PHL_Precip)

BOS$Loc='BOS'
NYC$Loc='NYC'
PHL$Loc='PHL'

BOS %<>% Precip_Evt_Sep(.,4) 
PHL %<>% Precip_Evt_Sep(.,4)
NYC %<>% Precip_Evt_Sep(.,4)

Raw_dt=rbind(BOS,NYC,PHL) %>% 
  mutate(Precip=ifelse(is.na(Precip),0,Precip))


# Convert Climate data into pressure events series------------
Get_Press_Evt=function(Dt) 
{
    Dt %>% 
        mutate(Mon=month(Time),
               Yr=year(Time)) %>% 
        group_by(Yr,Mon) %>% 
        summarise(MonT=mean(Temp,na.rm=T))->Dt_MonT
    
    
    Dt %>% 
        arrange(Time)%>%
        mutate(Mon=month(Time)) %>% 
        # Roll average over 24 hours
        mutate(Press_Evt_lab=ifelse(SLP_chng.av*lag(SLP_chng.av)<=0 & lag(SLP_chng.av)!=0,1,0)) %>% 
        mutate(Press_Evt_lab=ifelse(is.na(Press_Evt_lab),0,Press_Evt_lab)) %>% 
        mutate(Press_Evt_lab=cumsum(Press_Evt_lab)) %>% 
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
        mutate(Yr=year(St),
               Mon=month(St)) %>% 
        left_join(.,Dt_MonT,by=c('Yr'='Yr','Mon'='Mon')) %>% 
        ungroup %>% 
        arrange(St) %>% 
        mutate(Dur_lag1=lag(Dur),
               Press_Delta_lag1=lag(Sum_Press_Delta)) %>% 
        filter(Dur<10000) %>% #take events with more than 10000 hours as gap
        return
}

BOS_Press_Evt=Get_Press_Evt(BOS)
NYC_Press_Evt=Get_Press_Evt(NYC)
PHL_Press_Evt=Get_Press_Evt(PHL)



Raw_dt_Evt=rbind(BOS_Press_Evt,NYC_Press_Evt,PHL_Press_Evt) %>% 
    filter(Press_NA<7)


# Climate change monthly Temperature data -------------------
library(readxl)
file="\\\\swv.cae.drexel.edu\\personal\\Ziwen/Research/Precipitation analysis/Data/Data with climate change/Monthly Temp/Philadelphia Temperature.xlsx"
PHL_MonthT <- bind_rows(read_excel(file, 
                                sheet = "A2") %>% 
                           mutate(Emission="A2"),
                       read_excel(file, 
                                  sheet = "B1") %>% 
                           mutate(Emission="B1"),
                       read_excel(file, 
                                  sheet = "A1B") %>% 
                           mutate(Emission="A1B")
) %>% mutate(Loc="PHL")
file="\\\\swv.cae.drexel.edu\\personal\\Ziwen/Research/Precipitation analysis/Data/Data with climate change/Monthly Temp/BOS Temperature.xlsx"
BOS_MonthT <- bind_rows(read_excel(file, 
                                  sheet = "A2") %>% 
                           mutate(Emission="A2"),
                       read_excel(file, 
                                  sheet = "B1") %>% 
                           mutate(Emission="B1"),
                       read_excel(file, 
                                  sheet = "A1B") %>% 
                           mutate(Emission="A1B")
) %>% mutate(Loc="BOS")
file="\\\\swv.cae.drexel.edu\\personal\\Ziwen/Research/Precipitation analysis/Data/Data with climate change/Monthly Temp/NYC Temperature.xlsx"
NYC_MonthT <- bind_rows(read_excel(file, 
                                  sheet = "A2") %>% 
                           mutate(Emission="A2"),
                       read_excel(file, 
                                  sheet = "B1") %>% 
                           mutate(Emission="B1"),
                       read_excel(file, 
                                  sheet = "A1B") %>% 
                           mutate(Emission="A1B")
) %>% mutate(Loc="NYC")

MonthT_all=bind_rows(PHL_MonthT,BOS_MonthT,NYC_MonthT) %>% 
  mutate(Date=ymd(Date)) %>% 
  gather(.,Model,MonT,-Date,-Emission,-Loc)

rm(PHL_MonthT,BOS_MonthT,NYC_MonthT)
rm(PHL,PHL_Press_Evt)
rm(NYC,NYC_Press_Evt)
rm(BOS,BOS_Press_Evt)
