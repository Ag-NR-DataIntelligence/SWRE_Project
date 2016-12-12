library(data.table)
library(lubridate)
library(RcppRoll)
source('https://raw.githubusercontent.com/ZyuAFD/OptiRTCCode/master/Project/Library%20and%20Style%20Load.R')
###  Load Historical data -----------
Path='Y:\\Ziwen\\Research\\Precipitation analysis\\Data\\'

#### Miami data  ------------------------------
#Get column names
Col_Nm=c('Date','Time','Precip_cm')

MIAMI_Precip=fread(paste0(Path,'Miami\\Miami Precip.csv'),
                   skip=1,
                   sep=' ',
                   col.names = Col_Nm,
                   colClasses = c('character',
                                  'character',
                                  'numeric')) %>% 
  mutate(Time=substr(Time,1,8)) %>% 
  mutate(Time=ymd_hms(paste(Date,Time)),
         Precip_cm=ifelse(Precip_cm>9999,0,Precip_cm)) %>% 
  arrange(Time) %>% 
  select(Time,Precip_cm)


############### Date Range
Dt_Rng=MIAMI_Precip %>% 
    select(Time) %>% 
    summarise(MinTm=min(Time),
              MaxTm=max(Time))

Tm_srs=seq(
  from=Dt_Rng$MinTm[1],
  to=Dt_Rng$MaxTm[1],
  by=3600
) 
rm(Dt_Rng)



MIAMI=data.frame(Time=Tm_srs) %>% 
  left_join(MIAMI_Precip,by=c('Time'='Time')) %>% 
  mutate(Precip_cm=ifelse(is.na(Precip_cm),0,Precip_cm))
rm(MIAMI_Precip)


#### Syracuse data  ---------------------------
#Get column names
Col_Nm=fread(paste0(Path,'Syracuse\\Precip\\Hancock AP\\1948-2010dat.txt'),
             nrows=1,
             header=F,
             sep=',') %>% 
  t(.)

SYCUS_Precip=fread(paste0(Path,'Syracuse\\Precip\\Hancock AP\\1948-2010dat.txt'),
                   skip=2,
                   sep=',',
                   col.names = Col_Nm,
                   colClasses = c('numeric',
                                  'character',
                                  'character',
                                  'character',
                                  'character',
                                  'character',
                                  'character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character',
                                  'character','numeric','character','character')) %>% 
  select(YEAR,
         MO,
         DA,
         HOUR01,
         HOUR02,
         HOUR03,
         HOUR04,
         HOUR05,
         HOUR06,
         HOUR07,
         HOUR08,
         HOUR09,
         HOUR10,
         HOUR11,
         HOUR12,
         HOUR13,
         HOUR14,
         HOUR15,
         HOUR16,
         HOUR17,
         HOUR18,
         HOUR19,
         HOUR20,
         HOUR21,
         HOUR22,
         HOUR23,
         HOUR24) %>% 
  mutate(Date=paste(YEAR,MO,DA,sep='-')) %>% 
  select(-YEAR,-MO,-DA) %>% 
  gather(Hour,Precip,-Date) %>% 
  mutate(Hour=substr(Hour,5,6)) %>% 
  mutate(Time=ymd_h(paste(Date,Hour)),
         Precip=ifelse(Precip==99999,0,Precip)) %>% 
  arrange(Time) %>% 
  select(Time,Precip)


############### Date Range
Dt_Rng=SYCUS_Precip %>% 
  select(Time) %>% 
  summarise(MinTm=min(Time),
            MaxTm=max(Time))

Tm_srs=seq(
  from=Dt_Rng$MinTm[1],
  to=Dt_Rng$MaxTm[1],
  by=3600
) 
rm(Dt_Rng)



SYCUS=data.frame(Time=Tm_srs) %>% 
  left_join(SYCUS_Precip,by=c('Time'='Time')) %>% 
  mutate(Precip=ifelse(is.na(Precip),0,Precip))

rm(SYCUS_Precip)

#### NYC LGA data  ----------------------------------------
NYC_LGA_Clim=fread(paste0(Path,'NYC\\La Gadia\\Climate\\Data.txt'),
                   sep=',',
                   skip=2,
                   col.names = c('USAF',
                                 'NCDC',
                                 'Date',
                                 'HrMn',
                                 'I',
                                 'Type',
                                 'Temp','Temp_Q',
                                 'DewPt','DewPt_Q',
                                 'SLP','SLP_Q',
                                 'Precip_Pr','Precip_Amt','Precip_I','Precip_Q',
                                 'Precip_Pr','Precip_Amt','Precip_I','Precip_Q',
                                 'Precip_Pr','Precip_Amt','Precip_I','Precip_Q',
                                 'Precip_Pr','Precip_Amt','Precip_I','Precip_Q',
                                 'Precip_I','Precip_Amt',
                                 'PRES_CHG_T','PRES_CHG_Q','PRES_CHG_3Hr','PRES_CHG_3Hr_Q','PRES_CHG_24Hr','PRES_CHG_24Hr_q',
                                 'RHX','unk')
                   ,
                   colClasses=c('numeric',
                                'numeric',
                                'Character',
                                'Character',
                                'Character',
                                'Character',
                                'numeric','Character',
                                'numeric','Character',
                                'numeric','Character',
                                'numeric','numeric','character','character',
                                'numeric','numeric','character','character',
                                'numeric','numeric','character','character',
                                'numeric','numeric','character','character',
                                'character','numeric',
                                'numeric','Character','numeric','Character','numeric','Character',
                                'numeric','Character')
) %>% 
  mutate(Time=ymd_hm(paste(Date,HrMn))) %>% 
  select(Time,
         Temp,
         DewPt,
         SLP) %>% 
  #Remove invalid datas
  mutate(Temp=ifelse(Temp==999.9,NA,Temp),
         DewPt=ifelse(DewPt==999.9,NA,DewPt),
         SLP=ifelse(SLP==9999.9,NA,SLP)) %>% 
  #Combine duplicated Time
  select(Time,
         Temp,
         DewPt,
         SLP) %>% 
  group_by(Time) %>% 
  summarise(Temp=mean(Temp,na.rm =T),
            DewPt=mean(DewPt,na.rm =T),
            SLP=mean(SLP,na.rm =T))


#Get column names
Col_Nm=fread(paste0(Path,'NYC\\La Gadia\\Precip\\4200115587195dat.txt'),
             nrows=1,
             header=F,
             sep='\t') %>% 
  t(.)

NYC_LGA_Precip=fread(paste0(Path,'NYC\\La Gadia\\Precip\\4200115587195dat.txt'),
                     header=T,
                     sep='\t',
                     col.names = Col_Nm,
                     colClasses = c('numeric',
                                    'character',
                                    'character',
                                    'character',
                                    'character',
                                    'character',
                                    'character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character',
                                    'character','numeric','character','character')) %>% 
  select(YEAR,
         MO,
         DA,
         HOUR01,
         HOUR02,
         HOUR03,
         HOUR04,
         HOUR05,
         HOUR06,
         HOUR07,
         HOUR08,
         HOUR09,
         HOUR10,
         HOUR11,
         HOUR12,
         HOUR13,
         HOUR14,
         HOUR15,
         HOUR16,
         HOUR17,
         HOUR18,
         HOUR19,
         HOUR20,
         HOUR21,
         HOUR22,
         HOUR23,
         HOUR24) %>% 
  mutate(Date=paste(YEAR,MO,DA,sep='-')) %>% 
  select(-YEAR,-MO,-DA) %>% 
  gather(Hour,Precip,-Date) %>% 
  mutate(Hour=substr(Hour,5,6)) %>% 
  mutate(Time=ymd_h(paste(Date,Hour)),
         Precip=ifelse(Precip==99999,0,Precip)) %>% 
  arrange(Time) %>% 
  select(Time,Precip)


############### Date Range
Dt_Rng=rbind(
  NYC_LGA_Precip %>% 
    select(Time) %>% 
    summarise(MinTm=min(Time),
              MaxTm=max(Time)),
  NYC_LGA_Clim %>% 
    select(Time) %>% 
    summarise(MinTm=min(Time),
              MaxTm=max(Time))
) %>% 
  summarise(MinTm=max(MinTm),MaxTm=min(MaxTm)) 


NYC_LGA_Clim %<>% 
  filter(Time>=Dt_Rng$MinTm,
         Time<=Dt_Rng$MaxTm) 

NYC_LGA_Precip %<>% 
  filter(Time>=Dt_Rng$MinTm,
         Time<=Dt_Rng$MaxTm) 


NYC_LGA_Clim %>% 
  full_join(NYC_LGA_Precip,by=c('Time'='Time')) %>% 
  filter(minute(Time)==0)-> NYC


Dt_Rng=rbind(
  NYC %>% 
    select(Time) %>% 
    summarise(MinTm=min(Time),
              MaxTm=max(Time))
) %>% 
  summarise(MinTm=max(MinTm),MaxTm=min(MaxTm)) 


Tm_srs=seq(
  from=Dt_Rng$MinTm[1],
  to=Dt_Rng$MaxTm[1],
  by=3600
) 
rm(Dt_Rng)


NYC=data.frame(Time=Tm_srs) %>% 
  left_join(NYC,by=c('Time'='Time')) %>% 
  mutate(Precip=ifelse(is.na(Precip),0,Precip))


rm(Col_Nm,NYC_LGA_Clim,NYC_LGA_Precip,Tm_srs)


# Seperate Precipitation Event----------------------------------------------------
Precip_Evt_Sep= function(dt,Int_P)
{
  #The header of time and rain should be
  # Time    Rain
  
  dt %>% 
    arrange(Time) %>% 
    #select(Time,Rain) %>% 
    mutate(Cum_Precip_4hr_L=roll_sum(Rain,Int_P+1,align='left',fill=0)-Rain,
           Cum_Precip_4hr_R=roll_sum(Rain,Int_P+1,align='right',fill=0)-Rain) %>% 
    mutate(St=ifelse(Cum_Precip_4hr_R==0 & Rain>0,1,0),
           End=ifelse(Cum_Precip_4hr_L==0 & Rain>0,1,0)) %>% 
    mutate(St=ifelse(Rain>0 & is.na(lag(Rain)),1,St)) %>%
    mutate(Evt_lab=St+End) %>% 
    mutate(Evt_lab=cumsum(Evt_lab)) %>%
    mutate(Evt_lab=ifelse(lag(Evt_lab)<Evt_lab & ((Evt_lab %% 2==0)  & !is.na(lag(Evt_lab))),Evt_lab-1,Evt_lab)) %>% 
    mutate(Evt_lab=ifelse(is.na(lag(Evt_lab)) & (Evt_lab %% 2==0),Evt_lab-1,Evt_lab)) %>% 
    mutate(Evt_lab=ifelse(Evt_lab %% 2==0,0,(Evt_lab+1) %/% 2)) %>% 
    return
}

MonPEvnts=data.frame(Year=NULL,
                     Mon=NULL,
                     NumEvts=NULL,
                     Loc=NULL)


for (IntP in 1:20)
{
  
  MIAMI %>% 
    rename(Rain=Precip_cm) %>%
    arrange(Time) %>% 
    Precip_Evt_Sep(.,IntP) %>% 
    mutate(Mon=month(Time),
           Year=year(Time)) %>% 
    group_by(Year,Mon) %>% 
    summarise(NumEvtP=max(Evt_lab)) %>% 
    ungroup %>% 
    arrange(Year,Mon) %>% 
    filter(NumEvtP>0) %>% 
    mutate(MonNumEvt=NumEvtP-ifelse(is.na(lag(NumEvtP)),0,lag(NumEvtP))) %>% 
    select(Year,Mon,MonNumEvt) %>% 
    mutate(Loc='MIAMI',
           IntDP=IntP) -> MonP
  
  MonPEvnts %<>% rbind(.,MonP)
  rm(MonP)
  
  
  NYC %>% 
    rename(Rain=Precip) %>% 
    mutate(Rain=ifelse(is.na(Rain),0,Rain)) %>% 
    arrange(Time) %>% 
    Precip_Evt_Sep(.,IntP) %>% 
    mutate(Mon=month(Time),
           Year=year(Time)) %>%
    filter(Time<ymd('2011-5-1')) %>% 
    group_by(Year,Mon) %>% 
    summarise(NumEvtP=max(Evt_lab)) %>% 
    ungroup %>% 
    arrange(Year,Mon) %>% 
    filter(NumEvtP>0) %>% 
    mutate(MonNumEvt=NumEvtP-ifelse(is.na(lag(NumEvtP)),0,lag(NumEvtP))) %>% 
    select(Year,Mon,MonNumEvt) %>% 
    mutate(Loc='NYC',
           IntDP=IntP) -> MonP
  
  MonPEvnts %<>% rbind(.,MonP)
  
  SYCUS %>% 
    rename(Rain=Precip) %>% 
    mutate(Rain=ifelse(is.na(Rain),0,Rain)) %>% 
    arrange(Time) %>% 
    Precip_Evt_Sep(.,IntP) %>% 
    mutate(Mon=month(Time),
           Year=year(Time)) %>% 
    group_by(Year,Mon) %>% 
    summarise(NumEvtP=max(Evt_lab)) %>% 
    ungroup %>% 
    arrange(Year,Mon) %>% 
    filter(NumEvtP>0) %>% 
    mutate(MonNumEvt=NumEvtP-ifelse(is.na(lag(NumEvtP)),0,lag(NumEvtP))) %>% 
    select(Year,Mon,MonNumEvt) %>% 
    mutate(Loc='Syracus',
           IntDP=IntP) -> MonP
  
  MonPEvnts %<>% rbind(.,MonP)
}


MonPEvnts %>% 
  rename(Location=Loc) %>% 
  ggplot(data=.,aes(x=factor(IntDP),y=MonNumEvt,fill=Location,color=Location,linetype =Location))+
  geom_boxplot(outlier.shape = NA,alpha=0.1)+
  geom_smooth(se=FALSE,  aes(group=Location),)+
  ylim(0,75)+
  labs(y='Monthly Number of Events',x='Inter Event Dry Period')+
  Plot_theme+
  theme(axis.title.x= element_text(size=15),
        legend.title=element_blank(),
        legend.position = c(0.9, 0.9))

rm(MonPEvnts,MIAMI,SYCUS,MonP)

# Generating Synthetic Precipitation in NYC --------------------------------------------

source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/General-Functions/SynPrecipGeneratorFuncs.R')

#Pickup the Events within the current window
WithinWindow=function(CurT,AllDt,W_width=30)
{
  # CurT: current time
  # AllDt: All Data
  # W_width: Window width (Days)
  a=W_width/2
  return(AllDt[(as.numeric(AllDt$Wind_dt-CurT,units='days'))<a |abs(as.numeric(AllDt$Wind_dt-CurT,units='days')-366)<a,])
}

# Separate Events
Precip_Evt_Sep1= function(dt,Int_P)
{
  #The header of time and rain should be
  # Time    Rain
  
  dt %>% 
    arrange(Time) %>% 
    #select(Time,Rain) %>% 
    mutate(Cum_Precip_4hr_L=roll_sum(Rain,Int_P+1,align='left',fill=0)-Rain,
           Cum_Precip_4hr_R=roll_sum(Rain,Int_P+1,align='right',fill=0)-Rain) %>% 
    mutate(St=ifelse(Cum_Precip_4hr_R==0 & Rain>0,1,0),
           End=ifelse(Cum_Precip_4hr_L==0 & Rain>0,1,0)) %>% 
    mutate(St=ifelse(Rain>0 & is.na(lag(Rain)),1,St)) %>%
    mutate(Evt_lab=St+End) %>% 
    mutate(Evt_lab=cumsum(Evt_lab)) %>%
    mutate(Evt_lab=ifelse(lag(Evt_lab)<Evt_lab & ((Evt_lab %% 2==0)  & !is.na(lag(Evt_lab))),Evt_lab-1,Evt_lab)) %>% 
    mutate(Evt_lab=ifelse(is.na(lag(Evt_lab)) & (Evt_lab %% 2==0),Evt_lab-1,Evt_lab)) %>% 
    return
}

InterEvr_P=4

NYC %>% 
  rename(Rain=Precip) %>% 
  mutate(Rain=ifelse(is.na(Rain),0,Rain)) %>% 
  arrange(Time) %>% 
  Precip_Evt_Sep1(.,InterEvr_P) %>% 
  mutate(Mon=month(Time),
         Year=year(Time)) %>%
  filter(Time<ymd('2011-5-1')) %>% 
  group_by(Evt_lab) %>% 
  summarise(St=min(Time),
            End=max(Time),
            Type=ifelse(sum(Rain)>0,1,0)) %>% 
  mutate(Dur=as.numeric(End-St)/3600+1) %>% 
  filter(Evt_lab>0) %>% 
  mutate(Wind_dt=St+years(2012-year(St)),
         PreEvt_Dur=lag(Dur))->NYC_Evt


# initiation
StartT=ymd('2012-01-01')
Wndw_width= 30 #Days
Ptype= 1 # Rain 1, Dry 0
FinalYear = 2500

set.seed(40)

SynPrecipEvt=data.frame(Evt_lab=NULL,
                        Dur=NULL)

# choose first Event---------

NYC_Evt %>%
  # Find Window
  WithinWindow(StartT,.,Wndw_width) %>% 
  # Find Evt Types
  filter(Type==Ptype) %>% 
  # Sampling 1 Evt
  sample_n(1) %>% 
  select(Evt_lab,Dur)-> SynEvt

#Append Evt
SynPrecipEvt %<>% 
  rbind(SynEvt)

# Update Curr Time point
CurT=StartT+hours(tail(SynPrecipEvt$Dur,1))

Ptype=1-Ptype



# Generate Synthetic Events ---------------------------
repeat 
{
  
  NYC_Evt %>%
    # Find Window
    WithinWindow(CurT,.,Wndw_width) %>% 
    # Find Evt Types
    filter(Type==Ptype) %>% 
    # Find KNN Evts
    .$PreEvt_Dur %>% 
    FindKnearest(tail(SynPrecipEvt$Dur,1),.)-> Indx
  
  
  NYC_Evt[Indx,] %>% 
    # Sampling 1 Evt
    sample_n(1) %>% 
    select(Evt_lab,Dur)-> SynEvt
  
  
  #Append Evt
  SynPrecipEvt %<>% 
    rbind(SynEvt)
  
  # Update Curr Time point
  CurT=CurT+hours(tail(SynPrecipEvt$Dur,1))
  
  Ptype=1-Ptype
  
  rm(SynEvt)
  # Check if the synthetic series reach the end of the simulation
  if (year(CurT)>FinalYear) {break}
  print(CurT)
  
}









# form up precipitation series --------------------------------------------
NYC %<>% 
  rename(Rain=Precip) %>% 
  mutate(Rain=ifelse(is.na(Rain),0,Rain)) %>% 
  arrange(Time) %>% 
  Precip_Evt_Sep1(.,InterEvr_P) %>% 
  rename(Precip=Rain)

SynPrecipEvt %<>%
  mutate(RollDur=cumsum(Dur))

StartTime=ymd_hms("2012-01-01 00:00:00")

Precip.syn = data.frame(Time=NULL,
                        Precip=NULL)

for (i in c(1:nrow(SynPrecipEvt)))
{
  
  tmp=NYC %>% 
    filter(Evt_lab==SynPrecipEvt$Evt_lab[i]) %>% 
    select(Time,
           Precip) 
  
  Precip.syn=rbind(Precip.syn,tmp)
  
  if (nrow(Precip.syn)<SynPrecipEvt$RollDur[i]) break
  print(i)
  rm(tmp)
}

Precip.syn %<>% 
  mutate(ID=row_number()) %>% 
  mutate(SynTime=StartTime+lubridate::hours(ID-1),
         Precip=ifelse(is.na(Precip),0,Precip))

write.table(Precip.syn %>% 
              mutate(Precip=Precip*.025) %>% # Convert to cm
              select(SynTime,Precip),
            "F:\\NYC SynPrecip.csv",
            sep=';',
            append=TRUE,
            row.names=FALSE)





Precip.syn %>% 
  filter(Precip<9999) %>% 
  mutate(Year=year(SynTime),
         Mon=month(SynTime)) %>% 
  # filter(Year==2012,
  #        Mon==9) %>% head(1000) %>% View()
  group_by(Year,Mon) %>% 
  summarise(MonP=sum(Precip/100)) %>% 
  ggplot()+
  geom_boxplot(aes(x=factor(Mon),y=MonP))+
  



  
