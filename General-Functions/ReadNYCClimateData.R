
library(data.table)
library(lubridate)
###  Load Historical data -----------
Path='Y:\\Ziwen\\Research\\Precipitation analysis\\Data\\'

#### NYC LGA data  
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
  mutate(Time=ymd_h(paste(Date,Hour))) %>% 
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
  full_join(NYC_LGA_Precip,by=c('Time'='Time')) -> NYC


rm(Col_Nm,Dt_Rng,NYC_LGA_Clim,NYC_LGA_Precip)
