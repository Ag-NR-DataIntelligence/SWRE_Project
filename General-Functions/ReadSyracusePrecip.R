
library(data.table)
library(lubridate)
###  Load Historical data -----------
Path='Y:\\Ziwen\\Research\\Precipitation analysis\\Data\\'

#### Syracuse data  
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
  mutate(Time=ymd_h(paste(Date,Hour))) %>% 
  arrange(Time) %>% 
  select(Time,Precip)
