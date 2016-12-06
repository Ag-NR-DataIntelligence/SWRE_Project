library(data.table)
library(lubridate)
###  Load Historical data -----------
Path='Y:\\Ziwen\\Research\\Precipitation analysis\\Data\\'

#### Syracuse data  
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
  mutate(Time=ymd_hms(paste(Date,Time))) %>% 
  arrange(Time) %>% 
  select(Time,Precip_cm)
