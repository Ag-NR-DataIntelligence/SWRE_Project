library(lubridate)
library(tidyverse)
library(padr)
library(RcppRoll)
library(httr)

Location_dt=tibble(USAF=c(725090,999999,724080,999999,725030),
                   NCDC=c(14739,13739,13739,14732,14732),
                   Loc=c("BOS","PHL","PHL","NYC","NYC"))

#create temp folder to download zip file and extract
dir.create("temp")
# Historical Data
URL="https://www.dropbox.com/s/hdla2jmo0s694pa/HistoricalData.zip?dl=1"
GET(url = URL,write_disk("temp/temp.zip", overwrite = TRUE))
unzip("temp/temp.zip",exdir = "temp")
# GCM temp projections
URL="https://www.dropbox.com/s/pxwrv6p99x0elsc/GCM%20Temp.zip?dl=1"
GET(url = URL,write_disk("temp/temp.zip", overwrite = TRUE))
unzip("temp/temp.zip",exdir = "temp")

# Read data
Dt=NULL
path="temp"
for (n in list.files(path,pattern=".txt$"))
{
    read_csv(paste(path,n,sep="/"),
             col_names = c('USAF',
                           'NCDC',
                           'Date',
                           'HrMn',
                           'I',
                           'Type',
                           'QCP',
                           'Temp','Temp_Q',
                           'DewPt','DewPt_Q',
                           'SLP','SLP_Q',
                           'Precip_Pr1','Precip_Amt1',"Precip_I1","Precip_Q1",
                           'Precip_Pr6','Precip_Amt6',"Precip_I6","Precip_Q6",
                           'Precip_Pr24','Precip_Amt24',"Precip_I24","Precip_Q24",
                           'Precip_Pr99','Precip_Amt99',"Precip_I99","Precip_Q99",
                           'RHX'), 
             
             col_types = cols(I=col_character(),
                              Precip_Pr1=col_character(),
                              Precip_Pr6=col_character(),
                              Precip_Pr24=col_character(),
                              Precip_Pr24=col_character(),
                              X31 = col_skip()), 
             skip = 2) %>% 
        bind_rows(Dt)->Dt
}


Dt %>% 
    left_join(Location_dt,by=c("USAF","NCDC")) %>% 
    select(-USAF,-NCDC,-I,-Type,-QCP,Temp_Q,SLP_Q,DewPt_Q,-ends_with("6"),-ends_with("24"),-ends_with("99")) %>% 
    unite(Time,Date,HrMn,sep=" ") %>% 
    #Filter out the invalid data
    mutate(Time=ymd_hm(Time),
           Temp=ifelse(Temp>900,NA,Temp),
           DewPt=ifelse(DewPt>900,NA,DewPt),
           SLP=ifelse(SLP>9000,NA,SLP),
           Precip_Amt1=ifelse(Precip_Pr1!="01" | Precip_Q1 %in% c("2","3","6","7"),NA,Precip_Amt1),
           RHX==ifelse(RHX>900,NA,RHX)) %>%
    select(-Precip_Q1,-Precip_Pr1,-Precip_I1) %>% 
    rename(Precip=Precip_Amt1) %>% 
    mutate(Time=round_date(Time,"hour")) %>% 
    group_by(Time,Loc) %>% 
    summarise(Temp=mean(Temp,na.rm =T),
              DewPt=mean(DewPt,na.rm =T),
              SLP=mean(SLP,na.rm =T),
              Precip=mean(Precip,na.rm=T),
              RHX=mean(RHX,na.rm=T)) %>% 
    arrange(Time) %>% 
    ungroup->Climate_Dt


pad_loc=function(x)
{
    x %>% distinct(Loc) %>% pull(Loc)->Location
    x %>% 
        pad(interval="hour") %>% 
        replace_na(list(Loc=Location)) %>% 
        return
}
# Function to manipulate data
Climate_Dt %>% 
    group_by(Loc) %>% 
    do(pad_loc(.))  %>% 
    group_by(Loc) %>% 
    #Fill gap
    mutate(SLP.spl=spline(x=Time,y=SLP,xout=Time)$y,
           Temp.spl=spline(x=Time,y=Temp,xout=Time)$y,
           DewPt.spl=spline(x=Time,y=DewPt,xout=Time)$y,
           RHX.spl=spline(x=Time,y=RHX,xout=Time)$y) %>%
    #Moving Average
    mutate(Temp.av=roll_mean(Temp.spl,n=24,align='center',fill=NA),
           SLP.av=roll_mean(SLP.spl,n=24,align='center',fill=NA),
           DewPt.av=roll_mean(DewPt.spl,n=24,align='center',fill=NA),
           RHX.av=roll_mean(RHX.spl,n=24,align='center',fill=NA)) %>% 
    #Change of Pressure in 24 hours
    mutate(SLP_chng.av=SLP.av-lag(SLP.av,24)) %>% 
    ungroup->Curated_Climate_Dt





Curated_Climate_Dt %>% 
    group_by(Loc) %>% 
    do(Precip_Evt_Sep(.)) %>% 
    do(Get_Press_Evt_lab(.))->Raw_dt



Raw_dt_Evt=Raw_dt %>% Get_Press_Evt

Raw_dt_Evt %<>% 
    mutate(Season = as.character(cut(
        month(St),
        c(2, 5, 8, 11),
        c("Spring (MAM)", "Summer (JJA)", "Fall (SON)")
    ))) %>%
    replace_na(list(Season = "Winter (DJF)"))

# Climate change monthly Temperature data -------------------
library(readxl)
file="temp/Philadelphia Temperature.xlsx"
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
file="temp/BOS Temperature.xlsx"
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
file="temp/NYC Temperature.xlsx"
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


Raw_dt_Evt %<>% 
    mutate(SclPress_delta=scale(Sum_Press_Delta),
           SclDur=scale(Dur)) %>% 
    mutate(SclPress_delta_lag=lag(SclPress_delta),
           SclDur_lag=lag(SclDur))


rm(BOS_MonthT)
rm(PHL_MonthT)
rm(NYC_MonthT)
rm(Climate_Dt)
rm(Curated_Climate_Dt)
rm(Dt)
rm(Location_dt)
# Remove the temp folder
unlink("temp", recursive=TRUE)
