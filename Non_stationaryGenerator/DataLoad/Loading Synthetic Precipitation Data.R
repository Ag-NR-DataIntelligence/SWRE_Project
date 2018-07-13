
library(units)
library(glue)

dir.create("temp")
URL="https://www.dropbox.com/s/2dp85uh3s4zbfjm/Data.zip?dl=1"
GET(url = URL,write_disk("temp/temp.zip", overwrite = TRUE))
unzip("temp/temp.zip",exdir = "temp")

# Read Synthetic Precip

ReadSynPrecip_summary=function(n)
{
    read_rds(glue("temp\\SynDt{n}.rds"))[[2]] %>% 
        select(SyncTime,Precip,Loc,Press_Evt_lab,MonT) %>% 
        rename(Time=SyncTime) %>% 
        arrange(Time) ->x
    
    x %>% 
        mutate(Yr=year(Time)) %>% 
        group_by(Yr) %>% 
        summarise(HrP_max=max(Precip)) %>% 
        mutate(runid=n)->HrPrecip
    
    x %>% 
        mutate(DtP=c(roll_sum(Precip,24,align = 'left'),rep(NA,23)),
               Yr=year(Time)) %>% 
        group_by(Yr) %>% 
        summarise(DtP_max=max(DtP))%>% 
        mutate(runid=n)->DtPrecip
    
    x  %>% 
        mutate(Season = as.character(cut(
            month(Time),
            c(2, 5, 8, 11),
            c("Spring (MAM)", "Summer (JJA)", "Fall (SON)")
        )),
        Yr=year(Time)) %>% 
        replace_na(list(Season = "Winter (DJF)")) %>%
        group_by(Yr,Season) %>% 
        summarise(SnP=sum(Precip))%>% 
        mutate(runid=n)->SnPrecip
    
    x %>% 
        mutate(Mon=month(Time),
               Yr=year(Time)) %>% 
        group_by(Yr,Mon) %>% 
        summarise(Mon_P=sum(Precip),
                  MonT=mean(MonT)) %>% 
        mutate(runid=n)-> Mon_P
    
    list(HrPrecip,DtPrecip,SnPrecip,Mon_P) %>% 
        return
}


ReadSynPressPerd=function(n) 
{
    read_rds(glue("temp\\SynDt{n}.rds"))[[2]] %>% 
        select(SyncTime,Precip,Loc,Press_Evt_lab,Press_Evt,MonT) %>% 
        rename(Time=SyncTime,SynMonT=MonT,NewPress_perd=Press_Evt,HisPressEvt_lab=Press_Evt_lab) %>% 
        arrange(Time) %>% 
        Precip_Evt_Sep(IntE_P = 4) %>% 
        #Get new pressure events
        group_by(NewPress_perd) %>% 
        summarise(Sum_Precip=sum(Precip),
                  StTime=min(Time),
                  St_Rain=sum(StR),
                  St_Dry=sum(StD),
                  Loc=min(Loc),
                  HisPressEvt_lab=as.numeric(min(HisPressEvt_lab)),
                  SynMonT=min(SynMonT)) %>%
        left_join(Raw_dt_Evt %>% select(Loc,Press_Evt_lab,Sum_Press_Delta,Dur),by=c("Loc"="Loc","HisPressEvt_lab"="Press_Evt_lab")) %>% 
        mutate(Yr=year(StTime),
               Mon=month(StTime)) %>% 
        mutate(Season = as.character(cut(
            month(StTime),
            c(2, 5, 8, 11),
            c("Spring (MAM)", "Summer (JJA)", "Fall (SON)")
        ))) %>%
        replace_na(list(Season = "Winter (DJF)")) %>% 
        mutate(runid=n)
}



SyncPress_Perd=ReadSynPressPerd(1)

SyncPrecip=ReadSynPrecip_summary(1)

for (n in 2:100)
{
    destfile=glue("temp\\SynDt{n}.rds")
    if(!file.exists(destfile)) break
    
    SyncPress_Perd=ReadSynPressPerd(n) %>% 
        bind_rows(SyncPress_Perd,.)
    
    x=ReadSynPrecip_summary(n)
    SyncPrecip[[1]]=bind_rows(SyncPrecip[[1]],x[[1]])
    SyncPrecip[[2]]=bind_rows(SyncPrecip[[2]],x[[2]])
    SyncPrecip[[3]]=bind_rows(SyncPrecip[[3]],x[[3]])
    SyncPrecip[[4]]=bind_rows(SyncPrecip[[4]],x[[4]])
    
    gc()
}

SyncPress_Perd %<>% 
    mutate(Season = as.character(cut(
        month(StTime),
        c(2, 5, 8, 11),
        c("Spring (MAM)", "Summer (JJA)", "Fall (SON)")
    ))) %>%
    replace_na(list(Season = "Winter (DJF)"))

rm(ReadSynPrecip_summary)
rm(ReadSynPressPerd)
