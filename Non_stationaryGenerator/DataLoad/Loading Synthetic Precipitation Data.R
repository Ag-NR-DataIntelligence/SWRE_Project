source("https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/DataLoad/Intermedian%20loading%20functions.R")
library(units)
library(glue)
library(tidyverse)
library(lubridate)
library(httr)

dir.create("temp")
URL="https://www.dropbox.com/s/2dp85uh3s4zbfjm/Data.zip?dl=1"
GET(url = URL,write_disk("temp/temp.zip", overwrite = TRUE))
unzip("temp/temp.zip",exdir = "temp")



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

# Remove the temp folder
unlink("temp", recursive=TRUE)