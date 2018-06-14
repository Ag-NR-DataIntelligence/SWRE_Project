
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/DataLoad/DataPreparation.R')
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/Non_StationaryInterMFunctions.R')
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/Generator.R')

setwd("Z:\\Working Publications\\Ziwen\\Climate Change\\Code\\")
MonthT=MonthT_all %>% filter(Loc=="NYC",Emission=="A2")

library(glue)
# GCM models to simulate "IPSL","MIROC","MIUB","GFDL"
set.seed(11)
for (j in 1:100)
{
    SyncPrecip=SyncP_Generate(GCM = 'MIROC',TempWidth=1.5) 
    SyncPrecip[[1]] %>% mutate(Runid=j)
    SyncPrecip[[2]] %>% mutate(Runid=j)
    
    write_rds(SyncPrecip,path=glue("Data\\SynDt{j}.rds"),compress="gz")
    Sys.sleep(0.1)
    print(j)
}


