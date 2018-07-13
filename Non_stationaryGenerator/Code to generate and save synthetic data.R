
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/DataLoad/Loading%20data%20downloaded%20in%202018.R')
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/Non_StationaryInterMFunctions.R')
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/Generator.R')

setwd("\\\\swv.cae.drexel.edu\\group\\Working Publications\\Ziwen\\Climate Change\\Code\\")
MonthT=MonthT_all %>% filter(Loc=="NYC",Emission=="A2")

library(glue)
# GCM models to simulate "IPSL","MIROC","MIUB","GFDL"
set.seed(11)
for (j in 1:100)
{
    SyncPrecip=SyncP_Generate(GCM = 'MIROC',TempWidth=1.5) 
    
    write_rds(SyncPrecip,path=glue("Data\\SynDt{j}.rds"),compress="xz")
    Sys.sleep(0.1)
    print(j)
}



