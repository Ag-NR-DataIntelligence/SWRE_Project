source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/DataLoad/DataPreparation.R')
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/Generator.R')

for (j in 1:50)
{
    SyncPrecip=SyncP_Generate(FinalYear = ymd('2016-12-31')) %>% 
        mutate(Runid=j)
    
    
    write.table(SyncPrecip,
                'Y:\\Ziwen\\Research\\Precipitation analysis\\Data\\Dissertation\\Dissertation Data\\SyncPrecipitation.csv',
                row.names = F,
                col.names = (j==1),
                append = T)
}
