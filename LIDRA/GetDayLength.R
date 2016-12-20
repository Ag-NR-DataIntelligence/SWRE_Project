Get_Day_Length_For_LIDRA= function(Lat,Long)
{
  library(StreamMetabolism)
  library(lubridate)
  
  DayLength=function(lat,long,Date)
  {
    Sun_R_S=data.frame(sunrise.set(lat, long, Date, timezone="UTC") )
    DayLen=as.numeric(Sun_R_S$sunset-Sun_R_S$sunrise,unit='hours')
    return(DayLen)
  }
  
  
  Yr_dt=data.frame(Dt=ymd('2012-01-01'))
  
  for (i in 1:365) Yr_dt=rbind(Yr_dt,data.frame(Dt=ymd('2012-01-01')+lubridate::days(i)))
  
  Yr_dt$DayLen=NULL
  
  for (i in 1:nrow(Yr_dt))
  {
    Yr_dt$DayLen[i]=DayLength(Lat,Long,Yr_dt$Dt[i])
  }
  
  return(Yr_dt)
}
