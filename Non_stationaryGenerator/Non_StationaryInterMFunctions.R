
#Pickup the nearest months
NearestMonths<-function(x,n) {
  #x is the centered month, n is the number of months
  result=c((x-n %/% 2):(x+n %/% 2))
  result[which(result>12)]=result[which(result>12)]-11
  result[which(result<1)]=result[which(result<1)]+12
  return(result)
}

#Pickup the nearest numbers
FindKnearest<- function(x,n) {
  #x is the centered value, n is the vector
  n1=abs(n-x)
  r1=rank(n1,ties.method= "first")
  k=round(sqrt(length(n1)))
  return(n[which(r1<=k)])
}

#update current month, current month Temperature
UpdateCurMon_MonT <- function(x,MonTProj,GCM) {
  #x is the current Date
  CurMon<<-month(x)
  CurMonT<<-MonTProj %>% 
    gather(GCM,MonT,BCCR:UKMO) %>% 
    filter(year(Date)==year(x), month(Date)==month(x),GCM=='BCCR') %>% 
    .$MonT
}


#Round time to each hour
Round_hour=function(Datetime)
{
  hrs=difftime(Datetime,
             as.Date(Datetime),
             units='hour') %>% 
    round
  
  
  return(as.POSIXct((as.Date(Datetime)+hours(hrs)), 
                    format = "%Y-%m-%d %H:%M:%S")
  )
  
}

# function to find the day gap getween two synthetic dates
SyncDate_gap=function(Dt, SynDt,unit="day")
{
    interval(Dt,SynDt) %>% 
        as.period()  %>% 
        {
            gap=.
            (gap-years(as.numeric(gap,unit=unit)%/%1)) %>% 
                as.numeric(unit=unit)
        }  %>% 
        return
}
