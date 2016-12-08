# Pre-define Function -----------------------------------------------------

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
  n=abs(n-x)
  r1=rank(n,ties.method= "first")
  k=round(sqrt(length(n)))
  return(which(r1<=k))
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
