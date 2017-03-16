Libs=c('lubridate','dplyr','tidyr','magrittr','RcppRoll','wrapr','zoo')
lapply(Libs,library, character.only = TRUE)


DtF=read.csv("C:\\Users\\zy32\\Desktop\\Sample data .csv",
             sep=',',header=T,stringsAsFactors = F) %>% 
    mutate(Time=paste0(as.character(Year),'-',
                       as.character(Month),'-',
                       as.character(Date),' ',
                       as.character(Hour),':',
                       as.character(Minute))) %>% 
    mutate(Time=ymd_hm(Time))


    





DtF %>% 
    filter(minute(Time)>50) ->x
    


# Round to nearest hour

Round_hr=function(Datetime)
{
    hrs=round(
        difftime(Datetime,
                 as.Date(Datetime),
                 units='hour') %>% 
            as.numeric )
    
    
    return(as.POSIXct((as.Date(Datetime)+lubridate::hours(hrs)), 
                      format = "%Y-%m-%d %H:%M:%S")
    )
    
}



#Generate time stamps with regular interval of  1 hour-------------------------------

DtF %>% 
    arrange(Time) %>% 
    select(Time) %>% 
    summarize(min(Time),max(Time)) -> Dt_rng

colnames(Dt_rng)=c('MinDt','MaxDt')

Dt_rng %<>% 
    mutate(MinDt=Round_hr(MinDt),
           MaxDt=Round_hr(MaxDt))

Tm_srs=seq(
    from=Dt_rng$MinDt[1],
    to=Dt_rng$MaxDt[1],
    by=300*12
) 
rm(Dt_rng)



# Interpolation------------------------


# Linear for continuous values
y=data.frame(Time=Tm_srs,
             Press=na.approx(x$SLP,x=x$Time,xout=Tm_srs,na.rm=F),
             Temp=na.approx(x$Temperature,x=x$Time,xout=Tm_srs,na.rm=F)
)


# Aggregation for discrete values like Rain
DtF %>% 
    arrange(Time) %>% 
    mutate(Hr=Time-minutes(minute(Time))) %>% 
    group_by(Hr) %>% 
    summarise(Rain=sum(PCP01,na.rm=T)) ->y

