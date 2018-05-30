SyncDate_gap=function(Dt, SynDt,unit="day")
{
   interval(Dt,SynDt) %>% 
        as.period()  %>% 
        {
            gap=.
            (gap-years(as.numeric(gap,unit="year")%/%1)) %>% 
                as.numeric(unit=unit)
        }  %>% 
        return
}
