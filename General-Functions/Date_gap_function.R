Date_gap=function(Dt, SynDt,unit="day")
{
   interval(Dt,SynDt) %>% 
        as.period() %>% 
        as.numeric(unit=unit) %>% 
        return
}
