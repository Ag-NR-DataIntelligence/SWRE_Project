# RET Calculation 

library(StreamMetabolism)


#function to calculate sunrise and sunset time for a specific day

sunrs.st=function (lat, long, date, timezone = "UTC", num.days = 1) 
{
  lat.long <- matrix(c(long, lat), nrow = 1)
  day <- as.POSIXct(date, tz = timezone)
  sunrise <- sunriset(lat.long, day, direction = "sunrise", 
                      POSIXct = TRUE)
  sunset <- sunriset(lat.long, day, direction = "sunset", 
                     POSIXct = TRUE)
  ss <- data.frame(sunrise, sunset)
  ss <- ss[, -c(1, 3)]
  colnames(ss) <- c("sunrise", "sunset")
  return(ss)
}



#Rn (Net radiation at the crop surface [MJ/m^2/hr])
#G (soil heat flux density [MJ/m^2/day]) neglected
#Tmean (mean air temperature ['C]) : AirTC_Avg
#u2 (wind speed measured at 2 m height [m/s]) : WS_ms_Min

#*es (saturation vapor pressure [kPa]) = 0.6108*exp((17.27T)/(T+237.3))
#*ea (actual vapor pressure [kPa]) = es(AirTC_Avg)*RH/100-----------Need to confirm with lauren
#*es-ea (saturation vapor pressure deficit [kPa])


es_ea=function(RH,AirT)
{
  
  return(0.6108*exp(17.27*AirT/(AirT+237.3))*(1-RH/100))
}

#*deltaVP (slope vapor pressure curve) : 4098*(0.6108*exp(17.27*AirTC_Avg/(AirTC_Avg+237.3)))/(AirTC_Avg+237.3^2)

DeltaVP = function(AirT)
{
  
  4098*
    (0.6108*
       exp(17.27*AirT/
             (AirT+237.3)
       )
    )/(AirT+237.3)^2
}

#P atmospheric pressure 101.3((293-0.0065*z)/293^5.26 (z: elevation above sea level)

Atmo_Press= function (elevation) 
{
  return (101.3*((293-0.0065*elevation)/293)^5.26)
}

#gamma (psychrometric constant [kPa/'C]) : 0.665e-3 * P

gamma=function(elevation)
{
  return(0.665e-3*Atmo_Press(elevation))
}


RET_hr =function(elevation,R_Up,AirT,u2,RH,Day_nit)
{
  Cn=37
  Cd=ifelse(Day_nit=='D',0.24,0.96)
  
  (
    0.408*DeltaVP(AirT)*R_Up+
      gamma(elevation)*Cn/(AirT+273)*u2*es_ea(RH,AirT)
  )/
    (
      DeltaVP(AirT)+gamma(elevation)*(1+Cd*u2)
    ) #(mm/day) 
}
