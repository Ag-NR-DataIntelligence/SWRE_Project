
#Actual Vapor Pressure (ea), page 29-32 
  #Calculate actual vapor pressure (ea) by table 4, method 1, page 30
  #Saturation vapor pressure function, page 29, Eq 37
  eo=function(T) {0.618*exp((17.27*T)/(T+237.3))}
  #Saturation Vapor Pressure (es), page 29 
  es=function(T_max,T_min) {(eo(T_max)+eo(T_min))/2}
  
  #Page 32, equation 41
  ea = function(RH_max,RH_min,T_max,T_min) 
    {((RH_max/100)*eo(T_min)+(RH_min/100)*eo(T_max))/2}


# Extraterrestrial Radiation for 24-Hour Periods (Ra ) ------
# MJ m-2 d-1
Ra=function(Lat,Dt) 
  #Lat:  Latitude
  #Dt:   Date
{
  Gsc= 4.92 #solar constant MJ m-2 h-1
  phi=Lat*pi/180  #Radiaus latitude
  J_dt=yday(Dt)  #Julian date
  
  dr=1+0.033*cos(2*pi/360*J_dt)  # inverse relative distance factor (squared) for the earth-sun [unitless],
  
  delta_solar=0.409*sin(2*pi/360*J_dt-1.39) #solar declination [radians].
  
  ws=acos(-tan(phi)*tan(delta_solar))  #sunset hour angle [radians]
  
  return(24/pi*Gsc*dr*
           (ws*sin(phi)*sin(delta_solar)+
              cos(phi)*cos(delta_solar)*sin(ws)
            )
         )
}


#Net Radiation (Rn )-----------
Rn=function(Rs,RHmax,RHmin,Tmax,Tmin,z,Lat,Dt)
  #Rs:   incoming solar radiation [MJ m-2 d-1].
  #RHmax: max relative humidity
  #RHmin: min relative humidity
  #Tmax: maximum absolute temperature during the 24-hour period ,
  #Tmin: minimum absolute temperature during the 24-hour period 
  #z:    station elevation above sea level [m].
  #Lat:  Latitude
  #Dt:   Date
{
  
  #net short-wave radiation, [MJ m-2 d-1] ---------
  #(defined as being positive downwards and negative upwards)
    #-----------
    #albedo or canopy reflection coefficient, is fixed at 0.23 for the
    #standardized short and tall reference surfaces [dimensionless], and
  Alpha=0.19
  Rns=(1-Alpha)*Rs
  
  #net outgoing long-wave radiation, [MJ m-2 d-1] ------
  #(defined as being  positive upwards and negative downwards),
  #-----------
    
  sigma = 4.901e-09;  #Stefan-Boltzman_constant(sigma) in units of MJ K-4 m-2 d-1
  
  Rso=(0.75+2e-5*z)*Ra(Lat,Dt) # calculated clear-sky radiation [MJ m-2 d-1].
  #cloudiness function [dimensionless] (limited to 0.05 ??? fcd ??? 1.0)
  fcd=1.35*Rs/Rso-0.35
  
  Rnl=sigma*fcd*(0.34-0.14*sqrt(ea(RHmax,RHmin,Tmax,Tmin)))*
    ((Tmax+273.16)^4+(Tmin+273.16)^4)/2  #[K] (K =°C + 273.16)
  
  return (Rns-Rnl)
}
  
  
#Slope of the Saturation Vapor Pressure-Temperature Curve---------
  delta_SVP_hr = function (T_mean) {(4098*eo(T_mean))/((T_mean +237.3)^2)}
  
  
#aerodynamic resistance (s/m)
  ra=function(z,h,uz)
    #z:   Height of weather station (m)
    #h:   Crop height (m)
    #uz:  wind speed at height z (m/s)
  {
    K=0.41 #von Karman's constant, 0.41 [-]
    
    return(
      log((z-2/3*h)/(0.123*h))*
        log((z-2/3*h)/(0.0123*h))/
        K^2/
        uz
    )
  }
  
#Psychrometric Constant (gamma), page 10
  #Calculate psychrometric constant (gamma) in kPa °C-1 from atmospheric
  #pressure assuming constant lamda (latent heat of vaporization)
  gamma = function(z) 
  {
    
    #Atmospheric Pressure (P), page 10
    #Calculate atmosperhic pressure (P) in kPa, using site elevation 
    aP = 101.3*(((293-0.0065*z)/293)^5.26)
    
    return(0.000665*aP)
  }
  
  
# wet-surface crop evapotranspiration rate (mm/d)
  ET=function(T_mean,Rs,RHmax,RHmin,Tmax,Tmin,z,Lat,Dt,h,uz)
    #T_mean:   mean air temperature (°C)
    #Rs:   incoming solar radiation [MJ m-2 d-1].
    #RHmax: max relative humidity
    #RHmin: min relative humidity
    #Tmax: maximum absolute temperature during the 24-hour period ,
    #Tmin: minimum absolute temperature during the 24-hour period 
    #z:    station elevation above sea level [m].
    #Lat:  Latitude
    #Dt:   Date
    #h:    Crop height
    #uz:  wind speed at height z (m/s)
  {
    #Soil Heat Flux Density (G) in MJ m-2 d-1
    #Estimated as 0 over the daily time period
    G = 0
    Ktime=86400 #number of seconds in a day
    #ratio of molecular masses of water vapor and dry air (0.622); 
    epshlon=0.622
    #Latent Heat of Vaporization (page 9)
    #Constant: latent heat of vaporization (lambda) in units of MJ kg-1
    lambda = 2.45 
    # Specific gas constant 0.287 kJ kg-1 k-1
    R=0.287
    
    
    fst_pt=delta_SVP_hr(T_mean)*(Rn(Rs,RHmax,RHmin,Tmax,Tmin,z,Lat,Dt)-G)
    es_ea_ov_ra=(es(Tmax,Tmin)-ea(RHmax,RHmin,Tmax,Tmin))/ra(z,h,uz)
    Sec_pt=Ktime*(gamma(z)*epshlon*lambda)/
                    (1.01*R*(T_mean+273))*es_ea_ov_ra
    base=(delta_SVP_hr(T_mean)+gamma(z))*lambda
    return(
      (fst_pt+Sec_pt)/base
    )
  }
