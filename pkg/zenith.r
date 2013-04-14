solar <- function(day) {

  ## Time as Julian day (R form)
  Jd <- as.numeric(day)/86400.0+2440587.5

  ## Time as Julian century [G]
  Jc <- (Jd-2451545)/36525

  ## The geometric mean sun longitude (degrees) [I]
  L0 <- (280.46646+Jc*(36000.76983+0.0003032*Jc))%%360


  ## Geometric mean anomaly for the sun (degrees) [J]
  M <- 357.52911+Jc*(35999.05029-0.0001537*Jc)

  ## The eccentricity of earth's orbit [K]
  e <- 0.016708634-Jc*(0.000042037+0.0000001267*Jc)

  ## Equation of centre for the sun (degrees) [L]
  eqctr <- sin(pi/180*M)*(1.914602-Jc*(0.004817+0.000014*Jc))+
    sin(pi/180*2*M)*(0.019993-0.000101*Jc)+
      sin(pi/180*3*M)*0.000289

  ## The true longitude of the sun (degrees) [M]
  lambda0 <- L0 + eqctr

  ## The apparent longitude of the sun (degrees) [P]
  omega <- 125.04-1934.136*Jc
  lambda <- lambda0-0.00569-0.00478*sin(pi/180*omega)


  ## The mean obliquity of the ecliptic (degrees) [Q]
  seconds <- 21.448-Jc*(46.815+Jc*(0.00059-Jc*(0.001813)))
  obliq0 <- 23+(26+(seconds/60))/60

  ## The corrected obliquity of the ecliptic (degrees) [R]
  omega <- 125.04-1934.136*Jc
  obliq <- obliq0 + 0.00256*cos(pi/180*omega)

  ## The equation of time (minutes of time) [U,V]
  y <- tan(pi/180*obliq/2)^2
  eqtime <- 180/pi*4*(y*sin(pi/180*2*L0) -
                      2*e*sin(pi/180*M) +
                      4*e*y*sin(pi/180*M)*cos(pi/180*2*L0) -
                      0.5*y^2*sin(pi/180*4*L0) -
                      1.25*e^2*sin(pi/180*2*M))

  ## The sun's declination (radians) [T]
  solarDec <- asin(sin(pi/180*obliq)*sin(pi/180*lambda))
  sinSolarDec <- sin(solarDec)
  cosSolarDec <- cos(solarDec)

  ## Solar time unadjusted for longitude (degrees) [AB!!]
  ## Am missing a mod 360 here, but is only used within cosine.
  solarTime <- ((Jd-0.5)%%1*1440+eqtime)/4

  ## Return solar constants
  list(solarTime=solarTime,
       sinSolarDec=sinSolarDec,
       cosSolarDec=cosSolarDec)
}


## Compute elevations
zenith <- function(lon, lat, sun) {

  ## change subtraction to addition to work with -180<->180 convention MDS2Jul03
  ## Suns hour angle (degrees) [AC!!]
  hourAngle <- sun$solarTime+lon-180

  ## Cosine of sun's zenith [AD]
  cosZenith <- (sin(pi/180*lat)*sun$sinSolarDec+
                cos(pi/180*lat)*sun$cosSolarDec*cos(pi/180*hourAngle))

  ## Limit to [-1,1] [!!]
  cosZenith[cosZenith > 1] <- 1
  cosZenith[cosZenith < -1] <- -1

  ## Ignore refraction correction
  180/pi*acos(cosZenith)
}


## Adjust zenith angle for atmospheric refraction - this is most
## relevant in the range 85 to 95 degrees.
refraction <- function(zenith) {
  elev <- 90-zenith
  te <- tan((pi/180)*elev)
  ## Atmospheric Refraction [AF]
  r <- ifelse(elev>85,0,
              ifelse(elev>5,58.1/te-0.07/te^3+0.000086/te^5,
                     ifelse(elev>-0.575,
                            1735+elev*(-518.2+elev*(103.4+elev*(-12.79+elev*0.711))),-20.772/te)))
  ## Corrected Zenith [90-AG]
  zenith-r/3600
}
