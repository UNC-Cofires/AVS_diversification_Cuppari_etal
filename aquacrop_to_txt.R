##evapotranspiration with syn_weather 

##ET0 calculation, reference evapotranspiration
lat=44.9429 #N, in decimal degrees
long=123.0351 #W
elevation=46.9 ##above sea level, m
a=.23 #reference albedo
sigma=4.903*10^-9 ##boltzmann

##sim data
syn_weather=read.csv("D:\\Research\\Synthetic_results\\Synthetic_weather\\syn_weather200yr_rain.csv")
syn_weather=syn_weather[,-c(1,2)]
syn_weather[,16]=syn_weather[,16]*25.4 ##inches to mm

for (i in 2:nrow(syn_weather)) {
  year=syn_weather[i,9]
  if (leap_year(year)==TRUE) {
    if (syn_weather[i,3] <32) {
      syn_weather[i,10]=syn_weather[i,3]
    } else if (syn_weather[i,3]<61) {
      syn_weather[i,10]=syn_weather[(i-31),3]
    } else if (syn_weather[i,3]<92) {
      syn_weather[i,10]=syn_weather[(i-60),3]
    } else if (syn_weather[i,3]<122) {
      syn_weather[i,10]=syn_weather[(i-91),3]
    } else if (syn_weather[i,3]<153) {
      syn_weather[i,10]=syn_weather[(i-121),3]
    } else if (syn_weather[i,3]<183) {
      syn_weather[i,10]=syn_weather[(i-152),3]
    } else if (syn_weather[i,3]<214) {
      syn_weather[i,10]=syn_weather[(i-182),3]
    } else if (syn_weather[i,3]<245) {
      syn_weather[i,10]=syn_weather[(i-213),3]
    } else if (syn_weather[i,3]<275) {
      syn_weather[i,10]=syn_weather[(i-244),3]
    } else if (syn_weather[i,3]<306) {
      syn_weather[i,10]=syn_weather[(i-274),3]
    } else if (syn_weather[i,3]<336) {
      syn_weather[i,10]=syn_weather[(i-305),3]
    } else {
      syn_weather[i,10]=syn_weather[(i-335),3]
    }
  } else { 
    
    if (syn_weather[i,3] <32) {
      syn_weather[i,10]=syn_weather[i,3]
    } else if (syn_weather[i,3]<60) {
      syn_weather[i,10]=syn_weather[(i-31),3]
    } else if (syn_weather[i,3]<91) {
      syn_weather[i,10]=syn_weather[(i-59),3]
    } else if (syn_weather[i,3]<121) {
      syn_weather[i,10]=syn_weather[(i-90),3]
    } else if (syn_weather[i,3]<152) {
      syn_weather[i,10]=syn_weather[(i-120),3]
    } else if (syn_weather[i,3]<182) {
      syn_weather[i,10]=syn_weather[(i-151),3]
    } else if (syn_weather[i,3]<213) {
      syn_weather[i,10]=syn_weather[(i-181),3]
    } else if (syn_weather[i,3]<244) {
      syn_weather[i,10]=syn_weather[(i-212),3]
    } else if (syn_weather[i,3]<274) {
      syn_weather[i,10]=syn_weather[(i-243),3]
    } else if (syn_weather[i,3]<305) {
      syn_weather[i,10]=syn_weather[(i-273),3]
    } else if (syn_weather[i,3]<335) {
      syn_weather[i,10]=syn_weather[(i-304),3]
    } else {
      syn_weather[i,10]=syn_weather[(i-334),3]
    }
  }
}

syn_weather$ET0=0
for (i in 1:nrow(syn_weather)) {
  tmax=syn_weather[i,4]
  tmin=syn_weather[i,5]
  T=(tmax+tmin)/2 ##avg temp, C
  Rs=syn_weather[i,8]*.0864 ##GHI converted from watts to MJ/m2day; 1 W m-2 = 0.0864 MJ m-2 day-1##
  u=syn_weather[i,7] ##mean wind speed m/s
  expTerm=.6108*exp((17.27*T)/(T+237.3))
  delta=(4098*(expTerm))/(T+237.3)^2
  P=101.3*((293-.0065*elevation)^5.26/293) ##atmospheric pressure
  gamma=.000665*P ##for average atmospheric pressure and atmospheric conditions (specific heat value of 1.01 * 10^-3 MJ/kgC)
  DT=delta/(delta+gamma*(1+.34*u))
  PT=gamma/(delta+gamma*(1+.34*u))
  TT=(900/(T+273))*u

  etmax=6.11*(10^(7.5*tmax/(237.7+tmax))) ##saturated vapor pressure##
  etmin=6.11*(10^(7.5*tmin/(237.7+tmin))) ##actual vapor pressure##
  
  e1=6.11*(exp((17.67*tmin)/(tmin+243.5)))
  es1=6.11*(exp((17.67*tmax)/(tmax+243.5)))
  
  rhMax=109-.35*tmax+11.4*syn_weather[i,16] ##regression equation
  #rhMax=etmax/etmin
  if (rhMax>100){rhMax=99}
  if (rhMax<0) {rhMax=1}
  rhMin=rhMax
  
  es=(etmax+etmin)/2
  ea=((etmin*rhMax/100)+(etmax*rhMin)/100)/2
  
  J=syn_weather[i,3] ##daynum
  dr=1+.033*cos(2*pi*J/365)
  dec=.409*sin(2*pi*J/365-1.39)
  
  latRad=(pi/180)*lat
  sunset=acos(-tan(latRad)*tan(dec))
  Gsc=.0820
  Ra=(24*60/pi)*Gsc*dr*(sunset*sin(latRad)*sin(dec)+cos(latRad)*cos(dec)*sin(sunset))
  Rso=(.75+(2*10^-5)*elevation)*Ra
  Rns=(1-a)*Rs
  Rnl=sigma*(((tmax+273.16)^4+(tmin+273.16)^4)/2)*(.34-.14*sqrt(ea))*(1.35*(Rs/Rso)-.35)
  Rn=Rns-Rnl
  Rng=.408*Rn
  etR=DT*Rng
  etW=PT*TT*(es-ea)
  et0=etW+etR
  syn_weather[i,17]=et0
  if (syn_weather[i,17]<1){
    syn_weather[i,17]=1
  }
}

max(syn_weather[,17])
min(syn_weather[,17])

##write txt for AquaCrop
aquacrop=syn_weather[,c(10,11,9,5,4,16,17)]
aquacrop=arrange(aquacrop,year,month,day)

fn <- "D:\\Research\\Synthetic_results\\Synthetic_weather\\syn_aquacrop.txt"
cat("%%%% ---------- Weather input time-series for AquaCropOS ---------- %%%%",  "\n",
    "%%%% Day Month Year MinTemp MaxTemp Precipitation ReferenceET %%%%", "\n", file=fn, sep="\t")

write.table(aquacrop, file = "D:\\Research\\Synthetic_results\\Synthetic_weather\\syn_aquacrop.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE,append=TRUE)

aquacrop2=aquacrop[1:54786,]
write.csv(aquacrop2,file="D:\\Research\\Synthetic_results\\Synthetic_weather\\syn_aquacrop2.csv")
