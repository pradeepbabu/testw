#workflow
#1. get airport details in australia
#2. compute distance to coast
#3. select random 10
#4. create dummy climatic areas
#5. stochastic simulation of rainfall condition with a 3 state Markov chain model
#6. get temperature pressure and humidity as function of latitude, elevation, distance from coast & season
#7. generate expected conditions for year: max, min temp & humidity, avg. daily pressure
#8. report from 10 stations once daily at sunrise
#9. Not done: for more frequent reporting, impute from max min values

# get latitude, longitude, elevation, & timezones of airports
url <- "https://commondatastorage.googleapis.com/ckannet-storage/2012-07-09T214020/global_airports.csv"
download.file(url,file.path("~","iata.csv"))
#rm("url")
iata<-read.csv("iata.csv")
#str(iata)
names(iata)[names(iata) == 'iata_faa'] <- 'iata'
#selecting mostly mainland australia
iata<-subset(iata,country=="Australia"&iata!=""&
               longitude < 153.82&longitude > 112.93&
               latitude < -10.77&latitude > -39.06)
iata<-iata[!grepl("Island",iata$name),]

#dropping unused levels from original dataset 
iata<-droplevels(iata)
#sort & no.
iata[with(iata,order(name)),] ->iata
iata$airport_id<-c(1:length(iata$airport_id))
#str(iata)
#making a UTC+_ timezone for later use
iata$timezone<-paste("UTC+",iata$zone,sep="")
iata$timezone<-sub(iata$timezone,pattern = ".5",replacement = ":30")

#computing distance to coast
#getting coordinates of coastline
library(raster)
library(sp)
aus=getData('GADM',country='AUS',level=0)
aus=as(aus,'SpatialLines') 
#retrieve spatial coordinates from Spatial object
coast<-coordinates(aus)
coast<-sapply(coast,function(x) do.call("rbind",x))
coast<-matrix(coast,ncol=2)
dim(coast)
#1279197 coordinates of coastline, not needed for toy model
#selecting every 1000 points
coast<-coast[seq.int(from = 1, to = nrow(coast), by = 1000),]
#create new variable for distance to coast
iata$coastdist<-rep(NA,nrow(iata))
#assign values to coastdist variable for each airport
#by computing distance to coast & taking minimum distance
pts<-matrix(c(iata$longitude,iata$latitude),byrow = F,ncol=2)
for (i in 1:nrow(pts)){
  iata$coastdist[i]<-round(min(spDistsN1(coast,pts[i,],longlat = T)),0)
}
rm(list=c("url","coast","coastdist"))
#select random 10 airports
#set seed for reproducibility
set.seed(123)  
#select random 10  
iata10<-iata[sample(nrow(iata),10),]
#check distribution of selected 10 airports 
library(maps)
library(mapdata)
map('worldHires','Australia',ylim=c(-40,-5),xlim=c(100,170))
points(iata10$longitude,iata10$latitude)

# Define shape of 3 dummy geographical areas 
aridarea<-cbind(c(141.415444,153.264476,112.730486,113.215467),+ 
                  c(-38.862526,-22.62597,-19.046227,-35.948115))
temparea<-cbind(c(153.264476,141.415444,154.605717),+
                  c(-22.62597,-38.862526,-39.24904))
monsarea<-cbind(c(153.264476,153.264476,112.730486,112.730486),+
                  c(-10.582,-22.62597,-19.046227,-10.582)) 

#point in polygon check
pnts<-cbind(iata10$longitude,iata10$latitude)
#plot to check distribution if needed
#plot(rbind(pnts,arid,temp,mons))
#polygon(arid)
#polygon(mons)
#polygon(temp)

#getting climatic area type of weather stations
library(SDMTools)
iata10[pnt.in.poly(pnts,aridarea)$pip==T,"type"]<-"arid"
iata10[pnt.in.poly(pnts,temparea)$pip==T,"type"]<-"temperate"
iata10[pnt.in.poly(pnts,monsarea)$pip==T,"type"]<-"monsoon"

#### Stochastic simulation of daily climate 
##First order 3 state Markov chain 
##Transition probabilites assumed to homogenous by region within season 
##Because spatial correlation patterns of rainfall events may change monthly and
##seasonally (e.g., from a convective rainfall season to a frontal rainfall
##season), therefore, multiple probability matrices have been used


##http://www.australia.gov.au/about-australia/australian-story/austn-weather-and-the-seasons
##Australia has six climatic zones and this translates as two main seasonal
##patterns. There is a Summer / Autumn / Winter / Spring pattern in the
##Temperate zone, also affecting the Desert and the Grassland climatic zones
##and, a Wet / Dry pattern in the tropical north which includes the Equatorial,
##Tropical and sub-tropical zones. 

##More detailed climate zone classifications at: 
##Peel, M.C., Finlayson, B. L., and McMahon, T. A.: Updated world map of the Köppen-Geiger climate classification,
##Hydrol. Earth Syst. Sci., 11, 1633-1644, doi:10.5194/hess-11-1633-2007, 2007. 

##Kottek, M., J. Grieser, C. Beck, B. Rudolf, and F. Rubel, 2006: World Map of
##the Köppen-Geiger climate classification updated. Meteorol. Z., 15, 259-263.
##DOI: 10.1127/0941-2948/2006/0130. 
##http://koeppen-geiger.vu-wien.ac.at/present.htm


#Assinging patterns to 3 climatic area types created earlier 
#          Summer Autumn Winter Spring
#Monsoon   Wet    Wet     Dry     Dry
#Arid      Dry    Dry     Dry     Dry
#Temperate Dry    Dry     Wet     Dry

#e.g.3 state Markov chain
#        sunny cloudy  rain
#sunny   0.7   0.20    0.10
#cloudy  0.3   0.40    0.30
#rain    0.2   0.45    0.35
statenames<-c("sunny","cloudy","rain")
#Assign probablity matrices to various conditions
MonsoonWet<-matrix(data = c(0.30, 0.3,0.4,
                            0.2,0.4, 0.4,
                            0.2,0.4,0.4),byrow = T, nrow = 3,
                   dimnames=list(statenames, statenames))
MonsoonDry<-matrix(data = c(0.70,0.2,0.1,
                            0.3,0.4,0.3,
                            0.2,0.45,0.35),byrow = T, nrow = 3,
                   dimnames=list(statenames, statenames))
Arid<-matrix(data = c(0.80,0.15,0.05,
                      0.6,0.3,0.1,
                      0.2,0.6,0.2),byrow = T, nrow = 3,
             dimnames=list(statenames, statenames))
TemperateWinter<-matrix(data = c(0.3,0.4,0.3,
                                 0.3,0.3,0.4,
                                 0.2,0.4,0.4),byrow = T, nrow = 3,
                        dimnames=list(statenames, statenames))
TemperateOther<-matrix(data = c(0.70,0.2,0.1,
                                0.4,0.4,0.2,
                                0.4,0.4,0.2),byrow = T, nrow = 3,
                       dimnames=list(statenames, statenames))


library("markovchain")
#set seed for reproducibility
set.seed(1)  
#generating a random number from a uniform distribution
num=runif(1,0,1)
#initial weather conditions, calcualted together for ease of use. else assign different seeds
if (num<0.3) {            
  MonsoonWetStart <- c(1,0,0)
} else if (num < 0.5) {
  MonsoonWetStart<-c(0,1,0)
} else {
  MonsoonWetStart<-c(0,0,1)
}

if (num<0.65) {            
  MonsoonDryStart = c(1,0,0)
} else if (num < 0.85) {
  MonsoonDryStart = c(0,1,0)
} else {
  MonsoonDryStart = c(0,0,1)
}

if (num<0.7) {            
  GenericDryStart = c(1,0,0)
} else if (num < 0.9) {
  GenericDryStart = c(0,1,0)
} else {
  GenericDryStart = c(0,0,1)
}

if (num<0.3) {            
  GenericWetStart = c(1,0,0)
} else if (num < 0.5) {
  GenericWetStart = c(0,1,0)
} else {
  GenericWetStart = c(0,0,1)
}

#create markov chain structures for sunny, cloudy, rain
MonsoonWetMarkov<-new("markovchain",states=statenames,transitionMatrix=MonsoonWet)
MonsoonDryMarkov<-new("markovchain",states=statenames,transitionMatrix=MonsoonDry)
AridMarkov<-new("markovchain",states=statenames,transitionMatrix=Arid)
TemperateWetMarkov<-new("markovchain",states=statenames,transitionMatrix=TemperateWinter)
TemperateDryMarkov<-new("markovchain",states=statenames,transitionMatrix=TemperateOther)

#predicting rain for 365 days
raingen <- function(type = "monsoon") {
  condition <- data.frame(expand.grid(day = 1:365))
  names(condition) <- c("day")
  condition[, c("condition")] <- "sunny"
  #calculate weather states as per Markov Chain Model
  #Monsoon Wet season runs from December to May
  if (type == "monsoon") {
    #150 days+ Jan 1
    condition[1, "condition"] <- statenames[MonsoonWetStart]
    condition[1:151, "condition"] <-
      markovchainSequence(
        n = 150,
        MonsoonWetMarkov,
        t0 = statenames[MonsoonWetStart],
        include.t0 = T
      )
    #182 days+ Jun 1
    condition[152, "condition"] <- statenames[MonsoonDryStart]
    condition[152:334, "condition"] <-
      markovchainSequence(
        n = 182,
        MonsoonDryMarkov,
        t0 = statenames[MonsoonDryStart],
        include.t0 = T
      )
    #30 days+ Dec 1
    condition[335, "condition"] <- statenames[MonsoonWetStart]
    condition[335:365, "condition"] <-
      markovchainSequence(
        n = 30,
        MonsoonWetMarkov,
        t0 = statenames[MonsoonWetStart],
        include.t0 = T
      )
  } else if (type == "arid") {
    #Arid weather
    condition[1, "condition"] <-
      statenames[GenericDryStart] #same all year
    condition[1:365, "condition"] <-
      markovchainSequence(n = 364,
                          AridMarkov,
                          t0 = statenames[GenericDryStart],
                          include.t0 = T)
  } else {
    #Temperate weather
    condition[1, "condition"] <-
      statenames[GenericDryStart] #all year, except winter rain
    condition[1:365, "condition"] <-
      markovchainSequence(
        n = 364,
        TemperateDryMarkov,
        t0 = statenames[GenericDryStart],
        include.t0 = T
      )
    #change winter 92days+ Jun1
    condition[152, "condition"] <- statenames[GenericWetStart]
    condition[152:244, "condition"] <-
      markovchainSequence(
        n = 92,
        TemperateWetMarkov,
        t0 = statenames[GenericWetStart],
        include.t0 = T
      )
  }
  #View(condition)
  #changing from wide to long format
  return(condition)
}


#making a table with expected conditions for 365 days
#creating 365 days for each airport
#day: 1:365 day of year; leap year not accounted for
calendar<-data.frame(expand.grid(day=1:365,iata=iata10$iata)) 
library(plyr)
calendar<-join(calendar,iata10,by="iata")

calendar$date<-as.Date(calendar$day - 1, origin = "2016-01-01") 
calendar$month<-strftime(calendar$date,"%B")
#Assign seasons
##Summer: December to February
##Autumn: March to May
##Winter: June to August
##Spring: September to November
month<-c("January","February","March",
         "April","May","June",
         "July","August","September",
         "October","November","December")
season<-c(rep("Summer",2),rep("Autumn",3),
          rep("Winter",3),rep("Spring",3),"Summer")
seasonmonth<-data.frame(cbind(month,season))
names(seasonmonth)<-c("month","season")
calendar<-join(calendar,seasonmonth,by="month")

#new variables to be calculated
#listing out variables for clarity 
#sunrise: time of sunrise
#sunset: time of sunset
#noon: time of maximum solar angle 
#Tmax: maximum temperature
#Tmin: minimum temperature
#Tmax_time: time of maximum temperature
#Tmin_time: time of minium temperature
#Hmax: maximum rel. humidity
#Hmin: minimum rel. humidity
#Hmax_time: time of maximum rel. humidity
#Hmin_time: time of minimum rel. humidity
#condition: sunny, rainy, cloudy
#Tmax_adj: max temperature adjusted for rain
#Tmin_adj: min temperature adjusted for rain

calendar[,c("sunrise","sunset","noon","pressure",
            "Tmax","Tmin","Tmax_time","Tmin_time",
            "Hmax","Hmin","Hmax_time","Hmin_time")]<-NA


#get sunrise sunset times in local time for each airport
#calculate temperature & humidity values
#Temperature may be affected by:
#Sunshine, Latitude, Altitude, Aspect, Sea Proximity and Temperature, Ocean Currents
#Sea temperature e.g. 'El Nino' not used here. Aspect not used here.
library(RAtmosphere)

##Hadley's 3 cell model
##http://www.atmo.ttu.edu/schroeder/ATMO_1300/Notes/chapter8.pdf
##Strong solar heating leads to rising air near equator, air descends at 20°-30° latitude to form
##subtropical highs.
## semipermanent pressure cells over central australia in July and further south in January


fill.calendar <- function(calendar,meanerror=0,sderror=5) {
  err=rnorm(nrow(calendar),meanerror,sderror)
  for (i in 1:nrow(calendar)) {
    day <- calendar[i, "day"]
    lat <- calendar[i, "latitude"]
    long <- calendar[i, "longitude"]
    ele <- calendar[i, "altitude"]
    coastdist <- calendar[i, "coastdist"]
    mon <- as.numeric(strftime(calendar$date[i], "%m"))
    sunrise <- suncalc(d = day,  Lat = lat, Long = long,UTC = F)
    sunrise$sunrise[1] -> rise
    rise -> calendar$sunrise[i]
    sunrise$sunset[1] -> set
    set -> calendar$sunset[i]
    noon <- rise + (set - rise) / 2
    calendar$noon[i] <- noon
    #sine curve-like season effect
    seasoneffect <- 12 * sin((mon + 1) * pi / 6)
    #temperature in Kelvins
    #temperature & temperature range decrease with inceasing latitude & elevation
    #& increases with distance from coast & seasonal effect
    #adds a normally distributed random error 
    error=err[i]
    Tmax<-(
      300 - 0.2 * abs(lat) - 5 * (ele / 1000) +
        1.5 * (coastdist / 100) + seasoneffect +
        seasoneffect * coastdist / 100+error
    )
    Tmin<-(285 - 0.2 * abs(lat) - 5 * (ele / 1000) +
             1.5 * (coastdist / 100) + seasoneffect -
             seasoneffect * coastdist / 100+error)
    
    calendar$Tmax[i] <- Tmax
    calendar$Tmin[i] <- Tmin
    
    #assuming peak temperature in afternoon & lowest temperature at sunrise
    calendar$Tmax_time[i] <- noon + 2
    calendar$Tmin_time[i] <- rise
    ##http://www.bom.gov.au/jsp/ncc/climate_averages/relative-humidity/index.jsp?maptype=1&period=jan#maps
    #Humidity decreases with distance from coast
    #Humidity is more in lower latitudes in winter
    Hmax<-min(round(80 - .03 * coastdist-seasoneffect * lat / 10, 0)-error,100)
    Hmin<-round(Hmax-((Tmax-Tmin)/Tmax),0)
    calendar$Hmin[i]<-Hmin
    calendar$Hmax[i] <-Hmax
    #Assuming humidity is maximum at sunrise
    calendar$Hmax_time <- rise
    calendar$Hmin_time<-noon+2
    #pressure max at 30 deg lat with more decrease towards south pole &
    #seasonal variation & random error
    if (lat>30){
      deltap=abs(30-lat)
    } else{
      deltap=abs(30-lat)/2
    }
    calendar$pressure[i]<-round((1000-deltap+seasoneffect+error),0)
  }
  
  # for (iata in calendar$iata){
  #type<-calendar[iata,type][1]
  #  calendar$condition
  # }
  return(calendar)
}

calendar<-fill.calendar(calendar,meanerror = 0,sderror = 6)
#View(calendar)

#tempK<-273
#tempC<-tempK-273.15

#generating rain condition using condition function defined above
for (iata in iata10$iata){
  type = iata10$type[iata10$iata == iata]
  status <- raingen(type)
  for (day in 1:365) {
    calendar$condition[calendar$iata == iata &
                         calendar$day == day] <- status[day,"condition"]
  }
}


#report Station Local Time Conditions Temperature Pressure Humidity
#SYD|-33.86,151.21,39|2015-12-23T12:35:37Z|Sunny|+39.4|1114.1|12
#function reports once a day at sunrise
# to report more frequently, will have to impute values from max temperature, min temperature & timings thereof

reporter<-function(calendar=calendar,iata10=iata10){
  for(iata in iata10$iata){
    day<-strftime(Sys.time(),format="%j")
    temp<-calendar[calendar$iata==iata&calendar$day==day,]
    lat<-temp$latitude
    long<-temp$longitude
    ele<-temp$altitude
    localtime<-temp$sunrise
    h<-floor(localtime)
    m<-round((localtime %% 1)*60,2)
    localtime<-paste(h,m,sep=":")
    condition<-temp$condition
    temperature<-round(temp$Tmax-273.5,1)
    if(condition=="rain"&temperature<0){
      condition<-"snow"
    }
    pressure<-round(temp$pressure,0)
    humidity<-round(temp$Hmax,0)
    date<-temp$date
    time<-temp$time
    writeLines(strwrap(paste(iata,"|",lat,",",long,",",ele,"|",date,"T",localtime,
                             "|",condition,"|",temperature,"|",
                             round(pressure,0),"|",round(humidity,0),sep="")))
  }
}
reporter(calendar,iata10)