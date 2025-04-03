# Guillaume Evin
# Explore2

library(ncdf4)
library(CFtime)
library(QUALYPSO)

# parallel computation and monitor progress
library(doSNOW)
cl <- makeSOCKcluster(30)
registerDoSNOW(cl)

##########################################
# read selection of scenarios and pixels #
##########################################

# dfIndicMeteo and dfScenario
source("./buildDF.r")

# Extract indices for all catchments/scenarios/years
ref_year = 1990
final_year = 2100
vecY = ref_year:final_year
nY = length(vecY)
nS = nrow(dfScenario)

# coordinates France/SAFRAN grid: list of matrices
lCoords = readRDS(file = "./coordMeteo.rds")
nLon = nrow(lCoords$lon)
nLat = ncol(lCoords$lon)
isPixel = lCoords$mask
indexPixel = which(isPixel==1,arr.ind = T)
nPixels = sum(isPixel)

# path to netcdf files
path_data = "../indicMeteo/"

for(iindic in 1:nrow(dfIndicMeteo)){
  rInd = dfIndicMeteo[iindic,]
  
  # choose one variable / one indic
  varlong = rInd$folder
  var = rInd$var
  season = rInd$season
  
  # indicateurs
  Y = array(dim=c(nLon,nLat,nS,nY))
  
  for(is in 1:nS){
    rSc = dfScenario[is,]
    file_data = list.files(paste0(path_data,varlong,"/"),full.names=T,
                           pattern=glob2rx(paste0(var,"*",rSc$rcp,"*",rSc$gcm,"*",rSc$rcm,"*ADAMONT*",season,"*")))
    
    # get variables
    nc <- nc_open(file_data)
    v = ncvar_get(nc,varlong)
    time = ncvar_get(nc,"time")
    tunits <- ncatt_get(nc,"time","units")
    vecTimeCf <- CFtime(tunits$value, calendar = "proleptic_gregorian", time) # convert time to CFtime class
    vecTimeDates <- as_timestamp(vecTimeCf, format = "timestamp")
    vecyear <- as.numeric(format(as.Date(vecTimeDates),"%Y"))
    nc_close(nc)
    
    # fill array
    iY = match(vecY,vecyear)
    Y[,,is,] = v[,,iY]
  }
  
  
  
  # prepare inputs for QUALYPSO
  if(var=="tas"){
    typechangeVar="abs"
    spar=1
  }else{
    typechangeVar="rel"
    spar=1.1
  }
  probCI = 0.9
  X=vecY
  Xfut=seq(from=1990,to=2100,by=5)
  
  ################ LOOP OVER THE PIXELS ################ 
  
  # create folders if needed
  fQ = paste0("../QUALYPSOOUT/Meteo/")
  if (!file.exists(fQ))  dir.create(fQ)
  fQI = paste0(fQ,var,"_",season,"/")
  if (!file.exists(fQI))  dir.create(fQI)
  
  # call QUALYPSO
  # monitor progress
  pb <- txtProgressBar(min=1, max=nLon, style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  foreach(iPixel=1:nPixels, .packages = c('QUALYPSO'), .options.snow=opts) %dopar% {
    # for one pixel
    i1 = indexPixel[iPixel,1]
    i2 = indexPixel[iPixel,2]
    
    Yp = Y[i1,i2,,]
    
    
    # list of options
    listOption = list(spar=spar,typeChangeVariable=typechangeVar,probCI=probCI)
    
    # call QUALYPSO
    QOUT = QUALYPSO(Y=Yp, #one Y and run per pixel because otherwise we cannot have several future times
                    scenAvail=dfScenario,
                    X=vecY,
                    Xfut=Xfut,
                    listOption=listOption)
    
    QOUT$listOption=NA #to not store twice the same information
    QOUT$RESERR=NA
    QOUT$CHANGEBYEFFECT=NA
    saveRDS(QOUT,file=paste0(fQI,i1,"_",i2,".rds"))
  }
}