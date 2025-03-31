# Guillaume Evin
# Explore2

library(ncdf4)
library(CFtime)

setwd("D:/PROJET/")
path_data="D:/PROJET/Explore2/data/Rx1D/"

##########################################
# read selection of scenarios & catchments #
##########################################

# list runs
scenAvail = readRDS("D:/PROJET/Explore2/PAPIER/QUALYPSO/QUALYPSOOUT/meteo_France/scenAvail.rds")
dfPixel = readRDS("D:/PROJET/Explore2/PAPIER/QUALYPSO/QUALYPSOOUT/meteo_France/dfPixel.rds")



#for(iindic in 1:nrow(dfIndic)){
# choose one variable / one indic
#var = dfIndic$var[iindic]
#indic = dfIndic$indic[iindic]
var = "rx1day"



# Extract indices for all catchments/scenarios/years
ref_year = 1990
final_year = 2100
vecY = ref_year:final_year
nY = length(vecY)
nS = nrow(scenAvail)

# indicateurs
Y = array(dim=c(nbas,nS,nY))

for(is in 1:nS){
  r = scenAvail[is,]
  typeagg = strsplit(indic,"_")[[1]][1]
  peragg = strsplit(indic,"_")[[1]][2]
  file_data = list.files(paste0(path_data,var,"/"),full.names=T,
                         pattern=glob2rx(paste0(var,"*",r$rcp,"*",r$gcm,"*",r$rcm,"*",r$bc,"*",typeagg,"*",peragg,"*")))
  
  file_data = paste0(path_data,"rx1day_year_ICHEC-EC-EARTH_KNMI-RACMO22E_ADAMONT_rcp26_")
  
  
  
  # get variables
  ncin <- nc_open(file_data)
  
  # get and convert time
  time <- ncvar_get(ncin,"time")
  tunits <- ncatt_get(ncin,"time","units")
  vecTimeCf <- CFtime(tunits$value, calendar = "proleptic_gregorian", time) # convert time to CFtime class
  vecTimeDates <- as_timestamp(vecTimeCf, format = "timestamp")
  
  v = var.get.nc(nc,var)
  time = var.get.nc(nc,"time")
  att.get.nc(nc, "time","units")
  cf <- CFtime(att.get.nc(nc, "time","units"), att.get.nc(nc, "time","calendar"), time)
  vecyear <- as.numeric(format(as.Date(CFtimestamp(cf)),"%Y"))
  close.nc(nc)
  
  # fill array
  iY = match(vecY,vecyear)
  Y[,is,] = v[,iY]
}



# prepare inputs for QUALYPSO
if(var=="tasAdjust"){
  typechangeVar="abs"
  spar=1
}else{
  typechangeVar="rel"
  spar=1.1
}
probCI = 0.9
X=vecY
Xfut=seq(from=1990,to=2100,by=5)

################ LOOP OVER THE CATCHMENTS ################ 

# create folders if needed
fQ = paste0("C:/Explore2/QUALYPSO/",typeRes,"/")
if (!file.exists(fQ))  dir.create(fQ)
fQI = paste0("C:/Explore2/QUALYPSO/",typeRes,"/",var,"_",indic,"/")
if (!file.exists(fQI))  dir.create(fQI)

# call QUALYPSO
for(ibv in 1:nbas){
  # for one catchment
  Ybv = Y[ibv,,]
  
  ## climate response
  CLIMATERESPONSE = prepare_clim_resp(Y = Ybv, X = X, Xfut = Xfut, typeChangeVariable=typechangeVar, spar=spar,
                                      type="spline",scenAvail=scenAvail)
  saveRDS(CLIMATERESPONSE,file=paste0(fQI,vecBassins[ibv],"_CR.rds"))
  #plot(-1,-1,xlim=range(X),ylim=range(clim_resp$phiStar))
  #for(i in 1:nrow(clim_resp$phiStar)) lines(Xfut,clim_resp$phiStar[i,],col=i)
  
  # list of options
  listOption = list(spar=spar,typeChangeVariable=typechangeVar,probCI=probCI,climResponse=CLIMATERESPONSE)
  
  # call QUALYPSO
  QOUT = QUALYPSO(Y=Ybv, #one Y and run per pixel because otherwise we cannot have several future times
                  scenAvail=scenAvail,
                  X=X,
                  Xfut=Xfut,
                  listOption=listOption)
  lS = QOUT$listScenarioInput
  
  # reconstructed climate responses and replace by available values
  phiReconst=reconstruct_chains(QOUT)
  phiStar = CLIMATERESPONSE$phiStar
  
  #########  Missing scenarios   #########
  vScenComp <- apply(lS$scenComp, 1, paste, collapse='.')
  vScenAvail <- apply(lS$scenAvail, 1, paste, collapse='.')
  iMatchScen = match(vScenAvail,vScenComp)
  phiReconst[iMatchScen,]=phiStar
  
  # add estimates of quantiles from the reconstructed ensemble
  ieff_rcp = which(colnames(scenAvail)=="rcp")
  for(rcp in unique(scenAvail$rcp)){
    ieff_this_rcp=which(lS$listEff[[ieff_rcp]]==rcp)
    chg_mean = QOUT$MAINEFFECT$rcp$MEAN[,ieff_this_rcp] + QOUT$GRANDMEAN$MEAN
    
    # compute a multiplicative factor SDtot/SD(phi*) to match the standard deviation for each RCP
    # obtained from QUALYPSO
    sd.qua = sqrt(QOUT$TOTALVAR-QOUT$INTERNALVAR-QOUT$EFFECTVAR[,ieff_rcp])
    if(any(is.na(sd.qua))){
      chg_mean[which(is.na(sd.qua))]=NA
    }
    
    # reconcstructed phiStar for this rcp
    idx_this_rcp=which(lS$scenComp[,ieff_rcp]==rcp)
    phiRCP = phiReconst[idx_this_rcp,]
    sd.emp = apply(phiRCP,2,sd)
    sd.emp[sd.emp==0] = 1 # avoid NaN for the reference year
    sd.corr = sd.qua/sd.emp
    phiStar.corr = phiRCP*t(replicate(dim(phiRCP)[1],sd.corr))
    chg_q5 = apply(phiStar.corr,2,quantile,probs = (1-probCI)/2,na.rm=T)
    chg_q95 = apply(phiStar.corr,2,quantile,probs = 0.5+probCI/2,na.rm=T)
    
    QOUT[[rcp]] = rbind(chg_q5,chg_mean,chg_q95)
  }
  
  
  QOUT$listOption=NA #to not store twice the same information
  QOUT$RESERR=NA
  QOUT$CHANGEBYEFFECT=NA
  QOUT$CLIMATERESPONSE=NA
  saveRDS(QOUT,file=paste0(fQI,vecBassins[ibv],"_Q.rds"))
}
#}