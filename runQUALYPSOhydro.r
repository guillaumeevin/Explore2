# Guillaume Evin
# Explore2

library(fst)
library(CFtime)
library(QUALYPSO)
library(abind)
library(plyr)
acomb <- function(...) abind(..., along=3)

# parallel computation and monitor progress
library(doSNOW)
cl <- makeSOCKcluster(30)
registerDoSNOW(cl)

##########################################
# read selection of scenarios and pixels #
##########################################

# Extract indices for all catchments/scenarios/years
ref_year = 1990
final_year = 2100
vecY = ref_year:final_year
nY = length(vecY)

# selection projections
proj_raw = read.csv("../indicHydro/projections_selection.csv")
sel = (proj_raw$BC=="ADAMONT"&proj_raw$HM%in%c("CTRIP","GRSD","ORCHIDEE","SMASH"))&
  proj_raw$EXP!="SAFRAN"
proj_sel = proj_raw[sel,]
scen = proj_sel[,c("GCM","RCM","EXP","HM")]
colnames(scen) = c("gcm","rcm","rcp","hm")
scen$rcp = revalue(scen$rcp, c("historical-rcp26" = "rcp26", "historical-rcp45" = "rcp45","historical-rcp85" = "rcp85"))
nS = nrow(scen)

# selection stations: 1735 stations where CTRIP, GRSD, ORCHIDEE and SMASH have produced
# simulations except 4 stations where CTRIP fails to produce non-zero streamflows
stations_raw = read.csv("../indicHydro/stations_selection.csv")
stations_selection = readRDS("./vectorBasins.rds")
sel = stations_raw$code%in%stations_selection
stations_sel = stations_raw[sel,]
code_sel = stations_sel$code
nBas = nrow(stations_sel)

# path to netcdf files
path_data = "../indicHydro/"

indicHydro = c("QA","QJXA","QMNA")
for(indic in indicHydro){
  
  # extract projections
  Y <- foreach(is=1:nS,.packages = c("fst"), .combine='acomb', .multicombine=TRUE) %dopar% {
    proj = proj_sel[is,]
    out = read_fst(paste0("../indicHydro/",proj$EXP,"/",proj$GCM,"/",proj$RCM,"/ADAMONT/",
                          proj$HM,"/",indic,".fst"))
    colSt = out$code
    colY = as.numeric(format(out$date,"%Y"))
    colQ = out[[indic]]
    
    # fill array
    Yis = matrix(nrow=nBas,ncol=nY)
    for(iy in 1:nY){
      y = vecY[iy]
      zz = colY==y
      st = colSt[zz]
      q = colQ[zz]
      
      iq = match(code_sel,st)
      Yis[,iy] = q[iq]
    }
    Yis
  }
  
  
  # prepare inputs for QUALYPSO
  typechangeVar="rel"
  spar=1.1
  probCI = 0.9
  X=vecY
  Xfut=seq(from=1990,to=2100,by=5)
  
  ################ LOOP OVER THE PIXELS ################ 
  
  # create folders if needed
  fQ = paste0("../QUALYPSOOUT/Hydro/")
  if (!file.exists(fQ))  dir.create(fQ)
  fQI = paste0(fQ,indic,"/")
  if (!file.exists(fQI))  dir.create(fQI)
  
  # call QUALYPSO
  # monitor progress
  pb <- txtProgressBar(min=1, max=nBas, style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  foreach(iBas=1:nBas, .packages = c('QUALYPSO'), .options.snow=opts) %dopar% {
    # for one pixel
    Yb = t(Y[iBas,,])
    
    
    # list of options
    listOption = list(args.smooth.spline=list(spar=spar),typeChangeVariable=typechangeVar,probCI=probCI)
    
    # call QUALYPSO
    QOUT = QUALYPSO(Y=Yb, #one Y and run per pixel because otherwise we cannot have several future times
                    scenAvail=scen,
                    X=vecY,
                    Xfut=Xfut,
                    listOption=listOption)
    
    QOUT$listOption=NA #to not store twice the same information
    QOUT$RESERR=NA
    QOUT$CHANGEBYEFFECT=NA
    saveRDS(QOUT,file=paste0(fQI,code_sel[iBas],".rds"))
  }
}