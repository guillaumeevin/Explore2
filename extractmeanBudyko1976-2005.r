library(ncdf4)
library(CFtime)
library(tidyverse)
library(mapproj)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(sf)

file.etp = "../SAFRAN/safran_new_ETP_grid.nc"
file.Ptot = "../SAFRAN/safran_new_Ptot_grid.nc"
ref_period = c(1976, 2005)
periodyear = ref_period[1]:ref_period[2]

#Ouverture du fichier SAFRAN etp
etpsafran <- nc_open(file.etp)
etpsafran_var <- ncvar_get(etpsafran, "etp")
time = ncvar_get(etpsafran,"time")
tunits <- ncatt_get(etpsafran,"time","units")
vecTimeCf <- CFtime(tunits$value, calendar = "proleptic_gregorian", time) # convert time to CFtime class
vecTimeDates <- as_timestamp(vecTimeCf, format = "timestamp")
vecyearETP <- as.numeric(format(as.Date(vecTimeDates),"%Y"))


#Récupération des coordonnées 
safranX <- ncvar_get(etpsafran, "lon")
safranY <- ncvar_get(etpsafran, "lat")

#Ouverture du fichier SAFRAN Précipitations totales 
PrSafran <- nc_open(file.Ptot)
PrSafran_var <- ncvar_get(PrSafran, "prtotAdjust")
time = ncvar_get(PrSafran,"Time")
tunits <- ncatt_get(PrSafran,"Time","units")
vecTimeCf <- CFtime(tunits$value, calendar = "proleptic_gregorian", time) # convert time to CFtime class
vecTimeDates <- as_timestamp(vecTimeCf, format = "timestamp")
vecyearPtot <- as.numeric(format(as.Date(vecTimeDates),"%Y"))

#Calcul de l'index de Budyko
iPerETP = vecyearETP%in%periodyear
meanetp = apply(etpsafran_var[,,iPerETP],c(1,2),mean)

iPerPtot = vecyearPtot%in%periodyear
meanPtot = apply(PrSafran_var[,,iPerPtot],c(1,2),mean)

Budyko <- meanetp/meanPtot


lBudyko = list(lon=safranX,lat=safranY,meanetp=meanetp,meanPtot=meanPtot,Budyko=Budyko)
saveRDS(lBudyko,file="../SAFRAN/Budyko.rds")
