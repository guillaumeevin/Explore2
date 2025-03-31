# Guillaume Evin
# Explore2
# build data frame indicating the scenarios

dfScenario = data.frame(rbind(c("rcp26","CNRM-CM5-LR","ALADIN63"),
                        c("rcp26","EC-EARTH","HadREM3-GA7-05"),
                        c("rcp26","EC-EARTH","RACMO22E"),
                        c("rcp26","EC-EARTH","RCA4"),
                        c("rcp26","HadGEM2-ES","HadREM3-GA7-05"),
                        c("rcp26","HadGEM2-ES","RegCM4-6"),
                        c("rcp26","MPI-ESM-LR","CCLM4-8-17"),
                        c("rcp26","MPI-ESM-LR","RegCM4-6"),
                        c("rcp26","MPI-ESM-LR","REMO"),
                        c("rcp26","NorESM1-M","REMO"),
                        c("rcp45","CNRM-CM5-LR","ALADIN63"),
                        c("rcp45","EC-EARTH","RACMO22E"),
                        c("rcp45","EC-EARTH","RCA4"),
                        c("rcp45","HadGEM2-ES","CCLM4-8-17"),
                        c("rcp45","IPSL-CM5A-MR","RCA4"),
                        c("rcp45","MPI-ESM-LR","CCLM4-8-17"),
                        c("rcp45","MPI-ESM-LR","REMO"),
                        c("rcp45","NorESM1-M","HIRHAM5"),
                        c("rcp45","NorESM1-M","REMO"),
                        c("rcp85","CNRM-CM5-LR","ALADIN63"),
                        c("rcp85","CNRM-CM5-LR","HadREM3-GA7-05"),
                        c("rcp85","EC-EARTH","HadREM3-GA7-05"),
                        c("rcp85","EC-EARTH","RACMO22E"),
                        c("rcp85","EC-EARTH","RCA4"),
                        c("rcp85","HadGEM2-ES","ALADIN63"),
                        c("rcp85","HadGEM2-ES","CCLM4-8-17"),
                        c("rcp85","HadGEM2-ES","HadREM3-GA7-05"),
                        c("rcp85","HadGEM2-ES","RegCM4-6"),
                        c("rcp85","IPSL-CM5A-MR","HIRHAM5"),
                        c("rcp85","IPSL-CM5A-MR","RCA4"),
                        c("rcp85","MPI-ESM-LR","CCLM4-8-17"),
                        c("rcp85","MPI-ESM-LR","RegCM4-6"),
                        c("rcp85","MPI-ESM-LR","REMO"),
                        c("rcp85","NorESM1-M","HIRHAM5"),
                        c("rcp85","NorESM1-M","REMO"),
                        c("rcp85","NorESM1-M","WRF381P")))
colnames(dfScenario) = c("rcp","gcm","rcm")


dfIndicMeteo = data.frame(rbind(c("tasAdjust","tas","DJF"),
                                c("tasAdjust","tas","JJA"),
                                c("prtotAdjust","pr","DJF"),
                                c("prtotAdjust","pr","JJA"),
                                c("evspsblpotAdjust","evspsblpot","JJA"),
                                c("evspsblpotAdjust","evspsblpot","DJF"),
                                c("RX1day","rx1day","year")))
colnames(dfIndicMeteo) = c("folder","var","season")