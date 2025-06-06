# Guillaume Evin
# Explore2
# Summary of the changes by hydrological regime

library(patchwork)
library(dplyr)
library(tidyr)
library(ungeviz)
library(reshape2)
library(multipanelfigure)

source("./lib_functions.r")


##############
# Resources #
##############
path_sig = "../SIG/"
path_river=paste0(path_sig,"processed/CoursEau_idx1_wgs84.shp")
path_fr=paste0(path_sig,"raw/IGN/contours_FR/gadm36_FRA_0.shp")
background_for_maps(path_river,path_fr)



##########################################
# read selection of scenarios & catchments #
##########################################
# selection stations: 1735 stations where CTRIP, GRSD, ORCHIDEE and SMASH have produced
# simulations except 4 stations where CTRIP fails to produce non-zero streamflows
stations_raw = read.csv("../indicHydro/stations_selection.csv")
stations_selection = readRDS("./vectorBasins.rds")
sel = stations_raw$code%in%stations_selection
stations_sel = stations_raw[sel,]
code_sel = stations_sel$code

# read regime
dfAllRegime = read.csv2("./dataTotSAF_regimeAuMoins4Modele.csv") # 2500
iBasinReg = match(code_sel,dfAllRegime$code)
dfRegime = dfAllRegime[iBasinReg,]
dfRegime$hydro_regime = factor(dfRegime$hydro_regime, 
                               levels=c("PTC","PC","P","PMC","PN","NP","N"),
                               labels = c("HCP","CP","P","NCP","PN","NP","N"))
nPoints = nrow(dfRegime)

colRegime = c("HCP" = "#148530ff",
              "CP" ="#21de50ff",
              "P" = "#7aeb96ff",
              "NCP" = "#d3f8dcff",
              "PN" = "#88c4ffff",
              "NP" = "#2291ffff",
              "N" = "#0027a2ff")

########################################################
# end of century
horiz = 2085


########################################################
# plot 1: q50 climate change response by regime
########################################################
rcp_names=c("rcp26","rcp45","rcp85")
rcp.labs <- c("RCP2.6", "RCP4.5", "RCP8.5")
names(rcp.labs) <- rcp_names

for(indic in c("QA","QJXA","QMNA")){
  rcp = "rcp85"
  #  for(rcp in c("rcp26","rcp45","rcp85")){
  
  # output from QUALYPSO
  vecQ = paste0("../QUALYPSOOUT/Hydro/",indic,"/",dfRegime$code,".rds")
  
  # read outputs
  QOUT = readRDS(vecQ[1])
  
  # index in scenAvail corr to this rcp
  ir = which(QOUT$listScenarioInput$scenAvail$rcp==rcp)
  
  ###### map.3quant.3rcp.1horiz #####
  Xfut=QOUT$Xfut
  idx_Xfut=which(Xfut==horiz)
  
  ### loop over the basin
  chg_q50 = vector(length=nPoints)
  for(i in 1:nPoints){
    QOUT = readRDS(vecQ[i])
    phisf = QOUT$CLIMATERESPONSE$phiStar[ir,idx_Xfut]
    chg_q50[i] = quantile(phisf,probs=0.5)
  }  
  
  # fill df
  dfRegime$indic = indic
  dfRegime$rcp = rcp
  dfRegime$q50 = chg_q50
  
  if(indic=="QA"){#&rcp=="rcp26"){
    dfPlot = dfRegime
  }else{
    dfPlot = rbind(dfPlot,dfRegime)
  }
  #  }
}

dfPlot$indic = factor(dfPlot$indic, levels=c("QMNA","QA","QJXA"))

pltChange = ggplot(dfPlot,aes(hydro_regime,q50))+geom_boxplot(aes(fill=hydro_regime),outlier.shape = NA)+
  scale_y_continuous(limits = c(-0.75, 0.75),expand = expansion(add = 0)) +
  scale_fill_manual(name = "Regime", values = colRegime)+
  facet_grid(rcp ~ indic,labeller = labeller(rcp=rcp.labs)) + theme_bw() +
  theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
        axis.text.x=element_text(size = 14),axis.text.y=element_text(size = 14),
        strip.background=element_blank(),legend.title = element_text(face = "bold",size = 14),
        strip.text = element_text(face = "bold",size = 20), 
        legend.text = element_text(face = "bold",size = 11),plot.margin=grid::unit(c(0,0,0,0), "mm"),
        plot.title = element_text(size=20, face="bold")) + 
  ggtitle('A: Q50 CCR (%)')



########################################################
# plot 2: contribution by regime
########################################################

for(indic in c("QMNA","QA","QJXA")){
  
  # output from QUALYPSO
  vecQ = paste0("../QUALYPSOOUT/Hydro/",indic,"/",dfRegime$code,".rds")
  
  # read outputs
  QOUT = readRDS(vecQ[1])
  namesEff = colnames(QOUT$listScenarioInput$scenAvail)
  
  ###### map.3quant.3rcp.1horiz #####
  Xfut=QOUT$Xfut
  idx_Xfut=which(Xfut==horiz)
  
  ### loop over the basin
  pctEXP = pctGCM = pctRCM = pctHM = vector(length=nPoints)
  for(i in 1:nPoints){
    QOUT = readRDS(vecQ[i])
    DEC.raw = QOUT$DECOMPVAR[idx_Xfut,]
    DECOMP = as.data.frame(t(head(DEC.raw,n=(length(namesEff)+1))/(1-tail(DEC.raw,n=1))))*100
    pctEXP[i]= DECOMP$rcp
    pctGCM[i]= DECOMP$gcm
    pctRCM[i]= DECOMP$rcm
    pctHM[i]= DECOMP$hm
  }  
  
  # build df
  pctVtot = data.frame(RCP=pctEXP,GCM=pctGCM,RCM=pctRCM,HM=pctHM)
  
  # merge df for ggplot2
  for(source in c("RCP","GCM","RCM","HM")){
    dfRegime$indic = indic
    dfRegime$source = source
    dfRegime$pctVtot = pctVtot[[source]]
    if(indic=="QMNA"&source=="RCP"){
      dfPlot = dfRegime
    }else{
      dfPlot = rbind(dfPlot,dfRegime)
    }
  }
}

# see Boxplots_pctVtot.pdf
#plt = ggplot(dfPlot,aes(source,pctVtot))+geom_boxplot(aes(fill=hydro_regime),outlier.shape = NA)+
#  scale_fill_manual(name = "Regime", values = colRegime)+
#  facet_grid( hydro_regime ~ indic) + theme_bw() +
#  theme(strip.background=element_blank(),legend.title = element_text(face = "bold",size = 14),
#        legend.text = element_text(face = "bold",size = 11),plot.margin=grid::unit(c(0,0,0,0), "mm"))

dfPlot$indic = factor(dfPlot$indic, levels=c("QMNA","QA","QJXA"))
dfPlot$source = factor(dfPlot$source, levels=c("RCP","GCM","RCM","HM"))

pltANOVA = ggplot(dfPlot,aes(hydro_regime,pctVtot))+geom_boxplot(aes(fill=hydro_regime),outlier.shape = NA)+
  scale_y_continuous(limits = c(0, 63),expand = expansion(add = 0)) +
  scale_fill_manual(name = "Regime", values = colRegime)+
  facet_grid(source ~ indic,labeller = labeller(rcp=rcp.labs)) + theme_bw() +
  theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
        axis.text.x=element_text(size = 14),axis.text.y=element_text(size = 14),
        strip.background=element_blank(),legend.title = element_text(face = "bold",size = 14),
        strip.text = element_text(face = "bold",size = 20), 
        legend.text = element_text(face = "bold",size = 11),plot.margin=grid::unit(c(0,0,0,0), "mm"),
        plot.title = element_text(size=20, face="bold",margin = margin(20,0,0,0))) + 
  ggtitle('B: %CCRV')

########################################################
# merge plots
########################################################
plt = pltChange / pltANOVA + 
  plot_layout(heights = c(0.7, 2))

ggsave(filename = paste0("../FIGURES/Fig10_boxplots_regime.pdf"),
       plot=plt,device = "pdf",units="cm",height=30,width=30)


########################################################
# plot 3: GCM, RCM and HM effects
########################################################

for(indic in c("QMNA","QA","QJXA")){
  
  # output from QUALYPSO
  vecQ = paste0("../QUALYPSOOUT/Hydro/",indic,"/",dfRegime$code,".rds")
  
  # retrieve data
  QOUT = readRDS(vecQ[1])
  Xfut=QOUT$Xfut
  idx_Xfut=which(Xfut==horiz)
  nPoints = length(vecQ)
  
  
  # sources of uncertainty (GCM, RCM, HM)
  vec.name.eff = colnames(QOUT$listScenarioInput$scenAvail)
  for(name_eff in c("gcm","rcm","hm")){
    
    # retrieve effects for this type of effect (e.g. RCM)
    ieff=which(colnames(QOUT$listScenarioInput$scenAvail)==name_eff)
    effs.labs <- QOUT$listScenarioInput$listEff[[ieff]]
    neff = length(effs.labs)
    
    # retrieve effects
    mat.eff = matrix(nrow = nPoints,ncol=neff)
    for(i in 1:nPoints){
      QOUT = readRDS(vecQ[i])
      mat.eff[i,] = QOUT$MAINEFFECT[[name_eff]]$MEAN[idx_Xfut,]
    }
    
    colnames(mat.eff) <- effs.labs
    df <- as_tibble(mat.eff)
    df['indic'] = indic
    df['source'] = name_eff
    df['code'] = dfRegime$code
    df['regime'] = dfRegime$hydro_regime
    
    #pivot the data frame into a long format
    df.lg = df %>% pivot_longer(cols=all_of(effs.labs),
                                names_to='model',
                                values_to='effect')
    if(indic=="QMNA"&name_eff=="gcm"){
      dfPlot = df.lg
    }else{
      dfPlot = bind_rows(dfPlot,df.lg)
    }
  }
}

# colors RCM
lColRCM=c("ALADIN63"="#8dd3c7","HadREM3-GA7"="#ffffb3","RACMO22E"="#bebada",
          "RCA4"="#fb8072","RegCM4-6"="#80b1d3","CCLM4-8-17"="#fdb462",
          "HIRHAM5"="#b3de69","REMO"="#fccde5","WRF381P"="#d9d9d9")

# colors GCM
lColGCM=list("CNRM-CM5"="#377eb8","EC-EARTH"="#e41a1c","HadGEM2-ES"="#4daf4a",
             "IPSL-CM5A-MR"="#ff7f00","MPI-ESM-LR"="#984ea3","NorESM1-M"="#666666")

# colors HM
lColHM=list(CTRIP="#A88D72",GRSD="#619C6C",ORCHIDEE="#EFA59D",SMASH="#F6BA62")

dfPlot$indic = factor(dfPlot$indic, levels=c("QMNA","QA","QJXA"))
dfPlot$source = factor(dfPlot$source, levels=c("gcm","rcm","hm"), labels=c("GCM","RCM","HM"))

pltEffect = ggplot(dfPlot, aes(regime, effect, colour = model)) +
  scale_y_continuous(limits = c(-0.25, 0.25),expand = expansion(add = 0)) +
  geom_hpline(stat = "summary",fun="median",linewidth = 3) +
  scale_color_manual(values = c(lColGCM,lColRCM,lColHM)) +
  facet_grid(source ~ indic) + theme_bw()+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
        axis.text.x=element_text(size = 14),axis.text.y=element_text(size = 14),
        strip.background=element_blank(),legend.title = element_text(face = "bold",size = 14),
        strip.text = element_text(face="bold",size = 20), 
        legend.position="none")

ggsave(filename = "../FIGURES/Fig10_boxplots_effects_regime.pdf",
       plot=pltEffect,device = "pdf",units="cm",height=30,width=30)

# add legend
svg_legend = "../FIGURES/legend.png"

multi_panel_figure(width = 240, height = c(230, 50), columns = 1,
                   row_spacing = 0, column_spacing = 0) %>%
  fill_panel(pltEffect, label = "",row = 1,column=1) %>%
  fill_panel(svg_legend, label = "",scaling="fit", row=2, column=1) %>%
  save_multi_panel_figure(filename = "../FIGURES/Fig10_boxplots_effects_regime_withLegend.pdf")

