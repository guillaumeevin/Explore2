# Guillaume Evin
# Explore2
# Effects hydro indicators
library(patchwork)
library(sp)

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
nBas = nrow(stations_sel)

# coordinates
exut_L93<-data.frame(x = as.numeric(stations_sel$XL93_m),y = as.numeric(stations_sel$YL93_m))
coordinates(exut_L93) <- c("x", "y")
proj4string(exut_L93) <- crs("+init=epsg:2154")#Lambert93
exut_L2=spTransform(exut_L93,crs("+init=epsg:27572"))#Lambert2
coordsL2 = coordinates(exut_L2)
colnames(coordsL2) = c("x","y")
coordsL2 = as.data.frame(coordsL2)

# France
zoom = "FR"

## map_3quant_3rcp_1horiz
unitSB = "[%]"

# lead time
horiz = 2085

# color scale
mypalette = binned_pal(scales::manual_pal(precip_10))

# breaks
mybreaks = seq(from=-0.5,to=0.5,length.out=11)
mylabels = as.character(round(mybreaks*100))
mylabels[1] = paste0("< ",mylabels[1])
mylabels[length(mylabels)] = paste0("> ",mylabels[length(mylabels)])

########################################################
# PLOT 1: GCM/RCM effects for QMNA, QA, QJXA
########################################################

## freq_col for scaling the color scale limits
for(indic in c("QMNA","QA","QJXA")){
  
  # output from QUALYPSO
  vecFileQUALYPSO = paste0("../QUALYPSOOUT/Hydro/",indic,"/",
                           code_sel,".rds")
  
  # retrieve data
  output.map.effect = get.output.map.effect(x=coordsL2$x,y=coordsL2$y,horiz=horiz,
                                            vecFileQUALYPSO=vecFileQUALYPSO)
  
  lPlt = list()
  for(name_eff in c("gcm","rcm")){
    
    # reorder names
    if(name_eff == "gcm"){
      effs.name.FIG <- c("CNRM-CM5-LR","EC-EARTH","HadGEM2-ES","MPI-ESM-LR",
                         "NorESM1-M","IPSL-CM5A-MR")
      ncol=3
    }else if(name_eff == "rcm"){
      effs.name.FIG <- c("ALADIN63","HadREM3-GA7-05","RACMO22E","RCA4","RegCM4-6",
                         "CCLM4-8-17","REMO","HIRHAM5","WRF381P")
      ncol=3
    }else if(name_eff == "hm"){
      effs.name.FIG <- c("CTRIP","GRSD","ORCHIDEE","SMASH")
      ncol=4
    }
    
    
    out = output.map.effect[[name_eff]]
    out.reorder = transform(out,effs=factor(effs,levels=effs.name.FIG))
    
    lPlt[[name_eff]]=base_map(data=out.reorder,zoom=zoom)+
      geom_point(aes(x=x,y=y,fill=val),size=1.5,shape=21,stroke = NA)+
      geom_sf(data=river_L2,colour="gray80",linewidth=0.1,alpha=0.5)+
      geom_sf(data=fr_L2,fill=NA,linewidth=0.1,color="black")+
      guides(fill=guide_colorbar(barwidth = 25, barheight = 1.5,
                                 label.theme = element_text(size = 14, face = c("bold"),color=c("black")),
                                 title.theme=element_text(size = 14, face = "bold"),title.position = "top"))+
      binned_scale(aesthetics = "fill",name="[%]",palette = mypalette,limits=range(mybreaks),
                   breaks=mybreaks,oob=squish,labels=mylabels)+
      facet_wrap(~effs, ncol=ncol)+
      theme(panel.border = element_rect(colour = "black", fill=NA))+
      theme(strip.text = element_text(size = 14, face = "bold"),strip.background=element_blank())
    
    if(name_eff=="hm"){
      lPltHM[[indic]]=lPlt[[name_eff]]
    }
  }
  
  plt = lPlt$gcm / lPlt$rcm + plot_layout(heights = c(10, 15.5),guides = "collect") + 
    plot_annotation(tag_levels = 'A') & theme(legend.position='bottom',plot.tag = element_text(size = 40,face="bold"))
  
  ggsave(filename = paste0("../FIGURES/Fig9_effects_",indic,"_",horiz,".jpg"),
         plot=plt,device = "jpeg",units="cm",height=40,width=30,dpi=200)
}


########################################################
# PLOT 2: HM effects for QMNA, QA, QJXA
########################################################

## freq_col for scaling the color scale limits
lOut = list()
for(indic in c("QMNA","QA","QJXA")){
  
  # output from QUALYPSO
  vecFileQUALYPSO = paste0("../QUALYPSOOUT/Hydro/",indic,"/",
                           code_sel,".rds")
  
  # retrieve data
  lOut[[indic]] = get.output.map.effect(x=coordsL2$x,y=coordsL2$y,horiz=horiz,
                                            vecFileQUALYPSO=vecFileQUALYPSO)
}

# name effects
effs.name.FIG <- c("CTRIP","GRSD","ORCHIDEE","SMASH")

# extract HM effects only
lOutHM = list()
for(indic in c("QMNA","QA","QJXA")){
  lOutHM[[indic]] = lOut[[indic]]$hm
  lOutHM[[indic]]$indic = indic
}

out = rbind(lOutHM$QMNA,lOutHM$QA,lOutHM$QJXA)
out.reorder = transform(out,effs=factor(effs,levels=effs.name.FIG),
                        indic=factor(indic,levels=c("QMNA","QA","QJXA")))

plt=base_map(data=out.reorder,zoom=zoom)+
  geom_point(aes(x=x,y=y,fill=val),size=1.5,shape=21,stroke = NA)+
  geom_sf(data=river_L2,colour="gray80",linewidth=0.1,alpha=0.5)+
  geom_sf(data=fr_L2,fill=NA,linewidth=0.1,color="black")+
  guides(fill=guide_colorbar(barwidth = 25, barheight = 1.5,position = "bottom",
                             label.theme = element_text(size = 14, face = c("bold"),color=c("black")),
                             title.theme=element_text(size = 14, face = "bold"),title.position = "top"))+
  binned_scale(aesthetics = "fill",name="[%]",palette = mypalette,limits=range(mybreaks),
               breaks=mybreaks,oob=squish,labels=mylabels)+
  facet_grid(indic~effs,switch="y")+
  theme(panel.border = element_rect(colour = "black", fill=NA))+
  theme(strip.text = element_text(size = 20, face = "bold"),strip.background=element_blank())

ggsave(filename = paste0("../FIGURES/Fig9_effectsHM_",horiz,".jpg"),
       plot=plt,device = "jpeg",units="cm",height=25,width=30,dpi=200)