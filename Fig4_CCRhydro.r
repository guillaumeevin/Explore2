# Guillaume Evin
# Explore2
# Figure 4: climate change responses for hydro indicators

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

########################################################
## map_3quant_3rcp_1horiz
# index RCP
ieff_rcp=3
rcp_names=c("rcp26","rcp45","rcp85")
horiz = 2085

# rcp labels
rcp.labs <- c("RCP2.6", "RCP4.5", "RCP8.5")
names(rcp.labs) <- rcp_names

# quantile labels
quant.labs <- c("5%","50%","95%")
quant = c("5%","50%","95%")
names(quant.labs) <- quant

# limits / breaks
limIndic = 0.75
breaks = seq(-limIndic,limIndic,length.out=11)
nbrk = length(breaks)
labels = round(breaks*100)
labels[1] = paste0("< ",labels[1])
labels[nbrk] = paste0("> ",labels[nbrk])

# colors
mypalette = binned_pal(scales::manual_pal(precip_10))

# France
myzoom = "FR"

for(rcp in rcp_names){
  lExut = list()
  for(indic in c("QA","QJXA","QMNA")){
    
    # output from QUALYPSO
    vecFileQUALYPSO = paste0("../QUALYPSOOUT/Hydro/",indic,"/",
                             code_sel,".rds")
    
    # map.3quant.1rcp.1horiz
    exutRCP= get.output.map.3quant.1rcp.1horiz(quant=quant,dfCoords=coordsL2,
                                      horiz=horiz,
                                      vecFileQUALYPSO=vecFileQUALYPSO,
                                      ieff_rcp=ieff_rcp,rcp=rcp)
    
    exutRCP$indic = indic
    lExut[[indic]] = exutRCP
  }
  
  dataPlot = rbind(lExut$QMNA,lExut$QA,lExut$QJXA)
  dataPlot$indic = factor(dataPlot$indic, levels=c("QMNA","QA","QJXA"))
  
  # call plot
  plt=base_map(data = dataPlot,zoom= myzoom)+
    geom_point(aes(x=x,y=y,fill=val),size=1.5,shape=21,stroke = NA,show.legend = TRUE)+
    geom_sf(data=river_L2,colour="gray80",linewidth=0.1,alpha=0.5)+
    geom_sf(data=fr_L2,fill=NA,linewidth=0.1,color="black")+
    facet_grid(indic ~ quant,labeller = labeller(quant = quant.labs),switch = "y")+
    guides(fill=guide_colorbar(barwidth = 1.5, barheight = 20,
                               label.theme = element_text(size = 14, face = c("bold"),color=c("black")),
                               title.theme=element_text(size = 14, face = "bold"),title.position = "top"))+
    binned_scale(aesthetics = "fill",name="[%]",palette = mypalette,limits=range(breaks),
                 breaks=breaks,oob=squish,labels=labels)+
    theme(panel.border = element_rect(colour = "black", fill=NA))+
    theme(strip.background=element_blank(),legend.title = element_text(face = "bold",size = 20),
          strip.text = element_text(size = 20), legend.text = element_text(face = "bold",size = 14),
          plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
  # save
  ggsave(filename = paste0("../FIGURES/Fig4_",rcp,"_",horiz,".jpg"),
         plot=plt,device = "jpeg",units="cm",height=20,width=25,dpi=200)
}