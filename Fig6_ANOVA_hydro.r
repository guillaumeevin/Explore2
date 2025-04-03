# Guillaume Evin
# 01/04/2025
# Figure 6: Hydro var. QMNA, QA and QJXA
# A. Uncertainty climate change responses + IV
# B. ANOVA of hydro variables

library("patchwork")
library("ggpubr")
library("sp")

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

# horizon
horiz = 2085

# France
myzoom = "FR"

## main list
plt = list()

## horiz a temporal or temp?rature horizon
for(indic in c("QA","QJXA","QMNA")){
  plt[[indic]] = list()
  
  # output from QUALYPSO
  vecFileQUALYPSO = paste0("../QUALYPSOOUT/Hydro/",indic,"/",
                           code_sel,".rds")
  
  
  ######## breaks and colors for UNC and IV
  mybreaks = c(0.05,0.1,0.2,0.3,0.4,0.5)
  mylabels = c("< 5","10","20","30","40","> 50")
  mypalette = binned_pal(scales::manual_pal(ipcc_yelblue_5))
  
  ######## map_one_var incert ########
  exut = get.output.map.one.var(dfCoords=coordsL2,horiz=horiz,vecFileQUALYPSO=vecFileQUALYPSO,
                                vartype="incert")
  
  plt[[indic]][["UNC"]] = base_map(data = exut, zoom = myzoom)+
    geom_point(aes(x=x,y=y,fill=val),size=2,shape=21,stroke = NA)+
    geom_sf(data=river_L2,colour="gray80",linewidth=0.1,alpha=0.5)+
    geom_sf(data=fr_L2,fill=NA,linewidth=0.1,color="black")+
    guides(fill=guide_colorbar(barwidth = 1.5, barheight = 15,
                               label.theme = element_text(size = 14, face = c("bold"),color=c("black")),
                               title.theme=element_text(size = 14, face = "bold"),title.position = "top"))+
    binned_scale(aesthetics = "fill",palette = mypalette, name = "[%]",
                 guide="coloursteps",limits=range(mybreaks),breaks=mybreaks,labels= mylabels)+
    theme(panel.border = element_rect(colour = "black", fill=NA))+
    theme(strip.background=element_blank(),legend.title = element_text(face = "bold",size = 20),
          legend.text = element_text(face = "bold",size = 20),plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
  
  ######## map_one_var varint ########
  exut = get.output.map.one.var(dfCoords=coordsL2,horiz=horiz,vecFileQUALYPSO=vecFileQUALYPSO,
                                vartype="varint")
  
  plt[[indic]][["IV"]] = base_map(data = exut, zoom = myzoom)+
    geom_point(aes(x=x,y=y,fill=val),size=2,shape=21,stroke = NA)+
    geom_sf(data=river_L2,colour="gray80",linewidth=0.1,alpha=0.5)+
    geom_sf(data=fr_L2,fill=NA,linewidth=0.1,color="black")+
    guides(fill=guide_colorbar(barwidth = 1.5, barheight = 15,
                               label.theme = element_text(size = 14, face = c("bold"),color=c("black")),
                               title.theme=element_text(size = 14, face = "bold"),title.position = "top"))+
    binned_scale(aesthetics = "fill",palette = mypalette, name = "[%]",
                 guide="coloursteps",limits=range(mybreaks),breaks=mybreaks,labels= mylabels)+
    theme(panel.border = element_rect(colour = "black", fill=NA))+
    theme(strip.background=element_blank(),legend.title = element_text(face = "bold",size = 20),
          legend.text = element_text(face = "bold",size = 20),plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
  
  ###### map_var_part #####
  
  # breaks, colors
  mybreaks = c(0,10,20,40,60,100)
  purple5 = c('#ffffd4','#fed98e','#fe9929','#d95f0e','#993404')
  mypalette = binned_pal(scales::manual_pal(purple5))
  
  # get data
  exut = get.output.map.var.part(dfCoords=coordsL2,horiz=horiz,vecFileQUALYPSO=vecFileQUALYPSO)
  
  # New facet label names
  source.order = c("rcp", "gcm","rcm","hm","rv")
  source.labs <- c("RCP","GCM","RCM","HM","RV")
  
  ptmp = base_map(data = exut,zoom= myzoom)+
    geom_point(aes(x=x,y=y,fill=val),size=2,shape=21,stroke = NA)+
    geom_sf(data=river_L2,colour="gray80",linewidth=0.1,alpha=0.5)+
    geom_sf(data=fr_L2,fill=NA,linewidth=0.1,color="black")+
    guides(fill=guide_colorbar(barwidth = 1.5, barheight = 15,
                               label.theme = element_text(size = 14, face = c("bold"),color=c("black")),
                               title.theme=element_text(size = 14, face = "bold"),title.position = "top"))+
    binned_scale(aesthetics = "fill",name="[%]",palette = mypalette,
                 guide="coloursteps",limits=c(0,100),breaks=mybreaks,show.limits = T,oob=squish)+
    facet_wrap(. ~ factor(source, levels=source.order,labels = source.labs), ncol=1, strip.position="left")+
    theme(panel.border = element_rect(colour = "black", fill=NA)) +
    theme(strip.background=element_blank(),legend.title = element_text(face = "bold",size = 20),
          legend.text = element_text(face = "bold",size = 20),plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
  if(indic=="QMNA"){
    plt[[indic]][["ANOVA"]] = ptmp + theme(strip.text = element_text(size = 20, face = "bold"),
                                           strip.background=element_blank())
  }else{
    plt[[indic]][["ANOVA"]] = ptmp +  theme(strip.background = element_blank(),
                                            strip.text = element_blank(),
                                            strip.text.x = element_blank())
  }
}  

# PLOT 1: uncertainty climate change reponses and internal variability

# tas : UNC, IV
p1 = plt$QMNA$UNC + ggtitle("QMNA") + scale_y_continuous(name ="UNC") +
  theme(plot.title = element_text(hjust = 0.5,size = 20),
        axis.title.y=element_text(size=20, face = "bold"))
p2 = plt$QA$UNC + ggtitle("QA") +
  theme(plot.title = element_text(hjust = 0.5,size = 20))
p3 = plt$QJXA$UNC + ggtitle("QJXA") +
  theme(plot.title = element_text(hjust = 0.5,size = 20))

p4 = plt$QMNA$IV + scale_y_continuous(name ="IV") +
  theme(axis.title.y=element_text(size=20, face = "bold"))
p5 = plt$QA$IV
p6 = plt$QJXA$IV

pltIVUNC = (p1 + p2 + p3) / (p4 + p5 + p6) + plot_layout(guides = "collect")# +
#  plot_annotation(title = 'A', theme = theme(plot.title = element_text(size = 40, face = "bold")))
ggsave(filename = paste0("../FIGURES/Fig6_ANOVA_hydro_",horiz,"_A.jpg"),
       plot=pltIVUNC,device = "jpeg",units="cm",height=20,width=30,dpi=200)

# PLOT 2: ANOVA

# add title columns and remove row labels for the three plots on the right
p1 = plt$QMNA$ANOVA + ggtitle("QMNA") +
  theme(plot.title = element_text(hjust = 0.5))
p2 = plt$QA$ANOVA + ggtitle("QA") +
  theme(plot.title = element_text(hjust = 0.5))
p3 = plt$QJXA$ANOVA + ggtitle("QJXA") +
  theme(plot.title = element_text(hjust = 0.5))

# merge them
pltANOVA = ggarrange(p1,p2,p3,common.legend = TRUE, legend="right",ncol = 3,
                     widths = c(1.12,1,1))# +
#  plot_annotation(title = 'B', theme = theme(plot.title = element_text(size = 40, face = "bold")))

ggsave(filename = paste0("../FIGURES/Fig6_ANOVA_hydro_",horiz,"_B.jpg"),
       plot=pltANOVA,device = "jpeg",units="cm",height=40,width=28,dpi=200)