# Guillaume Evin
# Explore2
# Figure ANOVA ET0
# A. Uncertainty climate change responses + IV
# B. ANOVA of meteo variables

library("patchwork")
library("ggpubr")

source("./lib_functions.r")

##############
# Resources #
##############
path_sig = "../SIG/"
path_river=paste0(path_sig,"processed/CoursEau_idx1_wgs84.shp")
path_fr=paste0(path_sig,"raw/IGN/contours_FR/gadm36_FRA_0.shp")
background_for_maps(path_river,path_fr)

# horizon
horiz = 2085

# coordinates France/SAFRAN grid: list of matrices
lCoords = readRDS(file = "./coordMeteo.rds")
isPixel = lCoords$mask
indexPixel = which(isPixel==1,arr.ind = T)
nPixels = sum(isPixel)
dfCoords = data.frame(x=as.vector(lCoords$x_l2[as.logical(isPixel)]),
                      y=as.vector(lCoords$y_l2[as.logical(isPixel)]))

# domain
zoom = "FR"

## main list
plt = list()

for(indic in c("evspsblpot_JJA","evspsblpot_DJF")){
  plt[[indic]] = list()
  
  # output from QUALYPSO
  vecFileQUALYPSO = paste0("../QUALYPSOOUT/Meteo/",indic,"/",
                           indexPixel[,1],"_",indexPixel[,2],".rds")
  
  ##########################
  # Colors total uncertainty and internal var.
  mybreaks = c(0.05,0.1,0.2,0.3,0.4,0.5)
  mylabels = c("< 5","10","20","30","40","> 50")
  mypalette = binned_pal(scales::manual_pal(ipcc_yelblue_5))
  unitSB = "[%]"
  
  
  ######## incertitude r?ponses climatiques ########
  out = get.output.map.one.var(dfCoords=dfCoords,horiz=horiz,
                               vecFileQUALYPSO=vecFileQUALYPSO,
                               vartype="incert")
  
  plt[[indic]][["UNC"]] = base_map(data = out,zoom=zoom)+
    geom_tile(aes(x=x,y=y,fill=val))+
    geom_sf(data=river_L2,colour="gray80",linewidth=0.1,alpha=0.5)+
    geom_sf(data=fr_L2,fill=NA,linewidth=0.1,color="black")+
    guides(fill=guide_colorbar(barwidth = 25, barheight = 1.5,title.position = "top"))+
    binned_scale(aesthetics = "fill",name=unitSB,palette=mypalette,guide="coloursteps",
                 limits=range(mybreaks),breaks=mybreaks,labels= mylabels)+#that way because stepsn deforms colors
    theme(legend.direction = "horizontal", legend.position = "bottom", legend.box = "horizontal") +
    labs(title = "") +
    theme(panel.border = element_rect(colour = "black", fill=NA),
          plot.title = element_text(size = 40, face = "bold"),strip.background=element_blank(),
          legend.title=element_text(face = "bold",size=30), legend.text=element_text(face = "bold",size=30))
  
  ######## variabilit? interne ########
  out = get.output.map.one.var(dfCoords=dfCoords,horiz=horiz,
                               vecFileQUALYPSO=vecFileQUALYPSO,
                               vartype="varint")
  
  plt[[indic]][["IV"]] = base_map(data = out,zoom=zoom)+
    geom_tile(aes(x=x,y=y,fill=val))+
    geom_sf(data=river_L2,colour="gray80",linewidth=0.1,alpha=0.5)+
    geom_sf(data=fr_L2,fill=NA,linewidth=0.1,color="black")+
    guides(fill=guide_colorbar(barwidth = 25, barheight = 1.5,title.position = "top"))+
    binned_scale(aesthetics = "fill",name=unitSB,palette=mypalette,guide="coloursteps",
                 limits=range(mybreaks),breaks=mybreaks,labels= mylabels)+#that way because stepsn deforms colors
    theme(legend.direction = "horizontal", legend.position = "bottom", legend.box = "horizontal") +
    labs(title = "") +
    theme(panel.border = element_rect(colour = "black", fill=NA),
          plot.title = element_text(size = 40, face = "bold"),strip.background=element_blank(),
          legend.title=element_text(face = "bold",size=30), legend.text=element_text(face = "bold",size=30))
  
  ###### map_var_part #####
  mybreaks = c(0,10,20,40,60,100)
  
  purple5 = c('#ffffd4','#fed98e','#fe9929','#d95f0e','#993404')
  
  mypalette = binned_pal(scales::manual_pal(purple5))
  
  out = get.output.map.var.part(dfCoords=dfCoords,horiz=horiz,vecFileQUALYPSO=vecFileQUALYPSO)
  
  # New facet label names
  source.order = c("rcp", "gcm","rcm","rv")
  source.labs <- c("RCP","GCM","RCM","RV")
  
  ptmp = base_map(data = out,zoom=zoom)+
    geom_tile(aes(x=x,y=y,fill=val))+
    geom_sf(data=river_L2,colour="gray80",linewidth=0.1,alpha=0.5)+
    geom_sf(data=fr_L2,fill=NA,linewidth=0.1,color="black")+
    guides(fill=guide_colorbar(barwidth = 25, barheight = 1.5,title.position = "top"))+
    binned_scale(aesthetics = "fill",name="[%]",palette = mypalette,
                 guide="coloursteps",limits=c(0,100),breaks=mybreaks,show.limits = T,oob=squish)+
    facet_wrap(. ~ factor(source, levels=source.order,labels = source.labs), ncol=1, strip.position="left")+
    theme(panel.border = element_rect(colour = "black", fill=NA),
          plot.title = element_text(size = 40, face = "bold"),
          legend.title=element_text(face = "bold",size=30), legend.text=element_text(face = "bold",size=30))
  
  if(indic=="evspsblpot_JJA"){
    plt[[indic]][["ANOVA"]] = ptmp + theme(strip.text = element_text(size = 40, face = "bold"),
                                           strip.background=element_blank())
  }else{
    plt[[indic]][["ANOVA"]] = ptmp +  theme(strip.background = element_blank(),
                                            strip.text = element_blank(),
                                            strip.text.x = element_blank())
  }
}

# PLOT 1: uncertainty climate change reponses and internal variability

# ET0 : UNC, IV
pET0JJAUnc = plt$evspsblpot_JJA$UNC + ggtitle("JJA") + scale_y_continuous(name ="CCRU") +
  theme(plot.title = element_text(hjust = 0.5,size = 40),
        axis.title.y=element_text(size=40, face = "bold"))
pET0DJFUnc = plt$evspsblpot_DJF$UNC + ggtitle("DJF") +
  theme(plot.title = element_text(hjust = 0.5,size = 40))
pET0JJAIV = plt$evspsblpot_JJA$IV  + scale_y_continuous(name ="IV") +
  theme(axis.title.y=element_text(size=40, face = "bold"))
pET0DJFIV = plt$evspsblpot_DJF$IV


# PLOT 2: ANOVA

# add title columns and remove row labels for the three plots on the right
pANOVAET0JJA = plt$evspsblpot_JJA$ANOVA + ggtitle("JJA") +
  theme(plot.title = element_text(size = 40))
pANOVAET0DJF = plt$evspsblpot_DJF$ANOVA + ggtitle("DJF") +
  theme(plot.title = element_text(size = 40))

# merge them
pltIVUNC = ggarrange(pET0JJAUnc,pET0DJFUnc,pET0JJAIV,pET0DJFIV,
                 common.legend = TRUE, legend="bottom",ncol = 2, nrow = 2,
                 widths = c(1.15,1.07))+
  theme(plot.margin = margin(0,0,0,0.8, "cm")) 

pltANOVA = ggarrange(pANOVAET0JJA,pANOVAET0DJF,
                     common.legend = TRUE, legend="bottom",ncol = 2,
                     widths = c(1.15,1))

lplt = ggarrange(pltIVUNC,pltANOVA,nrow=2,heights = c(28, 45),
                 labels = c("A","B"),font.label = list(size = 40, color = "black", face = "bold", family = NULL))

ggsave(filename = paste0("../FIGURES/Fig_ANOVA_ET0_",horiz,".jpg"),
       plot=lplt,device = "jpeg",units="cm",height=73,width=25,dpi=200)