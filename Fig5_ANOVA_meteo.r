# Guillaume Evin
# Explore2
# 01/04/2025
# Figure 5: Meteo var. tas and pr / JJA and DJF
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

for(indic in c("tas_JJA","tas_DJF","pr_JJA","pr_DJF")){
  plt[[indic]] = list()
  
  # output from QUALYPSO
  vecFileQUALYPSO = paste0("../QUALYPSOOUT/Meteo/",indic,"/",
                           indexPixel[,1],"_",indexPixel[,2],".rds")
  
  ##########################
  # Colors total uncertainty and internal var.
  
  # choose labels and limits
  if(!indic%in%c("tas_JJA","tas_DJF")){
    mybreaks = c(0.05,0.1,0.2,0.3,0.4,0.5)
    mylabels = c("< 5","10","20","30","40","> 50")
    mypalette = binned_pal(scales::manual_pal(ipcc_yelblue_5))
    unitSB = "[%]"
  }else{
    mybreaks = c(0.5,1,1.5,2,2.5,3)
    mylabels = c("< 0.5","1","1.5","2","2.5","> 3")
    mypalette = binned_pal(scales::manual_pal(ipcc_yelred_5))
    unitSB = "[°C]"
  }
  
  
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
  
  if(indic=="tas_JJA"){
    plt[[indic]][["ANOVA"]] = ptmp + theme(strip.text = element_text(size = 40, face = "bold"),
                                           strip.background=element_blank())
  }else{
    plt[[indic]][["ANOVA"]] = ptmp +  theme(strip.background = element_blank(),
                                            strip.text = element_blank(),
                                            strip.text.x = element_blank())
  }
}

# PLOT 1: uncertainty climate change reponses and internal variability

# tas : UNC, IV
pTasJJAUnc = plt$tas_JJA$UNC + ggtitle("tas / JJA") + scale_y_continuous(name ="UNC") +
  theme(plot.title = element_text(hjust = 0.5,size = 40),
        axis.title.y=element_text(size=40, face = "bold"))
pTasDJFUnc = plt$tas_DJF$UNC + ggtitle("tas / DJF") +
  theme(plot.title = element_text(hjust = 0.5,size = 40))
pTasJJAIV = plt$tas_JJA$IV  + scale_y_continuous(name ="IV") +
  theme(axis.title.y=element_text(size=40, face = "bold"))
pTasDJFIV = plt$tas_DJF$IV

# pr : UNC, IV
pPrJJAUnc = plt$pr_JJA$UNC + ggtitle("pr / JJA") +
  theme(plot.title = element_text(hjust = 0.5,size = 40))
pPrDJFUnc = plt$pr_DJF$UNC + ggtitle("pr / DJF") +
  theme(plot.title = element_text(hjust = 0.5,size = 40))
pPrJJAIV = plt$pr_JJA$IV
pPrDJFIV = plt$pr_DJF$IV

# PLOT 2: ANOVA

# add title columns and remove row labels for the three plots on the right
pANOVAtasJJA = plt$tas_JJA$ANOVA + ggtitle("tas / JJA") +
  theme(plot.title = element_text(size = 40))
pANOVAtasDJF = plt$tas_DJF$ANOVA + ggtitle("tas / DJF") +
  theme(plot.title = element_text(size = 40))
pANOVAprJJA = plt$pr_JJA$ANOVA + ggtitle("pr / JJA") +
  theme(plot.title = element_text(size = 40))
pANOVAprDJF = plt$pr_DJF$ANOVA + ggtitle("pr / DJF") +
  theme(plot.title = element_text(size = 40))

# merge them

pTas = ggarrange(pTasJJAUnc,pTasDJFUnc,pTasJJAIV,pTasDJFIV,
                 common.legend = TRUE, legend="bottom",ncol = 2, nrow = 2,
                 widths = c(1.15,1.07))

pPr = ggarrange(pPrJJAUnc,pPrDJFUnc,pPrJJAIV,pPrDJFIV, 
                common.legend = TRUE, legend="bottom",ncol = 2, nrow = 2,
                widths = c(1,1))
pltIVUNC=ggarrange(pTas,pPr)+
  theme(plot.margin = margin(0,0,0,0.8, "cm")) 

pltANOVA = ggarrange(pANOVAtasJJA,pANOVAtasDJF,pANOVAprJJA,pANOVAprDJF,
                     common.legend = TRUE, legend="bottom",ncol = 4,
                     widths = c(1.15,1,1,1))

lplt = ggarrange(pltIVUNC,pltANOVA,nrow=2,heights = c(28, 45),
                 labels = c("A","B"),font.label = list(size = 40, color = "black", face = "bold", family = NULL))

ggsave(filename = paste0("../FIGURES/Fig5_ANOVA_meteo_",horiz,".jpg"),
       plot=lplt,device = "jpeg",units="cm",height=73,width=45,dpi=200)