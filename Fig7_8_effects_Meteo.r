# Guillaume Evin
# Explore2
# Effects meteo indicators

library(patchwork)

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


for(indic in c("tas_JJA","tas_DJF","pr_JJA","pr_DJF",
               "evspsblpot_JJA","evspsblpot_DJF","rx1day_year")){
  
  # traitement different des temperatures
  istas = indic%in%c("tas_JJA","tas_DJF")
  
  ################################################################################
  
  # color
  if(!istas){
    unitSB = "[%]"
    if(indic%in%c("evspsblpot_JJA","evspsblpot_DJF")){
      colorscale = binned_pal(scales::manual_pal(rev(precip_10)))
    }else{
      colorscale = binned_pal(scales::manual_pal(precip_10))
    }
  }else{
    unitSB = "[°C]"
    colorscale = binned_pal(scales::manual_pal(rev(temp_10)))
  }
  
  # output from QUALYPSO
  vecFileQUALYPSO = paste0("../QUALYPSOOUT/Meteo/",indic,"/",
                           indexPixel[,1],"_",indexPixel[,2],".rds")
  
  # retrieve data
  output.map.effect = get.output.map.effect(x=dfCoords$x,y=dfCoords$y,horiz=horiz,vecFileQUALYPSO=vecFileQUALYPSO)
  
  # range by effect
  ylimeff = list()
  for(eff in names(output.map.effect)){
    ylimeff[[eff]] = quantile(output.map.effect[[eff]]$val,probs=c(0.01,0.99))
  }
  
  # maximum bound for the color bar
  if(istas){
    ylim=round(max(abs(unlist(ylimeff)))/0.5)*0.5 #arrondi au 0.5 le plus proche
  }else{
    ylim=round(max(abs(unlist(ylimeff)))/0.1)*0.1 #arrondi au 0.1 le plus proche
  }
  
  # breaks
  mybreaks = seq(from=-ylim,to=ylim,length.out=11)
  if(istas){
    mylabels = as.character(mybreaks)
  }else{
    mylabels = as.character(round(mybreaks*100))
  }
  mylabels[1] = paste0("< ",mylabels[1])
  mylabels[length(mylabels)] = paste0("> ",mylabels[length(mylabels)])
  
  lPlt = list()
  for(name_eff in c("gcm","rcm")){
    
    # reorder names
    if(name_eff == "gcm"){
      effs.name.FIG <- c("CNRM-CM5-LR","EC-EARTH","HadGEM2-ES","MPI-ESM-LR",
                         "NorESM1-M","IPSL-CM5A-MR")
    }else if(name_eff == "rcm"){
      effs.name.FIG <- c("ALADIN63","HadREM3-GA7-05","RACMO22E","RCA4","RegCM4-6",
                         "CCLM4-8-17","REMO","HIRHAM5","WRF381P")
    }
    
    out = output.map.effect[[name_eff]]
    out.reorder = transform(out,effs=factor(effs,levels=effs.name.FIG))
    
    lPlt[[name_eff]]=base_map(data=out.reorder,zoom=zoom)+
      geom_tile(aes(x=x,y=y,fill=val))+
      geom_sf(data=river_L2,colour="gray80",linewidth=0.1,alpha=0.5)+
      geom_sf(data=fr_L2,fill=NA,linewidth=0.1,color="black")+
      guides(fill=guide_colorbar(barwidth = 25, barheight = 1.5,
                                 label.theme = element_text(size = 14, face = c("bold"),color=c("black")),
                                 title.theme=element_text(size = 14, face = "bold"),title.position = "top"))+
      binned_scale(aesthetics = "fill",name=unitSB,palette = colorscale,
                   guide="coloursteps",limits=range(mybreaks),breaks=mybreaks,
                   show.limits = T,oob=squish,labels= mylabels)+
      facet_wrap(~effs, ncol=3)+
      theme(panel.border = element_rect(colour = "black", fill=NA))+
      theme(strip.text = element_text(size = 14, face = "bold"),strip.background=element_blank())
    
    if(name_eff!="gcm"){
      lPlt[[name_eff]]=lPlt[[name_eff]]+
        theme(legend.position="none")
    }
  }
  
  plt = lPlt$gcm / lPlt$rcm + plot_layout(heights = c(10, 15.5),guides = "collect") + 
    plot_annotation(tag_levels = 'A') & theme(legend.position='bottom',plot.tag = element_text(size = 40, face = "bold"))
  ggsave(filename = paste0("../FIGURES/Fig_effects_",indic,"_",horiz,".jpg"),
         plot=plt,device = "jpeg",units="cm",height=45,width=25,dpi=200)
}
