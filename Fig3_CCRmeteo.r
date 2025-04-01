# Guillaume Evin
# Explore2
# Make figures from Qualypso runs for report

library(patchwork)

source("./lib_functions.r")

##############
# Resources #
##############

path_sig = "../SIG/"
path_river=paste0(path_sig,"processed/CoursEau_idx1_wgs84.shp")
path_fr=paste0(path_sig,"raw/IGN/contours_FR/gadm36_FRA_0.shp")
background_for_maps(path_river,path_fr)



########################################################
## map_3quant_3rcp_1horiz
# index RCP
ieff_rcp=1
rcp_names=c("rcp26","rcp45","rcp85")
horiz = 2085

# New facet label names
quant.labs <- c("5%","50%","95%")
quant = c("5%","50%","95%")
names(quant.labs) <- quant

# coordinates France/SAFRAN grid: list of matrices
lCoords = readRDS(file = "./coordMeteo.rds")
isPixel = lCoords$mask
indexPixel = which(isPixel==1,arr.ind = T)
nPixels = sum(isPixel)
dfCoords = data.frame(x=as.vector(lCoords$x_l2[as.logical(isPixel)]),
                      y=as.vector(lCoords$y_l2[as.logical(isPixel)]))

zoom = "FR"


for(rcp in rcp_names){
  ## freq_col for scaling the color scale limits
  lplt = list()
  for(v in c("tas","pr","evspsblpot","rx1day")){
    
    ################################################################################
    # choose labels and limits
    if(!v=="tas"){
      if(v%in%c("evspsblpot")){
        breaks=c(-0.5,-0.4,-0.3,-0.2,-0.1,-0.05,0,0.05,0.1,0.2,0.3,0.4,0.5)
        mypalette = binned_pal(scales::manual_pal(rev(prec_meanchange)))
      }else{
        breaks=c(-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6)
        mypalette = binned_pal(scales::manual_pal(prec_meanchange))
      }
      nbrk = length(breaks)
      labels = breaks*100
      labels[1] = paste0("< ",labels[1])
      labels[nbrk] = paste0("> ",labels[nbrk])
      unitSB = "[%]"
    }else{
      breaks=c(-1,-0.5,0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5)
      nbrk = length(breaks)
      labels = breaks
      labels[1] = paste0("< ",labels[1])
      labels[nbrk] = paste0("> ",labels[nbrk])  
      mypalette = binned_pal(scales::manual_pal(temp_meanchange))
      unitSB = "[°C]"
    }
    
    ################################################################################
    # process coordinates
    
    if(v!="rx1day"){
      lout = list()
      for(seas in c("JJA","DJF")){
        indic = paste0(v,"_",seas)
        
        # output from QUALYPSO
        vecFileQUALYPSO = paste0("../QUALYPSOOUT/Meteo/",indic,"/",
                                 indexPixel[,1],"_",indexPixel[,2],".rds")
        
        ###### map.3quant.3rcp.1horiz #####
        lout[[seas]] = get.output.map.3quant.1rcp.1horiz(quant=quant,dfCoords=dfCoords,
                                                         horiz=horiz,
                                                         vecFileQUALYPSO=vecFileQUALYPSO,
                                                         ieff_rcp=ieff_rcp,rcp=rcp)
        lout[[seas]]$season = seas
      }
      
      
      outJJADJFrcp = rbind(lout$JJA,lout$DJF)
      outJJADJFrcp$season = factor(outJJADJFrcp$season,levels = c("JJA","DJF"))
      
      # call plot
      lplt[[v]]=base_map(data = outJJADJFrcp,zoom=zoom)+
        geom_tile(aes(x=x,y=y,fill=val))+
        geom_sf(data=river_L2,colour="gray80",linewidth=0.1,alpha=0.5)+
        geom_sf(data=fr_L2,fill=NA,linewidth=0.1,color="black")+
        facet_grid(season ~ quant,labeller = labeller(quant = quant.labs),switch = "y")+
        guides(fill=guide_colorbar(barwidth = 1.5, barheight = 20,
                                   frame.colour = "black", ticks.colour = "black",
                                   label.theme = element_text(size = 14, face = c("bold"),color=c("black")),
                                   title.theme=element_text(size = 14, face = "bold")))+
        binned_scale(aesthetics = "fill",name=unitSB,
                     palette=mypalette,guide="coloursteps",limits=range(breaks),
                     breaks=breaks,oob=squish,labels=labels)+
        theme(panel.border = element_rect(colour = "black", fill=NA))+
        theme(strip.background=element_blank(),legend.title = element_text(face = "bold",size = 20),
              strip.text = element_text(size = 20), legend.text = element_text(face = "bold",size = 14),
              plot.margin=grid::unit(c(0,0,0,0), "mm"))
    }else{
      indic = paste0(v,"_year")
      
      # output from QUALYPSO
      vecFileQUALYPSO = paste0("../QUALYPSOOUT/Meteo/",indic,"/",
                               indexPixel[,1],"_",indexPixel[,2],".rds")
      
      ###### map.3quant.3rcp.1horiz #####
      out = get.output.map.3quant.1rcp.1horiz(quant=quant,dfCoords=dfCoords,
                                              horiz=horiz,
                                              vecFileQUALYPSO=vecFileQUALYPSO,
                                              ieff_rcp=ieff_rcp,rcp=rcp)
      fakeLabelSeason = "   "
      out$season = fakeLabelSeason
      out$season = factor(out$season,levels = fakeLabelSeason)
      
      # call plot
      lplt[[v]]=base_map(data = out,zoom=zoom)+
        geom_tile(aes(x=x,y=y,fill=val))+
        geom_sf(data=river_L2,colour="gray80",linewidth=0.1,alpha=0.5)+
        geom_sf(data=fr_L2,fill=NA,linewidth=0.1,color="black")+
        facet_grid(season ~ quant,labeller = labeller(quant = quant.labs),switch = "y")+
        guides(fill=guide_colorbar(barwidth = 1.5, barheight = 10,
                                   frame.colour = "black", ticks.colour = "black",
                                   label.theme = element_text(size = 14, face = c("bold"),color=c("black")),
                                   title.theme=element_text(size = 14, face = "bold")))+
        binned_scale(aesthetics = "fill",name=unitSB,
                     palette=mypalette,guide="coloursteps",limits=range(breaks),
                     breaks=breaks,oob=squish,labels=labels)+
        theme(panel.border = element_rect(colour = "black", fill=NA))+
        theme(strip.background=element_blank(),legend.title = element_text(face = "bold",size = 20),
              strip.text = element_text(size = 20), legend.text = element_text(face = "bold",size = 14),
              plot.margin=grid::unit(c(0,0,0,0), "mm"))
    }
  }
  
  # Figure 3a: TAS / PR
  plt = ggarrange(lplt$tas,lplt$pr,nrow=2,ncol=1,align="h",hjust=0,labels = c("TAS","PR"),
                  font.label = list(size = 20, color = "black", face = "bold", family = NULL))
  ggsave(filename = paste0("../FIGURES/Fig3a_",rcp,"_",horiz,".jpg"),
         plot=plt,device = "jpeg",units="cm",height=27,width=25,dpi=200)
  
  # Figure 3b: ET0 / RX1D
  plt = ggarrange(lplt$evspsblpot,lplt$rx1day,nrow=2,ncol=1,labels = c("ET0","RX1D"),
                  heights = c(2,1),widths = 1, align ="h",
                  font.label = list(size = 20, color = "black", face = "bold", family = NULL))
  ggsave(filename = paste0("../FIGURES/Fig3b_",rcp,"_",horiz,".jpg"),
         plot=plt,device = "jpeg",units="cm",height=22,width=25,dpi=200)
  
}