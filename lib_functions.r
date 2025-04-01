library(ggplot2)
library(dplyr) #left_join
library(raster)# CRS management
library(plotwidgets)#hsl colors (would be better to use hsluv model but not available on my R version)
library(viridis)
library(scales)#squish
library(tidyr)#pivot_longer
library(ggpubr) #ggarrange
library(ggnewscale) #new_scale
library(pals)
library(sf)

postprocessPlot <- function(myPlot, pointSize = 0.5, textSize = 10, spaceLegend = 0.1) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = textSize),
          legend.title = element_text(size = textSize), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"),
          legend.position="none")
}

element_textbox <- function(...) {
  el <- element_text(...)
  class(el) <- c("element_textbox", class(el))
  el
}

element_grob.element_textbox <- function(element, ...) {
  text_grob <- NextMethod()
  rect_grob <- element_grob(calc_element("strip.background", theme_bw()))
  
  ggplot2:::absoluteGrob(
    grid::gList(
      element_grob(calc_element("strip.background", theme_bw())),
      text_grob
    ),
    height = grid::grobHeight(text_grob), 
    width = grid::unit(1, "npc")
  )
}

#########################################
# CMYK to RGB colors
# assumes integer input for CMYK
cmyk <- function(C,M,Y,K) {
  C <- C / 100.0
  M <- M / 100.0
  Y <- Y / 100.0
  K <- K / 100.0
  n.c <- (C * (1-K) + K)
  n.m <- (M * (1-K) + K)  
  n.y <- (Y * (1-K) + K)
  r.col <- ceiling(255 * (1-n.c))
  g.col <- ceiling(255 * (1-n.m))
  b.col <- ceiling(255 * (1-n.y))
  tmp=col2rgb(sprintf("#%02s%02s%02s",
                      as.hexmode(r.col), 
                      as.hexmode(g.col), 
                      as.hexmode(b.col)))
  tmp=as.vector(tmp)
  return(rgb(tmp[1],tmp[2],tmp[3],maxColorValue=255))
  
}

##########################################
##IPCC colors and others

#For continuous variable requiring good distinction
ipcc_yelblue_5=c(rgb(255,255,204,maxColorValue=255),rgb(161,218,180,maxColorValue=255),
                 rgb(65,182,196,maxColorValue=255),rgb(44,127,184,maxColorValue=255),rgb(37,52,148,maxColorValue=255))
ipcc_yelred_5=c(rgb(255,255,178,maxColorValue=255),rgb(254,204,92,maxColorValue=255),
                rgb(253,141,60,maxColorValue=255),rgb(240,59,32,maxColorValue=255),rgb(189,0,38,maxColorValue=255))

#For Precipitation
precip_11=c(rgb(84,48,5,maxColorValue=255),rgb(140,81,10,maxColorValue=255),
            rgb(191,129,45,maxColorValue=255),rgb(223,194,125,maxColorValue=255),
            rgb(246,232,195,maxColorValue=255),rgb(245,245,245,maxColorValue=255),
            rgb(199,234,229,maxColorValue=255),rgb(128,205,193,maxColorValue=255),
            rgb(53,151,143,maxColorValue=255),rgb(1,102,94,maxColorValue=255),rgb(0,60,48,maxColorValue=255))
precip_10=c(rgb(84,48,5,maxColorValue=255),rgb(140,81,10,maxColorValue=255),
            rgb(191,129,45,maxColorValue=255),rgb(223,194,125,maxColorValue=255),
            rgb(246,232,195,maxColorValue=255),rgb(199,234,229,maxColorValue=255),
            rgb(128,205,193,maxColorValue=255),rgb(53,151,143,maxColorValue=255),
            rgb(1,102,94,maxColorValue=255),rgb(0,60,48,maxColorValue=255))
precip_9=c(rgb(140,81,10,maxColorValue=255),rgb(191,129,45,maxColorValue=255),
           rgb(223,194,125,maxColorValue=255),rgb(246,232,195,maxColorValue=255),
           rgb(245,245,245,maxColorValue=255),rgb(199,234,229,maxColorValue=255),
           rgb(128,205,193,maxColorValue=255),rgb(53,151,143,maxColorValue=255),rgb(1,102,94,maxColorValue=255))
precip_8=c(rgb(140,81,10,maxColorValue=255),rgb(191,129,45,maxColorValue=255),
           rgb(223,194,125,maxColorValue=255),rgb(246,232,195,maxColorValue=255),
           rgb(199,234,229,maxColorValue=255),rgb(128,205,193,maxColorValue=255),
           rgb(53,151,143,maxColorValue=255),rgb(1,102,94,maxColorValue=255))
precip_7=c(rgb(140,81,10,maxColorValue=255),rgb(216,179,101,maxColorValue=255),
           rgb(246,232,195,maxColorValue=255),rgb(245,245,245,maxColorValue=255),
           rgb(199,234,229,maxColorValue=255),rgb(90,180,172,maxColorValue=255),rgb(1,102,94,maxColorValue=255))
precip_6=c(rgb(140,81,10,maxColorValue=255),rgb(216,179,101,maxColorValue=255),
           rgb(246,232,195,maxColorValue=255),rgb(199,234,229,maxColorValue=255),
           rgb(90,180,172,maxColorValue=255),rgb(1,102,94,maxColorValue=255))
precip_5=c(rgb(166,97,26,maxColorValue=255),rgb(223,194,125,maxColorValue=255),
           rgb(245,245,245,maxColorValue=255),rgb(128,205,193,maxColorValue=255),rgb(1,133,113,maxColorValue=255))

prec_meanchange = c(precip_10[1:5],"#ffffff","#ffffff",precip_10[6:10])

#For temperature
temp_14=c(rgb(254,254,203,maxColorValue=255),
          rgb(252,242,168,maxColorValue=255),
          rgb(249,225,132,maxColorValue=255),
          rgb(243,201,101,maxColorValue=255),
          rgb(237,175,86,maxColorValue=255),
          rgb(232,152,82,maxColorValue=255),
          rgb(226,129,80,maxColorValue=255),
          rgb(214,104,77,maxColorValue=255),
          rgb(187,80,72,maxColorValue=255),
          rgb(152,67,62,maxColorValue=255),
          rgb(117,56,48,maxColorValue=255),
          rgb(84,46,32,maxColorValue=255),
          rgb(53,35,18,maxColorValue=255),
          rgb(25,25,0,maxColorValue=255))
temp_meanchange = c(rgb(173,206,226,maxColorValue=255),
                    rgb(248,248,248,maxColorValue=255),
                    rgb(248,248,248,maxColorValue=255),
                    temp_14)
temp_11=c(rgb(103,0,31,maxColorValue=255),rgb(178,24,43,maxColorValue=255),
          rgb(214,96,77,maxColorValue=255),cmyk(0,44,49,0),rgb(253,219,199,maxColorValue=255),
          rgb(247,247,247,maxColorValue=255),rgb(209,229,240,maxColorValue=255),
          rgb(146,197,222,maxColorValue=255),rgb(67,147,195,maxColorValue=255),
          rgb(33,102,172,maxColorValue=255),rgb(5,48,97,maxColorValue=255))
temp_10=c(rgb(103,0,31,maxColorValue=255),rgb(178,24,43,maxColorValue=255),
          rgb(214,96,77,maxColorValue=255),cmyk(0,44,49,0),rgb(253,219,199,maxColorValue=255),
          rgb(209,229,240,maxColorValue=255),rgb(146,197,222,maxColorValue=255),
          rgb(67,147,195,maxColorValue=255),rgb(33,102,172,maxColorValue=255),rgb(5,48,97,maxColorValue=255))
temp_9=c(rgb(178,24,43,maxColorValue=255),rgb(214,96,77,maxColorValue=255),
         cmyk(0,44,49,0),rgb(253,219,199,maxColorValue=255),rgb(247,247,247,maxColorValue=255),
         rgb(209,229,240,maxColorValue=255),rgb(146,197,222,maxColorValue=255),
         rgb(67,147,195,maxColorValue=255),rgb(33,102,172,maxColorValue=255))
temp_8=c(rgb(178,24,43,maxColorValue=255),rgb(214,96,77,maxColorValue=255),
         cmyk(0,44,49,0),rgb(253,219,199,maxColorValue=255),rgb(209,229,240,maxColorValue=255),
         rgb(146,197,222,maxColorValue=255),rgb(67,147,195,maxColorValue=255),rgb(33,102,172,maxColorValue=255))
temp_7=c(rgb(178,24,43,maxColorValue=255),rgb(239,138,98,maxColorValue=255),
         rgb(253,219,199,maxColorValue=255),rgb(247,247,247,maxColorValue=255),
         rgb(209,229,240,maxColorValue=255),rgb(103,169,207,maxColorValue=255),rgb(33,102,172,maxColorValue=255))
temp_6=c(rgb(178,24,43,maxColorValue=255),rgb(239,138,98,maxColorValue=255),
         rgb(253,219,199,maxColorValue=255),rgb(209,229,240,maxColorValue=255),
         rgb(103,169,207,maxColorValue=255),rgb(33,102,172,maxColorValue=255))
temp_5=c(rgb(202,0,32,maxColorValue=255),rgb(244,165,130,maxColorValue=255),
         rgb(247,247,247,maxColorValue=255),rgb(146,197,222,maxColorValue=255),rgb(5,113,176,maxColorValue=255))
temp_5=c(rgb(202,0,32,maxColorValue=255),rgb(244,165,130,maxColorValue=255),
         rgb(247,247,247,maxColorValue=255),rgb(146,197,222,maxColorValue=255),rgb(5,113,176,maxColorValue=255))


#For rcp
col_3rcp=c(rgb(0,52,102,maxColorValue=255),rgb(112,160,205,maxColorValue=255),rgb(153,0,2,maxColorValue=255))
names(col_3rcp)=c("rcp2.6","rcp4.5","rcp8.5")
col_3rcp_shade=c(rgb(67,147,195,maxColorValue=255),rgb(146,197,222,maxColorValue=255),rgb(252,209,197,maxColorValue=255))
names(col_3rcp_shade)=c("rcp2.6","rcp4.5","rcp8.5")
rcp.labs <- c("RCP 2.6","RCP 4.5","RCP 8.5")
names(rcp.labs) <- c("rcp26","rcp45","rcp85")

#For line charts
ipcc_6col=c(rgb(0,0,0,maxColorValue=255),rgb(112,160,205,maxColorValue=255),rgb(196,121,0,maxColorValue=255),rgb(178,178,178,maxColorValue=255),rgb(0,52,102,maxColorValue=255),rgb(0,79,0,maxColorValue=255))


# colors RCM
lColRCM=list("ALADIN63"="#8dd3c7","HadREM3"="#ffffb3","RACMO22E"="#bebada",
             "RCA4"="#fb8072","RegCM4"="#80b1d3","CCLM4"="#fdb462",
             "HIRHAM5"="#b3de69","REMO"="#fccde5","WRF381P"="#d9d9d9")

# colors GCM
lColGCM=list("CNRM"="#377eb8","EC"="#e41a1c","HadGEM2"="#4daf4a",
             "IPSL"="#ff7f00","MPI"="#984ea3","NorESM1"="#666666")

# colors HM
lColHM=list(CTRIP="#A88D72",EROS="#CECD8D",GRSD="#619C6C",J2000="#74AEB9",
            SIM2="#475E6A","MORDOR-SD"="#D8714E","MORDOR-TS"="#AE473E",ORCHIDEE="#EFA59D",
            SMASH="#F6BA62")


# for variance partition
tmp=col2hsl(c(plasma(6),"grey90"))
tmp["S",1:6]=1
tmp["L",1]=0.15
tmp["L",2]=0.35
tmp["L",4]=0.7
tmp["L",6]=0.425

tmp=col2hsl(c(plasma(6),"grey90"))
tmp["S",1:6]=1
tmp["L",1]=0.15
tmp["L",2]=0.35
tmp["L",4]=0.7
tmp["L",6]=0.425
# pal.safe(hsl2col(tmp))

# col=rev(c("orange","yellow","cadetblue1","blue1","darkgreen","darkgoldenrod4","darkorchid1"))
# col_7var=rev(viridis(7))
col_7var=hsl2col(tmp)
tmp=col_7var
##alternating colors for easier rading
col_7var[1]=tmp[2]
col_7var[2]=tmp[5]
col_7var[3]=tmp[3]
col_7var[4]=tmp[6]
col_7var[5]=tmp[4]
col_7var[6]=tmp[1]
col_7var[7]=tmp[7]
names(col_7var)=c("rcp","gcm","rcm","bc","hm","res","int")
legend_7var=c("RCP","GCM","RCM","BC","HM","Variabilité Résiduelle","Variabilité naturelle")


################################################
## Reformat a divergent (around 0) color scale
## Takes a color palette (pal) and a vector of values (values) that should be represented with this color palette centered on 0 
# param is between 0 and 1, the closest it is to 0 the more colors are attributed to the central values, for 1 the palette is unchanged

# ref=seq(0,1,0.01)
# plot(x=ref,y=ref,type = "l",col="black")
# lines(x=ref,y=(ref)^(0.75),col="red")
# lines(x=ref,y=(ref)^(0.5),col="blue")
# lines(x=ref,y=(ref)^(0.25),col="forestgreen")
# lines(x=ref,y=(ref)^(0.1),col="purple")
# legend("bottomright",legend=c("1","0.75","0.5","0.25","0.1"),col=c("black","red","blue","forestgreen","purple"),lty=1)

rescale_divergent_col=function(pal=warmcool(100),values,param){
  pal <- gradient_n_pal(pal)
  tmp=rescale(seq_along(values))
  col_spacing=rescale((abs(tmp-0.5))^param*sign(tmp-0.5))
  return(pal(c(0, col_spacing,1)))
}

################################################
## Reformat a color scale
## Takes a color palette (pal) and a vector of values (values) that should be represented with this color palette
# param is between 0 and 1, the closest it is to 0 the more colors are attributed to the lower values, for 1 the palette is unchanged

# ref=seq(0,1,0.01)
# plot(x=ref,y=ref,type = "l",col="black")
# lines(x=ref,y=(ref)^(0.75),col="red")
# lines(x=ref,y=(ref)^(0.5),col="blue")
# lines(x=ref,y=(ref)^(0.25),col="forestgreen")
# lines(x=ref,y=(ref)^(0.1),col="purple")
# legend("bottomright",legend=c("1","0.75","0.5","0.25","0.1"),col=c("black","red","blue","forestgreen","purple"),lty=1)

rescale_col=function(pal=brewer.blues(100),values,param){
  pal <- gradient_n_pal(pal)
  tmp=rescale(seq_along(values))
  col_spacing=rescale(tmp^param)
  return(pal(c(0, col_spacing,1)))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Included here as it is not exported from ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
binned_pal <- function (palette) {
  function(x) {
    palette(length(x))
  }
}

##################################
## Basemaps

rotate <- function(x) apply(t(x), 2, rev)

background_for_maps=function(path_river,path_fr){
  river=read_shp(path_river)
  fr=read_shp(path_fr)
  river_L2=read_shp(path_river,wgs84_to_l2 = T)
  fr_L2=read_shp(path_fr,wgs84_to_l2 = T)
  options(warn=-1)
  options(warn=0)
  assign("river",river,envir = globalenv())
  assign("river_L2",river_L2,envir = globalenv())
  assign("fr",fr,envir = globalenv())
  assign("fr_L2",fr_L2,envir = globalenv())
}

########################################################
## read and fortify for ggplot plotting shp file

read_shp=function(path,wgs84_to_l2=F){
  shp=sf::st_read(path) ## parameters allows reading
  if(wgs84_to_l2){
    crs_L2=crs("+init=epsg:27572")#Lambert2
    shp=st_transform(shp,crs_L2)
  }
  shp$id <- row.names(shp)
  #shp_fort=fortify(shp)
  return(shp)
}


#####################################################################################################################################
## Make a ggplot2 base map of France with SIM2 outlets as dots and val_name the name of the column for the color scale, that can be customized afterwards
## Data has at least longitude in x, latitude in y and a numeric filling value in val_name


base_map=function(data,zoom){
  
  # adjust extension France
  if(zoom=="FR"|is.null(zoom)){
    xlim =  c(0,1250000)
    ylim = c(1600000,2700000)
  }else if(zoom=="LO"){
    xlim =  c(250000,760000)
    ylim = c(1950000,2250000)
  }else if(zoom=="MOUNTAIN"){
    xlim =  c(300000,1050000)
    ylim = c(1700000,2300000)
  }
  
  plt=ggplot(data=data)+
    coord_sf(xlim = xlim,ylim = ylim,expand=FALSE)+
    scale_x_continuous("")+
    scale_y_continuous("")+
    theme_bw(base_size = 10)+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    theme(axis.ticks =element_blank(),axis.text = element_blank() )+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank())+
    theme(strip.text = element_text(size = 12, face = "bold"))
  
  return(plt)
}

#####################################################################################################################################
## Make a ggplot2 base map of France with SIM2 outlets as dots and val_name the name of the column for the color scale, that can be customized afterwards
## Data has at least longitude in x, latitude in y and a numeric filling value in val_name


get_xy_lim=function(zoom){
  
  # adjust extension France
  if(zoom=="FR"|is.null(zoom)){
    xlim =  c(0,1250000)
    ylim = c(1600000,2700000)
  }else if(zoom=="LO"){
    xlim =  c(250000,760000)
    ylim = c(1950000,2250000)
  }else if(zoom=="MOUNTAIN"){
    xlim =  c(300000,1050000)
    ylim = c(1700000,2300000)
  }
  
  xy_lim=list(xlim=xlim,ylim=ylim)
  
  return(xy_lim)
}


################################################################################
get.output.map.3quant.1rcp.1horiz = function(quant=c("5%","mean","95%"),
                                             dfCoords,
                                             horiz,
                                             vecFileQUALYPSO,
                                             ieff_rcp,
                                             rcp){
  QOUT = readRDS(vecFileQUALYPSO[1])
  Xfut=QOUT$Xfut
  idx_Xfut=which(Xfut==horiz)
  nPoints = nrow(dfCoords)
  
  out=data.frame(x=dfCoords$x,y=dfCoords$y)
  out$idx=seq(1:nrow(out))
  
  # replicate out to add columns related to the rcp/quantile
  tmp=out
  for (j in 1:2){
    out=rbind(out,tmp)
  }
  out$quant=rep(quant,each=nrow(out)/3)
  out$val=0
  
  ieff_this_rcp=which(QOUT$listScenarioInput$listEff[[ieff_rcp]]==rcp)
  
  # index in scenAvail corr to this rcp
  ir = which(QOUT$listScenarioInput$scenAvail[,ieff_rcp]==rcp)
  
  ### loop over the basin
  chg_q5 = chg_q50 = chg_q95 = vector(length=nPoints)
  for(i in 1:nPoints){
    QOUT = readRDS(vecFileQUALYPSO[i])
    phisf = QOUT$CLIMATERESPONSE$phiStar[ir,idx_Xfut]
    chg_q5[i] = quantile(phisf,probs=0.05)
    chg_q50[i] = quantile(phisf,probs=0.5)
    chg_q95[i] = quantile(phisf,probs=0.95)
  }
  
  # convert to percents
  out$val[out$quant==quant[1]]=chg_q5
  out$val[out$quant==quant[2]]=chg_q50
  out$val[out$quant==quant[3]]=chg_q95
  
  # reformat quant as factors
  out$quant=factor(out$quant,levels=quant)
  colnames(out)=c("x","y","idx","quant","val")
  
  return(out)
}


################################################################################
get.output.map.var.part = function(dfCoords,
                                   horiz,
                                   vecFileQUALYPSO){
  QOUT = readRDS(vecFileQUALYPSO[1])
  Xfut=QOUT$Xfut
  idx_Xfut=which(Xfut==horiz)
  nPoints = nrow(dfCoords)
  namesEff = colnames(QOUT$listScenarioInput$scenAvail)
  
  # format points (basin outlets) and initialize data.frame
  out=data.frame(x=dfCoords$x,y=dfCoords$y)
  out$idx=1:nPoints
  for(ee in namesEff) out[ee] = 0
  out$rv = 0
  
  for(i in 1:nPoints){
    QOUT = readRDS(vecFileQUALYPSO[i])
    DEC.raw = QOUT$DECOMPVAR[idx_Xfut,]
    DECOMP = head(DEC.raw,n=(length(namesEff)+1))/(1-tail(DEC.raw,n=1)) 
    for(ee in namesEff) out[ee][i,] = DECOMP[names(DECOMP)==ee]*100
    out$rv[i] = tail(DECOMP,n=1)*100
  }
  
  # reformat out
  out=pivot_longer(out,cols=-c(x,y,idx),names_to="source",values_to = "val")
  out=out[order(out$source),]
  
  return(out)
}

################################################################################
get.accord.rcp85 = function(dfCoords,
                            horiz,
                            vecQOUT,
                            vecCROUT){
  QOUT = readRDS(vecQOUT[1])
  scenAvail = QOUT$listScenarioInput$scenAvail
  Xfut=QOUT$Xfut
  idx_Xfut=which(Xfut==horiz)
  nPoints = nrow(dfCoords)
  
  # format points (basin outlets) and initialize data.frame
  out=data.frame(x=dfCoords$x,y=dfCoords$y)
  out$idx=1:nPoints
  out$val=0
  
  for(i in 1:nPoints){
    QOUT = readRDS(vecQOUT[i])
    CROUT = readRDS(vecCROUT[i])
    idx_rcp=which(scenAvail$EXP=="rcp85")
    phiStar = CROUT$phiStar[idx_rcp,idx_Xfut]
    out$val[i]=mean(phiStar>=0)*100
  }
  
  colnames(out)=c("x","y","idx","val")
  return(out)
}

################################################################################
get.output.map.one.var = function(dfCoords,
                                  horiz,
                                  vecFileQUALYPSO,
                                  vartype){
  QOUT = readRDS(vecFileQUALYPSO[1])
  Xfut=QOUT$Xfut
  idx_Xfut=which(Xfut==horiz)
  nPoints = nrow(dfCoords)
  
  # format points (basin outlets) and initialize data.frame
  out=data.frame(x=dfCoords$x,y=dfCoords$y)
  out$idx=1:nPoints
  out$val=0
  
  if(vartype=="mean"){
    for(i in 1:nPoints){
      QOUT = readRDS(vecFileQUALYPSO[i])
      out$val[i]=QOUT$GRANDMEAN$MEAN[idx_Xfut]
    }
  }else if(vartype=="varint"){
    for(i in 1:nPoints){
      QOUT = readRDS(vecFileQUALYPSO[i])
      out$val[i]=sqrt(QOUT$INTERNALVAR[idx_Xfut])
    }
  }else if(vartype=="varres"){
    for(i in 1:nPoints){
      QOUT = readRDS(vecFileQUALYPSO[i])
      out$val[i]=sqrt(QOUT$RESIDUALVAR$MEAN[idx_Xfut])
    }
  }else if(vartype=="vartot"){
    for(i in 1:nPoints){
      QOUT = readRDS(vecFileQUALYPSO[i])
      out$val[i]= sqrt(QOUT$TOTALVAR[idx_Xfut])
    }
  }else if(vartype=="incert"){# sans IV
    for(i in 1:nPoints){
      QOUT = readRDS(vecFileQUALYPSO[i])
      QOUT$TOTALVAR[idx_Xfut]*(1-tail(QOUT$DECOMPVAR[idx_Xfut,],1))
      out$val[i]= sqrt(QOUT$TOTALVAR[idx_Xfut]*(1-tail(QOUT$DECOMPVAR[idx_Xfut,],1)))
    }
  }
  
  colnames(out)=c("x","y","idx","val")
  return(out)
}

################################################################################
get.output.map.effect = function(x,y,
                                 horiz,
                                 vecFileQUALYPSO){
  QOUT = readRDS(vecFileQUALYPSO[1])
  Xfut=QOUT$Xfut
  idx_Xfut=which(Xfut==horiz)
  nPoints = length(vecFileQUALYPSO)
  
  # format points (basin outlets) and initialize data.frame
  out0=data.frame(x=x,y=y)
  out0$idx=1:nPoints
  
  list.out = list()
  vec.name.eff = colnames(QOUT$listScenarioInput$scenAvail)
  for(name_eff in vec.name.eff){
    # initialize df
    out = out0
    
    # retrieve effects for this type of effect (e.g. RCM)
    ieff=which(colnames(QOUT$listScenarioInput$scenAvail)==name_eff)
    effs.labs <- QOUT$listScenarioInput$listEff[[ieff]]
    
    # rename CNRM-CM5
    if(any(effs.labs=="CNRM-CM5")){
      effs.labs[effs.labs=="CNRM-CM5"] = "CNRM-CM5-LR"
    }
    
    # rename HadREM3-GA7-05
    if(any(effs.labs=="HadREM3-GA7")){
      effs.labs[effs.labs=="HadREM3-GA7"] = "HadREM3-GA7-05"
    }
    
    
    neff = length(effs.labs)
    
    # reformat out
    tmp=out
    for (j in 1:(neff-1)){
      out=rbind(out,tmp)
    }
    out$effs=rep(effs.labs,each=nrow(out)/neff)
    out$val=0
    
    # retrieve effects
    for(i in 1:nPoints){
      QOUT = readRDS(vecFileQUALYPSO[i])
      eff = QOUT$MAINEFFECT[[name_eff]]$MEAN[idx_Xfut,]
      for(ieff in 1:neff){
        out$val[out$effs==effs.labs[ieff]&out$idx==i]=eff[ieff]
      }
    }
    
    list.out[[name_eff]] = out
  }
  
  return(list.out)
}



################################################################################
get.output.summary.MME = function(CROUT,QOUT,nameEffRCP="EXP"){
  
  # rcp scenarios
  vecrcp = c("rcp26","rcp45","rcp85")
  nrcp = length(vecrcp)
  
  # intQ contains a data.frame with the different time series from QUALYPSO
  # for each RCP:
  # chg_q5 and chg_q95 are the reconstructed bounds of the 90% interval describing
  # the variability of the climate change responses
  Xfut = QOUT$Xfut
  nf = length(Xfut)
  
  intQ = data.frame(year=rep(Xfut,nrcp),
                    rcp=c(rep("rcp26",nf),rep("rcp45",nf),rep("rcp85",nf)),
                    chg_q5=c(QOUT$rcp26[1,],QOUT$rcp45[1,],QOUT$rcp85[1,]),
                    chg_mean=c(QOUT$rcp26[2,],QOUT$rcp45[2,],QOUT$rcp85[2,]),
                    chg_q95=c(QOUT$rcp26[3,],QOUT$rcp45[3,],QOUT$rcp85[3,]))
  
  # we also represent the variability of the raw climate change projections Ystar
  scenAvail = QOUT$listScenarioInput$scenAvail
  Ystar=CROUT$YStar
  dfMinMax = list()
  y = QOUT$Xmat[1,]
  ny = length(y)
  for(rcp in vecrcp){
    zz = scenAvail[nameEffRCP]==rcp
    minYstar = apply(Ystar[zz,],2,min)
    maxYstar = apply(Ystar[zz,],2,max)
    dfMinMax[[rcp]] = data.frame(year=y,rcp=rep(rcp,ny),min=minYstar,max=maxYstar)
  }
  intMinMaxYstar=rbind(dfMinMax$rcp26,dfMinMax$rcp45,dfMinMax$rcp85)
  
  return(list(intQ=intQ,intMinMaxYstar=intMinMaxYstar))
}

# out: output of get.output.summary.MME
# scale: unit for the y-axis
plot.summary.MME = function(out,add.legend=T,istas=F,label="",
                            dfminmaxHM=NULL,xlim=c(1985,2105),ylims=NULL){
  # choose labels and limits
  if(!istas){
    addUnit <- scales::percent
    ymax = 5
  }else{
    addUnit <- function(x,...) format(paste0(x, "?C"), ...)
    ymax = 10
  }
  
  if(is.null(ylims)){
    x = c(out$intMinMaxYstar$min,out$intMinMaxYstar$max)
    ylims = c(min(x,na.rm=T), min(ymax,max(x,na.rm=T)))
  }
  
  plt=ggplot(out$intQ)+
    geom_hline(yintercept = 0,linetype = "dashed")+
    geom_ribbon(data=out$intMinMaxYstar,
                aes(x=year,ymin=min,ymax=max,fill=rcp),alpha=0.3,
                linetype="dotted",color="gray40")+# min/max Ystar
    scale_fill_discrete("Variabilit? naturelle",type= as.vector(col_3rcp_shade))+
    guides(fill=guide_legend(order=3,nrow=1, byrow=TRUE,title.theme=element_text(size = 10),
                             label = F))+
    new_scale_fill()+
    geom_ribbon(aes(x=year,ymin=chg_q5,ymax=chg_q95,fill=rcp),alpha=0.6)+#uncertainty band
    scale_fill_discrete("Dispersion mod?le",type= as.vector(col_3rcp))+
    guides(fill=guide_legend(order=2,nrow=1, byrow=TRUE,title.theme=element_text(size = 10),
                             label = F))+
    theme_bw(base_size = 12)+
    theme(axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    theme(legend.key.width = unit(1.5,"cm"))+
    theme(legend.title = element_text(size=10))+
    theme(legend.margin = margin(-2, 0, -2, 0))+
    facet_wrap(~factor(rcp,levels=names(rcp.labs)),nrow = length(rcp.labs),
               strip.position = "top",labeller = as_labeller(rcp.labs))+
    theme(panel.border = element_rect(colour = "black", fill=NA))+
    theme(strip.text = element_text(face="bold", size=12,margin = margin(0.1,0.1,0.1,0.1, "cm")))+
    geom_text(data=data.frame(lab="a",rcp="rcp26"),
              aes(x=-Inf, y = Inf, label = label), vjust=1.1, hjust=-0.5,parse=T,size=6)+
    scale_x_continuous(data.frame(),limits=xlim,expand=c(0,0),
                       breaks = c(2000,2050,2100),minor_breaks = seq(1990,2100,10))+
    theme(plot.margin = margin(0,0,0,0, "cm")) +
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    scale_y_continuous(expand=c(0,0),oob=squish, labels = addUnit,
                       limits = function(x){ylims})+ylab("")+
    theme(strip.background = element_blank(),strip.text.y = element_blank())
  
  if(is.null(dfminmaxHM)){
    plt = plt +
      geom_line(aes(x=year,y=chg_mean,group=rcp,color=rcp),linewidth=0.8)+#RCP mean
      scale_color_discrete("Moyenne d'ensemble",type= as.vector(col_3rcp),labels=NULL)+
      guides(color=guide_legend(order=1,nrow=1, byrow=TRUE,title.theme=element_text(size = 10)))
  }else{
    plt = plt +
      new_scale_fill() +
      geom_ribbon(data=dfminmaxHM,aes(x=year,ymin=min,ymax=max,fill=rcp),linetype="solid")+
      scale_fill_discrete("Dispersion HM reconstitu?e",type= as.vector(col_3rcp))+
      guides(fill=guide_legend(order=1,nrow=1, byrow=TRUE,title.theme=element_text(size = 10),label = F))
  }
  
  if(!add.legend){
    plt=plt+
      theme(legend.position="none")
  }
  
  return(plt)
}

################################################################################
get.output.trendsign = function(CROUT,QOUT,nameEffRCP="EXP"){
  scenAvail = QOUT$listScenarioInput$scenAvail
  Xfut = QOUT$Xfut
  
  # rcp scenarios
  vecrcp = c("rcp26","rcp45","rcp85")
  nrcp = length(vecrcp)
  
  perc_pos=vector(mode="list",length=nrcp)
  for (r in 1:nrcp){
    idx_rcp=which(scenAvail[nameEffRCP]==vecrcp[r])
    phiStar = CROUT$phiStar[idx_rcp,]
    n_chain=nrow(phiStar)
    perc_pos[[r]]=apply(phiStar,MARGIN=2,function(x) sum(x>=0)/n_chain*100)
  }
  
  data=data.frame(cbind(Xfut,do.call("cbind",perc_pos)))
  colnames(data)=c("year",vecrcp)
  
  # remove 1990 and by decade only
  yz = seq(from=2000,to=2100,by=10)
  data = data[data$year%in%yz,]
  
  out=pivot_longer(data,cols=!year,names_to = "rcp",values_to = "val")
  out$cat="Pas de tendance"
  out$cat[out$val>=80]="Augmentation"
  out$cat[out$val<=20]="Diminution"
  out$cat=factor(out$cat,levels=c("Augmentation","Pas de tendance","Diminution"))
  return(out)
}

plot.trendsign = function(out,add.legend,xlim=c(1985,2105),label=""){
  
  plt=ggplot(out)+
    geom_point(aes(x=year,y=factor(rcp,levels=c("rcp85","rcp45","rcp26")),
                   shape=cat,color=rcp,fill=rcp))+
    scale_shape_manual("Accord\n sur la tendance",
                       values = c("Diminution"=25,"Pas de tendance"=1,"Augmentation"=24),
                       guide = guide_legend(direction = "vertical",title.position = "top"),
                       drop = FALSE)+
    scale_color_manual(values  = as.vector(col_3rcp))+
    scale_fill_manual(values = as.vector(col_3rcp))+
    theme_bw(base_size = 12)+
    theme(legend.title = element_text(size=10))+
    theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),axis.text.y = element_blank(),
          axis.ticks = element_blank(),axis.text.x = element_blank())+
    theme(legend.key.height = unit(0.5,"cm"))+
    guides(shape = guide_legend(override.aes = list(size = 3)))+
    ylab("")+
    annotate("text",  x=-Inf, y = Inf, label = label, vjust=1.1, hjust=-0.5,parse=T,size=6)+
    scale_x_continuous("",limits=xlim,expand=c(0,0),breaks = c(2000,2050,2100),
                       minor_breaks = seq(1990,2100,10))
  
  if(!add.legend){
    plt=plt+
      theme(legend.position="none")
  }
  
  return(plt)
}

################################################################################
get.output.decompANOVA = function(QOUT,namesEff){
  
  Xfut = QOUT$Xfut
  nFut = length(Xfut)
  nEff = QOUT$listScenarioInput$nEff
  vecEff = 1:nEff
  VARDECOMP = QOUT$DECOMPVAR
  cum = rep(0, nFut)
  data=VARDECOMP
  for (j in 1:(nEff + 2)) {
    cumPrevious = cum
    cum = cum + VARDECOMP[, j]
    data[,j] = cum
  }
  data=data.frame(cbind(Xfut,data))
  names_var=c(namesEff,"res","int")
  colnames(data)=c("Xfut",names_var)
  data=pivot_longer(data=data,cols=-c(Xfut),names_to = "var",values_to = "val")
  data$val=data$val*100 #percentage
  
  return(data)
}


plot.decompANOVA = function(out,namesEff,add.legend,xlim=c(1985,2105),label="",add.ylab=F,ylab=""){
  names_var=c(namesEff,"res","int")
  vec_color=col_7var[names(col_7var) %in% names_var]
  out$var=factor(out$var,levels=rev(names(vec_color)))
  labels_var=rev(legend_7var[names(col_7var) %in% names_var])
  addUnit <- function(x,...) format(paste0(x, "%"), ...)
  
  plt=ggplot(out)+
    geom_ribbon(aes(x=Xfut,ymin=0,ymax=val,fill=var,alpha=var))+
    scale_fill_discrete("",type = vec_color,labels=labels_var)+
    scale_alpha_manual("",values=c(0.7,rep(1,length(vec_color)-1)),labels=labels_var)+
    theme_bw(base_size = 12)+
    theme(legend.title = element_text(size=10))+
    theme(panel.border = element_blank(),axis.ticks = element_blank(),axis.text.x = element_blank(),
          axis.line.y = element_line(colour = "black"))+
    annotate("text",  x=-Inf, y = Inf, label = label, vjust=1.1, hjust=-0.5,parse=T,size=6)+
    theme(legend.position="right",legend.justification="left", legend.box.spacing = unit(0, "pt"))+
    scale_x_continuous("",limits=xlim,expand=c(0,0),breaks = c(2000,2050,2100),
                       minor_breaks = seq(1990,2100,10))
  
  # add y label
  if(add.ylab){
    plt=plt+
      scale_y_continuous(ylab,limits = c(0,100),expand=c(0,0),oob=squish, labels = addUnit)
  }else{
    plt=plt+
      scale_y_continuous(limits = c(0,100),expand=c(0,0),oob=squish, labels = addUnit)+ylab("")
  }
  
  if(!add.legend){
    plt=plt+
      theme(legend.position="none")
  }
  
  return(plt)
}

################################################################################
# plot effects of a main effect
get.output.effect = function(QOUT,nameEff){
  Xfut = QOUT$Xfut
  iEff = which(QOUT$namesEff == nameEff)
  nEff = QOUT$listScenarioInput$nTypeEff[iEff]
  if (length(iEff) == 0){    stop("wrong value for nameEff")} 
  EffHat = data.frame(QOUT$MAINEFFECT[[nameEff]]$MEAN)
  
  # format the mean effects
  colnames(EffHat)=QOUT$listScenarioInput$listEff[[iEff]]
  EffHat$year=Xfut
  data=pivot_longer(data=EffHat,cols=!year,names_to = "eff",values_to = "data")
}

plot.effect = function(out,ylims,nameEff,labEff,add.ylab=F,istas=F,add.legend=T,
                       ylab=NULL,ltypes=NULL,add.ticks=F,label="",xlim=c(1985,2105)){
  # choose colors
  nEff = length(labEff)
  if(nameEff%in%c("rcp","EXP")){
    vecCol = col_3rcp
  }else if(nameEff=="HM"){
    vecCol = vector(length=nEff)
    for(ihm in 1:nEff) vecCol[ihm] = lColHM[[labEff[ihm]]]
  }else if(nameEff=="GCM"){
    vecCol = vector(length=nEff)
    for(igcm in 1:nEff){
      lab = strsplit(labEff[igcm],'-')[[1]][1]
      vecCol[igcm] = lColGCM[[lab]]
    }
  }else if(nameEff=="RCM"){
    vecCol = vector(length=nEff)
    for(ircm in 1:nEff){
      lab = strsplit(labEff[ircm],'-')[[1]][1]
      vecCol[ircm] = lColRCM[[lab]]
    }
  }else if(nameEff=="BC"){
    vecCol = c("#e41a1c","#377eb8")
  }
  
  if(is.null(ltypes)){
    ltypes = rep("solid", nEff)
  }
  
  
  # prepare the plot
  plt=ggplot(out)+
    geom_hline(yintercept = 0,linetype = "dashed")+
    geom_line(aes(x=year,y=data,group=eff,color=eff, linetype = eff),linewidth=1.2)+
    theme_bw(base_size = 18)+
    scale_color_manual(name=nameEff,values=vecCol |> `names<-`(labEff))+
    scale_linetype_manual(name=nameEff,values = ltypes |> `names<-`(labEff))+
    theme( plot.background = element_blank() ,
           panel.grid.major = element_blank() ,
           panel.grid.minor = element_blank() ,
           panel.border = element_blank() ,
           panel.background = element_blank() ) +
    theme(axis.line = element_line(color = 'black'))
  
  # choose labels and limits
  if(!istas){
    if(is.null(ylab)) ylab = "Effet principal (%)"
    addUnit <- scales::percent
  }else{
    if(is.null(ylab)) ylab = "Effet principal (?C)"
    addUnit <- function(x,...) format(paste0(x, "?C"), ...)
  }
  
  # add y label
  if(add.ylab){
    plt=plt+
      scale_y_continuous(ylab,limits = ylims,expand=c(0,0),oob=squish, labels = addUnit)
  }else{
    plt=plt+
      scale_y_continuous(limits = ylims,expand=c(0,0),oob=squish, labels = addUnit)+ylab("")
  }
  
  
  plt = plt +
    labs(title=NULL)+
    annotate("text",  x=-Inf, y = Inf, label = label, vjust=1.1, hjust=-0.5,parse=T,size=6)+
    theme_bw(base_size = 12)+
    scale_x_continuous("",limits=xlim,expand=c(0,0),breaks = c(2000,2050,2100),
                       minor_breaks = seq(1990,2100,10))
  
  if(!add.ticks){
    plt = plt +
      theme(axis.text.x = element_blank())
  }
  
  
  if(!add.legend){
    plt=plt+
      theme(legend.position="none")
  }
  
  return(plt)
}
