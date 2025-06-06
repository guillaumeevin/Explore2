# Guillaume Evin
# Explore2
# Figure 2: map of the regimes

library(sp)
library(purrr)
library(ggnewscale)
library(ggspatial)
library(tidyverse)
library(RColorBrewer)
library(raster)
library(maps)
library(prettymapr)
library(aplot)
library(patchwork)

source("./lib_functions.r")

##############
# Resources #
##############

path_sig = "../SIG/"
path_river=paste0(path_sig,"processed/CoursEau_idx1_wgs84.shp")
path_fr=paste0(path_sig,"raw/IGN/contours_FR/gadm36_FRA_0.shp")
background_for_maps(path_river,path_fr)


#######
# colors
colRegime = c("HCP" = "#148530ff",
              "CP" ="#21de50ff",
              "P" = "#7aeb96ff",
              "NCP" = "#d3f8dcff",
              "PN" = "#88c4ffff",
              "NP" = "#2291ffff",
              "N" = "#0027a2ff")

shadowtext <- function(x, y=NULL, labels, col='white', bg='black',
                       theta= seq(0, 2*pi, length.out=50), r=0.1, ... ) {
  
  xy <- xy.coords(x,y)
  xo <- r*strwidth('A')
  yo <- r*strheight('A')
  
  # draw background text with small shift in x and y in background colour
  for (i in theta) {
    text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
  }
  # draw actual text in exact xy position in foreground colour
  text(xy$x, xy$y, labels, col=col, ... )
}

# Fonction transparence:
transp.col <- function(x,alpha){
  opal <- col2rgb(x, alpha = TRUE)/255
  opal["alpha", ] <- alpha
  mycol <- do.call(rgb, as.list(as.data.frame(t(opal))))
  return(mycol)
}

mnt.rel=raster("../SIG/MNT250_LIIe_FRANCE.tif",crs = "+init=epsg:27572")
mnt.shd=raster("../SIG/france_shd.tif",crs = "+init=epsg:27572")
mers.sp=sf::st_read("../SIG/oceans")
rivs.lg=sf::st_read("../SIG/Rivieres")
fleuve.lg=sf::st_read("../SIG/Fleuves")
euro.sp=sf::st_read("../SIG/Europe_Ouest")
euro.sp.notFR = euro.sp[euro.sp$ne_10m_adm!="FRA",]

#Palette de couleurs pour le relief:
lvl.mnt   = seq(0,1900, by=100)
lab.mnt = c("0","","200","","400","","600","","800","","1000","","1200","",
            "1400","","1600","","1800","")
nlvl.mnt  = length(lvl.mnt)
col.mnt   = rev(c("#F5F4F2","#E0DED8","#CAC3B8","#BAAE9A","#AC9A7C","#AA8753",
                  "#B9985A","#C3A76B","#CAB982","#D3CA9D","#DED6A3","#E8E1B6",
                  "#EFEBC0","#E1E4B5","#D1D7AB","#BDCC96","#A8C68F","#94BF8B","#ACD0A5"))
myPal.mnt = binned_pal(scales::manual_pal(col.mnt))

# Palette de couleurs pour le shade:
lvl.shd   = seq(0,254, by=0.5)
nlvl.shd  = length(lvl.shd)
col.shd   = gray.colors(n=100,start=0.9,end=0.1)
myPal.shd = binned_pal(scales::manual_pal(col.shd))


##########################################
# plot 1: carte France avec relief  #
##########################################

mnt.rel.spdf <- as(mnt.rel, "SpatialPixelsDataFrame")
mnt.rel.df <- as.data.frame(mnt.rel.spdf)
colnames(mnt.rel.df) <- c("z", "x", "y")

xlim =  c(0,1250000)
ylim = c(1600000,2700000)
map_relief = ggplot()+
  scale_x_continuous("")+
  scale_y_continuous("")+
  theme_bw(base_size = 10)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  theme(axis.ticks =element_blank(),axis.text = element_blank() )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank())+
  theme(strip.text = element_text(size = 12, face = "bold"))+
  geom_tile(data=mnt.rel.df, aes(x=x, y=y, fill=z))+
  guides(fill=guide_colorbar(barwidth = 1.5, barheight = 20,
                              frame.colour = "black", ticks.colour = "black",
                              label.theme = element_text(size = 14, face = c("bold"),color=c("black")),
                              title.theme=element_text(size = 14, face = "bold")))+
  binned_scale(aesthetics = "fill",name="Elevation [m]",palette=myPal.mnt,limits=range(lvl.mnt),
                guide="coloursteps",breaks=lvl.mnt,labels = lab.mnt)+
  geom_sf(data=euro.sp.notFR,fill="white")+
  geom_sf(data=river_L2,colour="#C7E3F1",linewidth=0.1,alpha=0.5)+
  geom_sf(data=fr_L2,fill=NA,linewidth=0.1,color="black")+
  geom_sf(data=mers.sp,fill="#C7E3F1")+
  coord_sf(xlim = xlim, ylim = ylim,expand=FALSE)+
  annotate("text", x = 500000, y = 1750000, label = "Pyrénées", size=9)+
  annotate("text", x = 700000, y = 1920000, label = "Cévennes",angle=45, size=9)+
  annotate("text", x = 650000, y = 2050000, label = "Massif", size=9)+
  annotate("text", x = 650000, y = 2000000, label = "Central", size=9)+
  annotate("text", x = 900000, y = 2000000, label = "Alps", size=9)+
  annotate("text", x = 900000, y = 1770000, label = "Mediterranean",col="darkblue", size=9)+
  annotate("text", x = 900000, y = 1720000, label = "sea",col="darkblue", size=9)+
  annotate("text", x = 150000, y = 2100000, label = "Atlantic",angle=90,col="darkblue", size=9)+
  annotate("text", x = 200000, y = 2100000, label = "Ocean",angle=90,col="darkblue", size=9)+
  annotate("text", x = 320000, y = 2600000, label = "English Channel",angle=10,col="darkblue", size=9)+
  annotate("text", x = 599767, y = 2429816, label = "Paris", size=9)+
  annotate("text", x = 599767, y = 2429816-50000, label = "Basin", size=9)+
  annotate("text", x = 880000, y = 2200000, label = "Jura",angle=60, size=9)+
  annotate("text", x = 940000, y = 2350000, label = "Vosges",angle=70, size=9)+
  annotate("text", x = 1160000, y = 1710000, label = "Corsica",angle=90, size=9)+
  annotation_scale(text_cex =1.5)
  
  
##########################################
# plot 1: Parde coefficients per regime  #
##########################################
data <- tibble(Month = 1:12,
               P = c(0.125,0.131,0.117,0.114,0.100,0.064,0.043,0.032,0.033,0.054,0.076,0.109),
               CP = (c(0.147,0.151,0.114,0.112,0.093,0.051,0.028,0.020,0.022,0.050,0.083,0.128)+
                       c(0.131,0.128,0.115,0.098,0.079,0.051,0.033,0.023,0.032,0.070,0.103,0.129))/2,
               HCP=c(0.179,0.169,0.126,0.098,0.065,0.038,0.025,0.016,0.018,0.042,0.079,0.138),
               NCP=c(0.103,0.101,0.101,0.095,0.085,0.076,0.069,0.063,0.063,0.068,0.077,0.091),
               PN=c(0.118,0.099,0.090,0.105,0.100,0.044,0.023,0.016,0.039,0.106,0.138,0.126),
               NP=c(0.090,0.091,0.104,0.125,0.131,0.079,0.047,0.032,0.042,0.078,0.088,0.091),
               N=c(0.033,0.031,0.043,0.070,0.154,0.202,0.141,0.081,0.067,0.068,0.055,0.038))

# labels
datalong <- gather(data, key="Regime", value="CM", -Month)
datalong$Regime = factor(datalong$Regime,levels=c("HCP","CP","P","NCP","PN","NP","N"))
regime_label <- c("Highly Contrasted Pluvial (HCP)", "Pluvio-Nival (PN)",
                  "Contrasted Pluvial (CP)", "Nivo-Pluvial (NP)", "Pluvial (P)",
                  "Nival (N)","Non-Contrasted Pluvial (NCP)")
names(regime_label) = c("HCP","PN","CP","NP","P","N","NCP")

# plot 2: seasonality of the monthly mean flow by regime
barplot_CM = ggplot(datalong, aes(x=Month, y=CM,fill=Regime))+
  geom_bar(stat='identity', width = 1)+
  facet_wrap(~Regime, ncol = 2,labeller = labeller(Regime = regime_label),dir = "v") +
  scale_fill_manual(values = colRegime) + 
  scale_x_continuous(breaks=seq(1,12),labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  theme_bw()+
  theme(strip.background = element_blank(),strip.text = element_text(face="bold", size=14),
        axis.text.x =element_text(size=12), axis.text.y =element_text(size=12), 
        axis.title.y=element_blank(),axis.title.x = element_blank(),
        legend.text = element_text(size=16),legend.title = element_text(size=20))


##########################################
# plot 2: map of the regimes            #
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
coordsL2$code = code_sel

# read regime
dfAllRegime = read.csv2("./dataTotSAF_regimeAuMoins4Modele.csv") # 2500
iBasinReg = match(code_sel,dfAllRegime$code)
dfRegime = dfAllRegime[iBasinReg,]
sum(abs(match(dfRegime$code,coordsL2$code)-1:1735)) # 0

# build data.frame for the plot
coordsL2$regime = factor(dfRegime$hydro_regime, 
                         levels=c("PTC","PC","P","PMC","PN","NP","N"))

# call plot
map_regime = base_map(data = coordsL2, zoom = "FR")+
  geom_sf(data=river_L2,colour="gray80",linewidth=1) +
  geom_sf(data=fr_L2,fill=NA,linewidth=0.5,color="black") +
  geom_point(aes(x=x,y=y,fill = regime),colour="gray",pch=21, size=3)+
  scale_fill_manual(name = "Regime", 
                    values = c("PTC" = "#148530ff",
                               "PC" ="#21de50ff",
                               "P" = "#7aeb96ff",
                               "PMC" = "#d3f8dcff",
                               "PN" = "#88c4ffff",
                               "NP" = "#2291ffff",
                               "N" = "#0027a2ff")) +
  theme(legend.title=element_text(face = "bold",size=16), 
        legend.text=element_text(size=14)) +
  theme(legend.position="none")


##########################################
# plot 3: map of Budyko coefficients     #
##########################################
lBudyko = readRDS("../SAFRAN/Budyko.rds")
background_for_maps(path_river,path_fr)

# coordinates France/SAFRAN grid: list of matrices
lCoords = readRDS(file = "./coordMeteo.rds")
isPixel = lCoords$mask
indexPixel = which(isPixel==1,arr.ind = T)
nPixels = sum(isPixel)
selPixel = as.logical(isPixel)
dfBudyko = data.frame(x=as.vector(lCoords$x_l2[selPixel]),
                      y=as.vector(lCoords$y_l2[selPixel]),
                      budyko = as.vector(1/lBudyko$Budyko[selPixel]))

map_budyko = base_map(data = dfBudyko,zoom="FR")+
  geom_tile(aes(x=x,y=y,fill=budyko))+
  geom_sf(data=river_L2,colour="gray80",linewidth=0.1,alpha=0.5)+
  geom_sf(data=fr_L2,fill=NA,linewidth=0.1,color="black")+
  guides(fill=guide_colorbar(barwidth = 1.5, barheight = 15,title.position = "top"))+
  binned_scale(aesthetics = "fill",name="[P/PET]",
               palette = binned_pal(scales::manual_pal(precip_6)),
               guide="coloursteps",limits=c(0, 2.5),
               breaks=c(0, 0.375, 0.75, 1, 1.5, 2, 2.5),show.limits = T,oob=squish)+
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.title=element_text(face = "bold",size=14), legend.text=element_text(face = "bold",size=14))


# merge
plt = (map_relief|map_budyko) / (barplot_CM+map_regime) + 
  plot_layout(heights = c(20, 20),widths = c(25, 23)) + 
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 40, face = "bold"))

ggsave(filename = "../FIGURES/Fig1_regime.jpg", plot=plt,device = "jpeg",units="cm",height=40,width=48,dpi=200)