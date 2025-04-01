# Guillaume Evin
# Explore2
# Figure 2: map of the regimes

library(sp)
library(purrr)
library(ggnewscale)
library(ggspatial)
library(tidyverse)

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
colRegime = c("PVC" = "#148530ff",
              "PC" ="#21de50ff",
              "P" = "#7aeb96ff",
              "PNC" = "#d3f8dcff",
              "PN" = "#88c4ffff",
              "NP" = "#2291ffff",
              "N" = "#0027a2ff")


##########################################
# plot 1: Parde coefficients per regime  #
##########################################
data <- tibble(Month = 1:12,
               P = c(0.125,0.131,0.117,0.114,0.100,0.064,0.043,0.032,0.033,0.054,0.076,0.109),
               PC = (c(0.147,0.151,0.114,0.112,0.093,0.051,0.028,0.020,0.022,0.050,0.083,0.128)+
                       c(0.131,0.128,0.115,0.098,0.079,0.051,0.033,0.023,0.032,0.070,0.103,0.129))/2,
               PVC=c(0.179,0.169,0.126,0.098,0.065,0.038,0.025,0.016,0.018,0.042,0.079,0.138),
               PNC=c(0.103,0.101,0.101,0.095,0.085,0.076,0.069,0.063,0.063,0.068,0.077,0.091),
               PN=c(0.118,0.099,0.090,0.105,0.100,0.044,0.023,0.016,0.039,0.106,0.138,0.126),
               NP=c(0.090,0.091,0.104,0.125,0.131,0.079,0.047,0.032,0.042,0.078,0.088,0.091),
               N=c(0.033,0.031,0.043,0.070,0.154,0.202,0.141,0.081,0.067,0.068,0.055,0.038))

# labels
datalong <- gather(data, key="Regime", value="CM", -Month)
datalong$Regime = factor(datalong$Regime,levels=c("PVC","PC","P","PNC","PN","NP","N"))
regime_label <- c("Pluvial - Very contrasted (PVC)", "Pluvio-Nival (PN)",
                  "Pluvial - Contrasted (PC)", "Nivo-Pluvial (NP)", "Pluvial (P)",
                  "Nival (N)","Pluvial - Not contrasted (PNC)")
names(regime_label) = c("PVC","PN","PC","NP","P","N","PNC")

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
coordsL2$code = code_sel

# domain France
myzoom = "FR"

# read regime
dfAllRegime = read.csv2("./dataTotSAF_regimeAuMoins4Modele.csv") # 2500
iBasinReg = match(code_sel,dfAllRegime$code)
dfRegime = dfAllRegime[iBasinReg,]
sum(abs(match(dfRegime$code,coordsL2$code)-1:1735)) # 0

# build data.frame for the plot
coordsL2$regime = factor(dfRegime$hydro_regime, 
                         levels=c("PTC","PC","P","PMC","PN","NP","N"))

# call plot
map_regime = base_map(data = coordsL2, zoom = myzoom)+
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
  annotation_scale(text_cex =1.5) + theme(legend.position="none")

# merge
plt = ggarrange(barplot_CM,map_regime,ncol=2,widths = c(20, 20),
                labels = c("A","B"),font.label = list(size = 20, color = "black", face = "bold", family = NULL))
ggsave(filename = "../FIGURES/Fig1_regime.jpg", plot=plt,device = "jpeg",units="cm",height=20,width=40,dpi=200)