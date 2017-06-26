#GGMAPS Libraries:===========
#install.packages("ggplot2", "grid", "maptools", "maps", "ggmap" dependencies = TRUE)
library(grid)
library(maptools)
library(maps)
library(ggplot2)
library(ggmap)

#MAP 1==========
#Reproducing the Figure 3.2 from Pawel's Thesis:
#Location of topsoil donor site at Jandakot (circle)
#and two topsoil recipient sites at Forrestdale Lake (upper triangle) and Anketell 
#Road (bottom triangle). Topsoil was collected and transferred in April-May 2012.

#load csv file with GPS coordinates:
sitemap <- read.csv("sitemap.csv")
str(sitemap)#72 obs. of  10 variables:

#Map of FSW & AnkM (topsoil recipients) + Jandakot Airport (topsoil donor)============
ank<- sitemap[ 15, c("site", "lat", "lon") ] #AnkM.DROC -32.20946 115.9096  AnkM 
ank #view to double check
forSW<- sitemap [ 63, c("site", "lat", "lon") ] #ForSW.DROC -32.16611 115.9271 ForSW 
forSW #view to double check
jandakot <- data.frame (site = "Jandakot",
                        lat = -32.094983,
                        lon = 115.864268)
jandakot #view to double check, I read this Coordinates of DPaW report.

transferMap<- rbind(jandakot, ank, forSW) #bind them
transferMap$site2<-c("Jandakot", "Anketell", "Forrestdale") #name the study sites
transferMap$Topsoil <-c("Donor", "Recipient", "Recipient") #name the topsoil sites

#Define middleof the map location
mean(transferMap$lat)# -32.15685 = our middle coordinates
mean(transferMap$lon)#115.9003  = our middle coordinates

#MAP IT!
#maptype can be = "roadmap", "watercolor", "terrain", "satellite", "toner"

glgmap<-get_map(location = c(lon =115.9003, lat =-32.15685),maptype = "terrain" , zoom= 11)#,  extent = "normal")# ,color = "bw") #getting our map as the canvas 
transfer <- ggmap(glgmap)#combine google map with ggplot using ggmap function
transfer #base map - have a look

transfer1<-transfer + geom_point(aes(x = lon, y = lat, color = Topsoil, shape = Topsoil, fill=Topsoil), size=10,  data = transferMap) #placing the points on our map that 
transfer2<-transfer1 +ylab("Latitude") +xlab("Longitude")+scale_color_manual(values=c("red", "yellow"))
transfer3 <- transfer2 +theme( axis.text.x=element_text(size=15), #define your own theme parts
                               axis.text.y=element_text(size=15),
                               axis.title.x=element_text(size=20),
                               axis.title.y=element_text(size=20))
transfer3

transfer4<-transfer3 +scale_shape_manual(values=c(21,25,25))+scale_fill_manual(values=c("red", "yellow"))
transfer4 #Final Map
ggsave(transfer4, file="Topsoil_Sites_Map1.png", width=8, height=4)


#MAP 2 My Study Site Location in South-Western Australia ======
#(Figure 3.3 in Version 23june2017 of my thesis).
library(grid)
library(maptools)
library(maps)
library(ggplot2)
library(ggmap)

SiteMapURL<- "https://raw.githubusercontent.com/PWaryszak/Topsoil_Sites/master/sitemap.csv"
sitemap <- read.csv(SiteMapURL)#reading-in straight from online file.
str(sitemap)#72 obs. of  10 variables:

MyStudySitesPin<- data.frame(CentralLon = mean(sitemap$lon),
                             CentralLat = mean(sitemap$lat))
MyStudySitesPin# Computed the central location for all my 6 sites (12 clusters of plot each)
#CentralLon CentralLat
# 115.9184  -32.18693

#maptype can be = "roadmap", "watercolor", "terrain", "satellite", "toner"
#I chose "toner"

glgmap<-get_map(location = c(lon = MyStudySitesPin[,"CentralLon"], lat = MyStudySitesPin[,"CentralLat"])
                ,maptype = "toner" , zoom= 7,color = "bw")#getting our map as the canvas 
MyStudySites <- ggmap(glgmap)#combine google map with ggplot using ggmap function
MyStudySites #base map
MyStudySites1<-MyStudySites + geom_point(aes(x = CentralLon, y = CentralLat, fill = "red"),
                                         size=8, shape = 22, data = MyStudySitesPin) #placing the pin on our map.Play with shapes from 0 to 25.
MyStudySites2 <- MyStudySites1 + annotate('text', x = 117.8, y=-32.16644, label = "Study Sites",colour = I("red"), size = 10)
MyStudySites2a<-MyStudySites2 +  ylab("Latitude") + xlab("Longitude")
MyStudySites3 <- MyStudySites2a +theme( axis.text.x=element_text(size=18), #define your own theme parts
                      axis.text.y=element_text(size=18),
                      axis.title.x=element_text(size=20),
                      axis.title.y=element_text(size=20),
                      legend.position = "none")
MyStudySites3


#MAP 3 Continent-scale of my Study Site Location======
#To be included in my poster fro ESA2017 in Portland, USA
if (!require(grid)) library(grid) # load ggplot package if not done already 
if (!require(maptools)) library(maptools) # load ggplot package if not done already 
if (!require(ggplot2)) library(ggplot2) # load ggplot package if not done already 
if (!require(ggmap)) library(ggmap) # load ggplot package if not done already 

SiteMapURL<- "https://raw.githubusercontent.com/PWaryszak/Topsoil_Sites/master/sitemap.csv"
sitemap <- read.csv(SiteMapURL)#reading-in straight from online file.
str(sitemap)#72 obs. of  10 variables:

MyStudySitesPin<- data.frame(CentralLon = mean(sitemap$lon),
                             CentralLat = mean(sitemap$lat),
                             AusCentreLat = -25.610111,
                             AusCentreLon = 134.354806 )
MyStudySitesPin# Computed the central location for all my 6 sites (12 clusters of plot each)
#CentralLon CentralLat
# 115.9184  -32.18693

#maptype can be = "roadmap", "watercolor", "terrain", "satellite", "toner"
#I chose "toner"

glgmap<-get_map(location = c(lon = MyStudySitesPin[,"AusCentreLon"], lat = MyStudySitesPin[,"AusCentreLat"])
                ,maptype = "terrain", zoom = 4)#getting our map as base we will put our pin on. 
ContFrame <- ggmap(glgmap)#combine google map with ggplot using ggmap function
ContFrame #base map
ContFrame1<-ContFrame + geom_point(aes(x = CentralLon, y = CentralLat, color = "red"),
                                         size=15, shape = 0,stroke = 2, data = MyStudySitesPin) #placing the pin on our map.Play with shapes from 0 to 25.
ContFrame2<-ContFrame1 +  ylab("") + xlab("")
ContFrame3 <- ContFrame2 +theme( axis.text.x=element_text(size=10), #define your own theme parts
                                        axis.text.y=element_text(size=10),
                                        axis.title.x=element_text(size=12),
                                        axis.title.y=element_text(size=12),
                                        legend.position = "none")
ContFrame3
ggsave(ContFrame3, file="Topsoil_Sites_ContinetalScaleMap2.png", width=8, height=4)
