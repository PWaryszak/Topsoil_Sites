#GGMAPS Libraries:===========
install.packages("ggplot2", "grid", "maptools", "maps", "ggmap" dependencies = TRUE)
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

#load file with GPS coordinates:
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
