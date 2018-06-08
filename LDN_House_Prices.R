#-----SET WORKING DIRECTORY-----#

#-----LOAD RELEVANT LIBRARIES-----#
library(sp)    
library(rgdal)
library(rgeos)
library(tmap)    
library(spatstat)
library(maptools)
library(classInt)
library(RColorBrewer)
library(dplyr)

#-----LOAD HOUSE PRICE DATA-----#
hp <- read.csv("london-house-prices-pp-2016.txt", header = T, sep = ",", stringsAsFactors = F, check.names = T)

#-----LOAD POSTCODE COORDINATE DATA SETS-----#
codepoint <- read.csv("london-postcode-bng-lookup.txt", header = T, sep = ",", stringsAsFactors = F, check.names = T) 

#-----LOAD LONDON BOROUGH SHAPEFILE-----#
boroughs <- readOGR(dsn=".", layer = "London_Borough_Excluding_MHW")
plot(boroughs)

#-----MAKE ALL POSTCODES UNIFORM BY REMOVING ANY SPACES BETWEEN THEM-----#
library(stringr)
hp$Postcode <- str_replace_all(hp$Postcode, fixed(" "), "")
codepoint$Postcode <- str_replace_all(codepoint$Postcode, fixed(" "), "")

#-----CAPITALISE THE BOROUGH NAMES IN THE SHAPEFILE TO ALLOW SUCCESSFUL JOIN-----#
boroughs$NAME <- toupper(boroughs$NAME) 

#-----JOIN HOUSE PRICES AND POSTCODE COORDINATE DATA SETS-----#
m2 <- left_join(hp, codepoint, by=c("Postcode"="Postcode"))
summary(m2)

#-----RENAME THE 'DISTRICT' COLUMN TO 'NAME' TO MATCH THE SHAPEFILE TO ALLOW SUCCESSFUL JOIN-----#
m2<-rename(m2, NAME = District)

#-----PLOT MAP, FOLLOWED BY PLOTTING ALL POINTS-----#
plot(boroughs)
points(m2$Eastings, m2$Nothings, col = "Black", cex =.3)
#-----NOTE: THE POINTS PLOTTED HERE RELATE TO THE POSTCODES AT WHICH EACH HOUSE HAS BEEN SOLD. 3 SALES IN 1 POSTCODE = 3 POINTS FOR THAT COODINATE OVERLAYING EACHOTHER-----#

#-----CALCULATE MEAN HOUSE PRICE PER BOROUGH-----#
library(data.table)
House_price_mean2 <- setDT(m2)[ , .(mean_Price = mean(Price)), by = NAME]


#-----JOIN MEAN HOUSE PRICE DATA TO SHAPEFILE-----#
Join_trial2 <- merge(boroughs, House_price_mean2)

#-----PLOT HISTOGRAM OF MEAN HOUSE PRICE PER BOROUGH-----#
plot(Join_trial2$mean_Price)

#-----DEFINE BREAKS FOR MAP OF MEAN HOUSE PRICES-----#
breaks <- classIntervals(House_price_mean2$mean_Price, n=6, style = "fisher")
print(breaks)

#-----MEAN HOUSE PRICE MAP-----#
qtm(Join_trial2, "mean_Price")
tm_shape(Join_trial2) +
  tm_fill("mean_Price",title="Mean House Price (£)", palette = "Reds")+
  tm_borders(alpha =.5)+
  tm_layout("Mean House Price", 
            legend.title.size = 0.9,
            legend.text.size = 0.6, 
            legend.position = c("right", "bottom"))


#-----REORDER M2 DATASET FROM MOST EXPENSIVE TO LEAST EXPENSIVE HOUSE SALE PRICE-----#
Reorded <- m2[order(m2$Price, decreasing = TRUE),]

#-----EXTRACT MOST EXPENSIVE 750 SALE PRICES FROM THE DATASET-----#
first750 <- Reorded[1:750,] 

#-----EXTRACT LEAST EXPENSIVE 750 SALE PRICES FROM THE DATASET-----#
last750 <- Reorded[119768:120518,] 

#-----PLOT MAP, FOLLOWED BY MOST EXPENSIVE POINTS, FOLLOWED BY LEAST EXPENSIVE POINTS-----#
plot(boroughs)
points(first750$Eastings, first750$Nothings, col = "RED", cex =.5)
points(last750$Eastings, last750$Nothings, col = "BLUE", cex =.5)
title("Distribution of the Most and Least Expensive 750 Sales")
legend("bottomleft",
       c("Most Expensive Sale Prices", "Least Expensive Sale Prices"),
       lty = c(NA, NA),
       pch = c(1),
       lwd=c(2.5,2.5),col=c("red", "blue"))


#-----JOIN MEDIAN HOUSE PRICE DATA TO SHAPEFILE-----#
House_price_median <- read.csv("prices_paid_median.csv", header = T, sep = ",", stringsAsFactors = F, check.names = T) 
Join_trial3 <- merge(boroughs, House_price_median)

#-----PLOT HISTOGRAM OF MEDIAN HOUSE PRICE PER BOROUGH-----#
plot(Join_trial3$median_Price)

#-----PLOT BASIC MAP-----#
library(tmap)
qtm(Join_trial3, "median_Price")

#-----MEDIAN HOUSE PRICE MAP-----#
qtm(Join_trial3, "median_Price")
tm_shape(Join_trial3) +
  tm_fill("median_Price",title="Median House Price (£)", palette = "Reds")+
  tm_borders(alpha =.5)+
  tm_layout("Median House Price", 
            legend.title.size = 0.9,
            legend.text.size = 0.6, 
            legend.position = c("right", "bottom"))

#-----SPATIAL AUTO-CORRELATION - MEDIAN HOUSE PRICE-----#
#-----LOAD LONDON SHAPEFILE WITH NO WESTMINSTER (CREATED IN ARCGIS)-----#
ldn_no_westminster <- readOGR(dsn=".", layer = "LDN_No_Westminster")
plot(ldn_no_westminster)
plot(Westminster, col = "black", add = T)

#-----CAPITALISE THE BOROUGH NAMES IN THE SHAPEFILE TO ALLOW SUCCESSFUL JOIN-----#
ldn_no_westminster$NAME <- toupper(ldn_no_westminster$NAME) 

#-----JOIN MEDIAN HOUSE PRICE DATA TO SHAPEFILE-----#
Join_trial4 <- merge(ldn_no_westminster, House_price_median)
neighbours <- poly2nb(Join_trial4)

#-----LOAD RELEVANT LIBRARY----#
library(spdep)

#-----CALCUATE NEAREST NEIGHBOUR-----#
plot(Join_trial4, col = "grey")
plot(neighbours, coordinates(Join_trial4), add = TRUE, col='red')

#-----CREATE LISTW AND PERFORM MORAN TEST-----#
listw <- nb2listw(neighbours)
listw
moran.test(Join_trial4$median_Price, listw)

#-----CREATE A LOCAL MORAN OUTPUT-----#
local <- localmoran(x = Join_trial4$median_Price, listw = nb2listw(neighbours, style = "W"))
#-----BIND THE RESULT TO THE MAP----#
local_moran_map <- cbind(Join_trial4, local)
#-----MAP THE RESULT-----#
tm_shape(local_moran_map) + tm_fill(col = "Ii", style = "quantile", title = "Local Moran Statistic")+
  tm_layout("Spatial Autocorrelation of 
            Average House
            Prices",
            legend.title.size=1.2,
            legend.text.size=1,
            legend.position=c("left","bottom"))

#-----RASTER/IDW FOR LONDON-----#
plot(boroughs)
plot(house.pts, add = T)
house.pts <- spTransform(house.pts, CRS(proj4string(boroughs)))
house.pts_clip <- house.pts[boroughs, ] #CLIP SALE POINTS TO BOROUGH SHP
plot(house.pts_clip, add = T)

dat.pp <- as(dirichlet(as.ppp(house.pts_clip)), "SpatialPolygons")
dat.pp <- as(dat.pp, "SpatialPolygons")

proj4string(dat.pp) <- CRS("+init=EPSG:27700")
proj4string(house.pts_clip) <- CRS("+init=EPSG:27700")
int.Z <- over(dat.pp, house.pts_clip, fn=mean)

tm_shape(raster_idw) + tm_raster("prediction", style = "quantile", n = 100, palette = "Reds", legend.show = FALSE) + 
  tm_shape(boroughs) + tm_borders(alpha=.7)

library(gstat)
library(xts)
grid <- spsample(house.pts_clip, type = 'regular', n = 10000)
idw <- idw(house.pts_clip$Price ~1, house.pts_clip, newdata= grid)
idw.output = as.data.frame(idw)
names(idw.output)[1:3] <- c("long", "lat", "Price")
#idw.output <- idw.output[, -4]
View(idw.output)
library(raster)
spg <- idw.output
coordinates(spg) <- ~ long + lat
gridded(spg) <- TRUE
raster_idw <- raster(spg)
projection(raster_idw) <- CRS(bng)
plot(boroughs, add = T)
plot(raster_idw)
projection(boroughs) <- CRS(bng)
masked_idw <- mask(raster_idw, boroughs)
plot(masked_idw)

#-----IDW RASTER MAP-----#
tm_shape(masked_idw) + tm_raster("Price", style = "quantile", n = 10, palette = "Reds", legend.show = TRUE) +
  tm_shape(boroughs) + tm_borders(col = "black", lwd=0.5) +
  tm_layout("House Price
            Estimation(£)", title.size =1.4, legend.position = c("left", "bottom"), legend.text.size = 0.71, legend.title.size = 1.2, frame = FALSE)

#-----CREATE POINT MAP OF ALL PRICE PAID DATA POINTS-----#
#-----PLOT MAP, FOLLOWED BY PLOTTING ALL POINTS-----#
plot(boroughs)
points(m2$Eastings, m2$Nothings, col = "Price", cex =.3)

tm_shape(boroughs) + tm_borders(alpha=.5) +
  tm_shape(house.pts) + tm_dots(col= "Price", breaks = breaks$brks, palette = "Reds", title = "Price Paid (£)") +
  tm_compass() +
  tm_scale_bar(width = 0.15) + 
  tm_layout("Spatial Distribution
            of Price Paid Data
            2016", title.size =1.2, legend.position = c("left", "bottom"), legend.text.size = 0.8, legend.title.size = 01.2, frame = FALSE)

