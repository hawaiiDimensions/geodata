library(sp)
library(spatstat)
library(rgdal)
library(rgeos)
library(maptools)

setwd('~/Dropbox/hawaiidimensions/geodata/kipuka')

x <- readOGR('Kipuka_NSFSelection.kml', 'Kipuka_NSFSelection')
x <- spTransform(x, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

kip <- data.frame(name=paste('kipuka', x@data$Name, sep='_'),
                  rats=gsub('</td>.*', '', gsub('.*<td>RatRemoval</td> <td>', '', x@data[, 2])),
                  area=as.numeric(gsub('</td>.*', '', gsub('.*<td>Area_ha</td> <td>', '', x@data[, 2]))))
x@data <- kip

x.gpx <- SpatialPointsDataFrame(gCentroid(x, byid=TRUE), data=kip[, 1, drop=FALSE],
                                proj4string=CRS(prog4string(x)))

writeOGR(x.gpx, 'kipuka.gpx', layer='waypoints', driver='GPX', overwrite_layer=TRUE)
