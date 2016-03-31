library(sp)
library(spatstat)
library(rgdal)
library(maptools)
library(XML)

setwd('~/Dropbox/hawaiidimensions/geodata/kipuka')

x <- readOGR('Kipuka_NSFSelection.kml', 'Kipuka_NSFSelection')
plot(x)

kip <- data.frame(name=paste('kipuka', x@data$Name, sep='_'),
                  rats=gsub('</td>.*', '', gsub('.*<td>RatRemoval</td> <td>', '', x@data[, 2])),
                  area=as.numeric(gsub('</td>.*', '', gsub('.*<td>Area_ha</td> <td>', '', x@data[, 2]))))
x@data <- kip
x.gpx <- x
x.gpx@data <- kip[, 1, drop=FALSE]
