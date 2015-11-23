## load needed libraries
library(sp)
library(rgdal)

## direct R to the correct place to look for polygons
setwd('~/Dropbox/hawaiidimensions/site_selection/kauai/LSAGWaialaePolygons')

## get a list of all the shape files that need to be loaded
poly.files <- list.files('./', pattern='shp')

## load all of the shape files
polys <- lapply(1:length(poly.files), function(i) {
	x <- readOGR('./', gsub('\\.shp', '', poly.files[i]))
	spChFIDs(x, paste(i, rownames(as(x, 'data.frame')), sep=''))
})

## combind the polygons into one spatial object
onepoly <- do.call(rbind, polys)

## reproject to geographic coordinates (WGS84)...kml is geographic only
onepoly <- spTransform(onepoly, CRS('+proj=longlat +datum=WGS84 +ellps=WGS84'))

## save that object to the computer (the exact location is specified above in the `setwd' command)
writeOGR(onepoly, 'kauai_poly.kml', layer='kauai_poly', driver='KML')