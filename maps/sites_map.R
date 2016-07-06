library(sp)
library(maptools)
library(rgdal)
library(maps)
library(mapdata)
library(mapproj)
library(RColorBrewer)
library(rgeos)

## load flow layers
setwd('~/Dropbox/hawaiiDimensions/geoData/site_selection/Haw_St_shapefiles/Haw_St_geo_20070426_region')
hi.geo.poly <- readOGR('.', 'Haw_St_geo_20070426_region')

## get island outlines
islands <- gUnionCascaded(hi.geo.poly)
islands <- SpatialPolygons(list(
    Polygons(islands@polygons[[1]]@Polygons[sapply(islands@polygons[[1]]@Polygons, 
                                                   function(p) !p@hole & p@area > 1e+07)
                                            ], ID=1)), 
    proj4string = CRS(proj4string(hi.geo.poly)))

## load sites
setwd('~/Dropbox/hawaiiDimensions/geoData/sites')
plots <- readOGR('.', 'dimensions_plots')
plots <- spTransform(plots, CRS(proj4string(hi.geo.poly)))
sites <- lapply(split(plots, gsub('_.*', '', plots$name)), gCentroid)
sites <- do.call(rbind, sites)

## change wd for plotting
setwd('~/Dropbox/hawaiiDimensions/geoData/maps')

## colors for flow ages
geo.col <- colorRampPalette(brewer.pal(9,"YlGnBu"))(max(hi.geo.poly@data$AGE_GROUP) + 1)

png(filename='map_sites.png', width=10, height=5, units='in', res=4000)
par(mar=rep(0, 4))
plot(hi.geo.poly, col=geo.col[hi.geo.poly$AGE_GROUP], border=geo.col[hi.geo.poly$AGE_GROUP])
plot(islands, add=TRUE)
plot(sites, add=TRUE, pch=21, col='white', bg='black', cex=2)
dev.off()
