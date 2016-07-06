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

## load age info
chrono.age <- read.csv('../Haw_St_ageCode.csv',stringsAsFactors=FALSE)
chrono.age$age.low <- chrono.age$age.low*c(yr=10^-6,ka=10^-3,Ma=10^0)[chrono.age$unit]
chrono.age$age.hi <- chrono.age$age.hi*c(yr=10^-6,ka=10^-3,Ma=10^0)[chrono.age$unit]
chrono.age <- chrono.age[,-4]
chrono.age <- rbind(chrono.age, cbind(code=c(13:14), age.low=c(2, 4), age.hi=c(4, 6)))
chrono.age$age.mid <- (chrono.age$age.low + chrono.age$age.hi) / 2

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
geo.col <- c('gray', colorRampPalette(brewer.pal(9,"YlGnBu"))(max(hi.geo.poly@data$AGE_GROUP)))

## plotting
png(filename='map_sites.png', width=10, height=5, units='in', res=4000)

pdf('map_sites.pdf', width=10, height=5)
par(mar=rep(0, 4))
plot(hi.geo.poly, col=geo.col[hi.geo.poly$AGE_GROUP+1], border=geo.col[hi.geo.poly$AGE_GROUP+1])
plot(islands, add=TRUE)
plot(sites, add=TRUE, pch=21, col='white', bg='black', cex=2)
dev.off()



