library(raster)
library(sp)
library(rgdal)
library(maptools)

setwd('~/Dropbox/hawaiiDimensions/site_selection/Haw_St_shapefiles/Haw_St_geo_20070426_region')
hi.geo.poly <- readOGR('.', 'Haw_St_geo_20070426_region')
mo.geo <- hi.geo.poly[hi.geo.poly$ISLAND=='Molokai', ]

setwd('~/Dropbox/hawaiiDimensions/site_selection/molokai')
pnts1.sp <- readOGR('kamakou_points_final.kml', 'kamakou_points_final.kml')

# temp <- spTransform(pnts1.sp, CRS('+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0'))
# write.table(data.frame(temp$Name, coordinates(temp)), 'temp.csv', sep=',', row.names=FALSE)

plot(spTransform(pnts1.sp, CRS(proj4string(ma.geo))))
text(coordinates(spTransform(pnts1.sp, CRS(proj4string(ma.geo)))), 
     labels=gsub('.*_', '', pnts1.sp@data[, 1]))
plot(mo.geo, add=TRUE)
