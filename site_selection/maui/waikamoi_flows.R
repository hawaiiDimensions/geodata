library(raster)

setwd('~/Dropbox/hawaiiDimensions/site_selection/Haw_St_shapefiles/Haw_St_geo_20070426_region')
hi.geo.poly <- readOGR('.', 'Haw_St_geo_20070426_region')
ma.geo <- hi.geo.poly[hi.geo.poly$ISLAND=='Maui', ]

setwd('~/Dropbox/hawaiidimensions/site_selection/maui')
pnts1.sp <- readOGR('waikamoi_points_final.kml', 'waikamoi_points_final')

plot(spTransform(pnts1.sp, CRS(proj4string(ma.geo))))
text(coordinates(spTransform(pnts1.sp, CRS(proj4string(ma.geo)))), 
     labels=gsub('.*_', '', pnts1.sp@data[, 1]))
plot(ma.geo, add=TRUE)

axis(2)
abline(v=c(784000, 789000))
abline(h=c(2300000, 2305000)

setwd('~/Dropbox/hawaiiDimensions/site_selection/maui')
bla <- readOGR('waikamoi_18A.kml', 'waikamoi_18A')
plot(spTransform(bla, CRS(proj4string(ma.geo))), add=TRUE)