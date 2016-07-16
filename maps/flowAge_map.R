library(sp)
library(maptools)
library(rgdal)
library(maps)
library(mapdata)
library(RColorBrewer)

## load flow layers
setwd('~/Dropbox/hawaiiDimensions/geoData/site_selection/Haw_St_shapefiles/Haw_St_geo_20070426_region')

hi.geo.poly <- readOGR('.', 'Haw_St_geo_20070426_region')
hi.geo <- hi.geo.poly[hi.geo.poly$ISLAND=='Hawaii', ]

## get island outline
island <- gUnionCascaded(hi.geo)
island <- SpatialPolygons(list(
    Polygons(island@polygons[[1]]@Polygons[sapply(island@polygons[[1]]@Polygons, 
                                                   function(p) !p@hole & p@area > 1e+07)
                                            ], ID=1)), 
    proj4string = CRS(proj4string(hi.geo)))

## colors for flow ages
geo.col <- colorRampPalette(brewer.pal(9,"YlGnBu"))(max(hi.geo.poly@data$AGE_GROUP) + 1)

jpeg(filename='../../../maps/map_hawaii_flowAge.jpg', width=4, height=4, units='in', res=400)
par(mar=rep(0, 4))
plot(hi.geo, col=geo.col[hi.geo$AGE_GROUP], border=geo.col[hi.geo$AGE_GROUP])
plot(island, add=TRUE)
dev.off()
