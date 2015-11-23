library(sp)
library(maptools)
library(rgdal)
library(maps)
library(mapdata)
library(RColorBrewer)

## load flow layers
setwd('~/Dropbox/hawaiiDimensions/geoData/site_selection/Haw_St_shapefiles/Haw_St_geo_20070426_region')

hi.geo.poly <- readOGR('.', 'Haw_St_geo_20070426_region')

ka.geo <- hi.geo.poly[hi.geo.poly$ISLAND=='Kauai', ]
ma.geo <- hi.geo.poly[hi.geo.poly$ISLAND=='Maui', ]
hi.geo <- hi.geo.poly[hi.geo.poly$ISLAND=='Hawaii', ]

## colors for flow ages
geo.col <- colorRampPalette(brewer.pal(9,"YlGnBu"))(max(hi.geo.poly@data$AGE_GROUP) + 1)

jpeg(filename='../../../maps/map_hawaii_flowAge.jpg', bg='black')
par(mar=rep(0, 4), bg='black')
plot(hi.geo, col=geo.col[hi.geo$AGE_GROUP], border=geo.col[hi.geo$AGE_GROUP])
dev.off()
