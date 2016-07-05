library(sp)
library(maptools)
library(rgdal)
library(maps)
library(mapdata)
library(RColorBrewer)

## load flow layers
setwd('~/Dropbox/hawaiiDimensions/geoData/site_selection/Haw_St_shapefiles/Haw_St_geo_20070426_region')
hi.geo.poly <- readOGR('.', 'Haw_St_geo_20070426_region')
hi.geo.poly <- spTransform(hi.geo.poly, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

## change wd for plotting
setwd('~/Dropbox/hawaiiDimensions/geoData/maps')

## colors for flow ages
geo.col <- colorRampPalette(brewer.pal(9,"YlGnBu"))(max(hi.geo.poly@data$AGE_GROUP) + 1)

jpeg(filename='map_sites.jpg', width=4800*2, height=4800, type='quartz')
par(mar=rep(0, 4))
plot(hi.geo.poly, col=geo.col[hi.geo.poly$AGE_GROUP], border=geo.col[hi.geo.poly$AGE_GROUP])
dev.off()
