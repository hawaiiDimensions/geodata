library(sp)
library(maptools)
library(rgdal)
library(RColorBrewer)
library(rgeos)

## needed functions
source('~/R_functions/logAxis.R')

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
geo.col <- c(NA, colorRampPalette(brewer.pal(9,"YlGnBu"))(max(hi.geo.poly@data$AGE_GROUP)))

## actual ages of plots
site.ages <- as.character(over(sites, hi.geo.poly)$AGE_RANGE)
site.ages[grep('A.D.', site.ages)] <- sapply(strsplit(gsub('A.D. ', '', site.ages[grep('A.D.', site.ages)]), '-'), 
                                             function(x) 2016 - mean(as.numeric(x)))
site.ages[grep('yr', site.ages)] <- sapply(strsplit(gsub(',| yr', '', site.ages[grep('yr', site.ages)]), '-'), 
                                           function(x) mean(as.numeric(x)))
site.ages[grep('Ma', site.ages)] <- sapply(strsplit(gsub('Ma', '', site.ages[grep('Ma', site.ages)]), '-'), 
                                           function(x) mean(as.numeric(x)) * 10^6)
site.ages[grep('About', site.ages)] <- sapply(strsplit(gsub('About ', '', site.ages[grep('About', site.ages)]), ' to '), 
                                           function(x) mean(as.numeric(x)) * 10^6)
site.ages <- as.numeric(site.ages) / 10^6

## plotting

jpeg(filename='map_sites.jpg', width=10, height=5, units='in', res=400)
close.screen(all.screens = TRUE)
par(mar=rep(0, 4))
plot(hi.geo.poly, col=geo.col[hi.geo.poly$AGE_GROUP+1], border=geo.col[hi.geo.poly$AGE_GROUP+1])
plot(islands, add=TRUE)
plot(sites, add=TRUE, pch=21, col='white', bg='black', cex=1.5)

split.screen(figs=matrix(c(0, 0.5, 0.05, 0.35), nrow=1), erase=FALSE)
par(mar=c(2, 1, 0, 1) + 1, mgp=c(2,0.5,0), xpd=NA)
plot(1, xlim=range(chrono.age[, -1]) + 0.0001, ylim=c(0, 1.5), log='x',type='n',
     axes=FALSE, xlab='Substrate age (My)', ylab='', xaxs='i', yaxs='i')

apply(chrono.age, 1, function(x) rect(x[2] + 0.0001, 0, x[3] + 0.0001, 0.99, col=geo.col[x[1] + 1], border=NA))
rect(min(chrono.age$age.low) + 0.0001, 0, max(chrono.age$age.hi) + 0.0001, 0.99)

arrows(x0=site.ages + 0.0001, y0=1.5, y1=1, length=0.1)

logAxis(1, labels=FALSE)
axis(side=1, at=c(10^(-3:0), 6))

dev.off()
close.screen(all.screens = TRUE)
