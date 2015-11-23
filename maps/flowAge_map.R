library(sp)
library(maptools)
library(rgdal)
library(maps)
library(mapdata)

setwd('~/Dropbox/hawaiiDimensions/geb_paper')
source('~/R_functions/logAxis.R')

## helper funciton for making time colors
tcol <- function(n) {
	hsv(0.17 + 0.45 / (1 + exp(-5 * seq(-1, 1, length=n))))
}

## helper function for making island age plots
islandAgePlot <- function(x, ...) {
	# x <- x[1:10, ]
	plot(x, col=geo.col[x$AGE_GROUP], border=geo.col[x$AGE_GROUP], ...)
	points(samp.site, pch=21, col='white', bg='gray', lwd=1.5, cex=1)
	points(focal.site, pch=21, col='white', bg='black', lwd=2, cex=2)
}

## load data on sampling locations
site.info <- read.csv('network/data/siteInfo.csv', as.is=TRUE)
spec.info <- read.csv('genetics/popSiteNamesAges.csv', as.is=TRUE)

## load flow layers
oldwd <- setwd('~/Dropbox/hawaiiDimensions/site_selection/Haw_St_shapefiles/Haw_St_geo_20070426_region')
hi.geo.poly <- readOGR('.', 'Haw_St_geo_20070426_region')
setwd(oldwd)

ka.geo <- hi.geo.poly[hi.geo.poly$ISLAND=='Kauai', ]
ma.geo <- hi.geo.poly[hi.geo.poly$ISLAND=='Maui', ]
hi.geo <- hi.geo.poly[hi.geo.poly$ISLAND=='Hawaii', ]

## colors for flow ages
library(RColorBrewer)
geo.col <- colorRampPalette(brewer.pal(9,"YlGnBu"))(max(hi.geo.poly@data$AGE_GROUP) + 1)
# geo.col <- c(tcol(max(hi.geo.poly@data$AGE_GROUP))[1], tcol(max(hi.geo.poly@data$AGE_GROUP)))

## re-project sampling locations
focal.site <- SpatialPoints(site.info[!is.na(site.info$lat), c('lon','lat')],
                            proj4string=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84'))
focal.site <- spTransform(focal.site, CRS(proj4string(hi.geo.poly)))
samp.site <- SpatialPoints(spec.info[!is.na(spec.info$Lat), c('Long','Lat')],
                           proj4string=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84'))
samp.site <- spTransform(samp.site, CRS(proj4string(hi.geo.poly)))

## plot
this.par <- list(mar=rep(0, 4))

pdf(width=8, height=3, file='fig_mapAge.pdf')
split.screen(c(1, 3))

screen(1)
par(mar=rep(2.75, 4))
islandAgePlot(ka.geo)

screen(2)
par(mar=rep(1.25, 4))
islandAgePlot(ma.geo)

screen(3)
par(mar=rep(0.5, 4))
islandAgePlot(hi.geo)

close.screen(all=TRUE)
dev.off()

pdf(width=5.5, height=3.5, file='fig_mapOutline.pdf')
map('worldHires', xlim=c(-160, -154), ylim=c(18, 23), 
    resolution=0, mar=rep(0, 4), col='white', fill=TRUE)
dev.off()

## make legend
chrono.age <- read.csv("~/Dropbox/hawaiiDimensions/site_selection/Haw_St_shapefiles/Haw_St_ageCode.csv",stringsAsFactors=FALSE)
chrono.age$age.low <- chrono.age$age.low*c(yr=10^-6,ka=10^-3,Ma=10^0)[chrono.age$unit]
chrono.age$age.hi <- chrono.age$age.hi*c(yr=10^-6,ka=10^-3,Ma=10^0)[chrono.age$unit]
chrono.age <- chrono.age[,-4]

chrono.age <- rbind(chrono.age, cbind(code=c(13:14), age.low=c(2, 4), age.hi=c(4, 6)))

pdf(width=4.5, height=1.5, file='fig_mapLegend.pdf')
par(mar=c(2, 1, 0, 1) + 1, mgp=c(2,0.5,0), xpd=NA)
plot(1,xlim=range(chrono.age[,-1])+0.0001,ylim=0:1,log="x",type="n",
	 axes=FALSE,xlab="Substrate age (My)",ylab="",xaxs="i",yaxs="i")

apply(chrono.age,1,function(x) rect(x[2]+0.0001,0,x[3]+0.0001,1,col=geo.col[x[1]+1],border=NA))
box()
logAxis(1, labels=FALSE)
axis(side=1, at=c(10^(-3:0), 6))

# old.scipen <- options(scipen=999)
# axis(side=1, at=10^(-4:0), labels=1000000*10^(-4:0))
# options(scipen=old.scipen)
dev.off()

