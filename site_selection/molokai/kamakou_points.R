library(sp)
library(spatstat)
library(rgdal)

setwd('~/Dropbox/hawaiidimensions/site_selection/molokai')

x <- readOGR('kamakou_poly1.kml', 'kamakou_poly1')
plot(x)
x.poly <- locator(20)

n <- 40
pnts1 <- runifpoint(n, win=owin(poly=x.poly))
pnts1.sp <- SpatialPointsDataFrame(coords(pnts1), 
                                   data=data.frame(name=paste('temp_kamakou', 1:n, sep='_')), 
                                   proj4string=CRS(proj4string(x)))
pnts1.sp@data$name <- as.character(pnts1.sp@data$name)


alldist <- spDists(pnts1.sp, longlat=TRUE)

more <- TRUE
i <- 1
while(more) {
	print(i)
	useThese <- sample(n, 4)
	theseDist <- alldist[useThese, useThese]
	more <- !all(theseDist[lower.tri(theseDist)] >= 0.2)
	i <- i + 1
	if(i > 100) {
		more <- FALSE
		useThese <- NA
	}
}

plot(x)
points(pnts1.sp)
points(pnts1.sp[useThese, ], pch=16)

pnts1.sp@data$name[useThese] <- gsub('temp_', '', pnts1.sp@data$name[useThese])

writeOGR(pnts1.sp, 'kamakou_pnts1.gpx', layer='waypoints', driver='GPX')


####### redo with bigger polygon

x <- readOGR('kamakou_poly2.kml', 'kamakou_poly2')
plot(x)
x.poly <- locator(22)
points(x.poly)

n <- 40
pnts2 <- runifpoint(n, win=owin(poly=x.poly))
pnts2.sp <- SpatialPointsDataFrame(coords(pnts1), 
                                   data=data.frame(name=paste('temp_kamakou', 1:n, sep='_')), 
                                   proj4string=CRS(proj4string(x)))
pnts2.sp@data$name <- as.character(pnts2.sp@data$name)

alldist <- spDists(pnts2.sp, longlat=TRUE)

more <- TRUE
i <- 1
while(more) {
	print(i)
	useThese <- sample(n, 5)
	theseDist <- alldist[useThese, useThese]
	more <- !all(theseDist[lower.tri(theseDist)] >= 0.2)
	i <- i + 1
	if(i > 500) {
		more <- FALSE
		useThese <- NA
	}
}

plot(x)
points(pnts2.sp)
points(pnts2.sp[useThese, ], pch=16)

pnts2.sp@data$name[useThese] <- gsub('temp_', '', pnts2.sp@data$name[useThese])

writeOGR(pnts2.sp, 'kamakou_pnts2.gpx', layer='waypoints', driver='GPX')



