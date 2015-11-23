library(sp)
library(spatstat)
library(rgdal)
library(maptools)

setwd('~/Dropbox/hawaiidimensions/site_selection/maui')

x <- readOGR('waikamoi_poly1.kml', 'waikamoi_poly1')

plot(x)
x.owin <- as(as(x, 'SpatialPolygons'), 'owin')


n <- 40
pnts1 <- runifpoint(n, win=x.owin)
pnts1.sp <- SpatialPointsDataFrame(coords(pnts1), 
                                   data=data.frame(name=paste('temp_waikamoi', 1:n, sep='_')), 
                                   proj4string=CRS(proj4string(x)))
pnts1.sp@data$name <- as.character(pnts1.sp@data$name)

alldist <- spDists(pnts1.sp, longlat=TRUE)

more <- TRUE
i <- 1
while(more) {
	print(i)
	useThese <- sample(n, 6)
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

writeOGR(pnts1.sp, 'waikamoi_pnts1.gpx', layer='waypoints', driver='GPX')


list.files()
