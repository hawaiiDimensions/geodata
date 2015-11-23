library(sp)
library(rgdal)
setwd('~/Dropbox/hdim/routes')

#############
## still need to include date and time. date of file is in the `name' element of data when read in
## but date and time of actual point not read in, use layer='track_points'
#############

## from `trackDists.R' we know that a cuttoff of 10^-5 radians is about right
minrad <- 10^-5

## function to calculate approx great circle dist
gcd <- function(p1, p2) {
	2*asin(sqrt(4*sin(pi*(p1[1] - p2[1])/360)^2 + 4*sin(pi*(p1[2] - p2[2])/360)^2)/2)
}


cleanTracks <- function(f) {
	x <- readOGR(f, layer='tracks')
	
	## re-project if needed
    if(proj4string(x) != '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0') {
        spTransform(x, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
    }
    
	out <- lapply(coordinates(x), function(p) {
		p <- p[[1]]
		D <- numeric(nrow(p))
		for(i in 2:nrow(p)) {
			D[i] <- gcd(p[i-1, ], p[i, ])
		}
		
		## split p into chunks 
		if(any(D > minrad)) {
			p <- split(as.data.frame(p), as.factor(cumsum(D > minrad)), drop=TRUE)
		} else {
			p <- list(p)
		}
		
		## list of Lines objects
		l <- lapply(p, function(lp) {
			Lines(list(Line(lp)), ID=as.character(runif(1)))
		})
		
		return(l)
	})
	
	out <- do.call(c, out)
	# sl <- SpatialLines(out, proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
	# sldf <- SpatialLinesDataFrame(sl, data=data.frame(name=as.character(1:length(sl))), match.ID=FALSE)
	
	return(out)
}

out <- lapply(paste('gpx', list.files('gpx', pattern='\\.GPX'), sep='/'), cleanTracks)
out <- do.call(c, out)
sl <- SpatialLines(out, proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
sldf <- SpatialLinesDataFrame(sl, data=data.frame(name=as.character(1:length(sl))), match.ID=FALSE)
sldf <- spChFIDs(sldf, as.character(sldf@data$name))

writeOGR(sldf, 'semi_clean_tracks.gpx', layer='tracks', driver='GPX', overwrite_layer=TRUE)