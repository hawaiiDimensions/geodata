library(sp)
library(rgdal)
setwd('~/Dropbox/hdim/routes')

gcd <- function(p1, p2) {
    2*asin(sqrt(4*sin(pi*(p1[1] - p2[1])/360)^2 + 4*sin(pi*(p1[2] - p2[2])/360)^2)/2)
}

trackDists <- function(f) {
    x <- readOGR(f, layer='tracks')
    
    ## re-project if needed
    if(proj4string(x) != '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0') {
        spTransform(x, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
    }
    
    rapply(coordinates(x), function(p) {
        D <- numeric(nrow(p))
        for(i in 2:nrow(p)) {
            # D[i] <- sqrt((p[i-1, 1] - p[i, 1])^2 + (p[i-1, 2] - p[i, 2])^2)
            D[i] <- gcd(p[i-1, ], p[i, ])
        }
        return(D)
    })
}

x <- lapply(paste('gpx', list.files('gpx', pattern='\\.GPX'), sep='/'), trackDists)
x <- unlist(x)
hist(log(x[x>10^-6 & x<10^-3], base=10), breaks=500)
foo <- density(log(x[x>10^-6 & x<10^-3], base=10))
plot(foo, log='y')
abline(v=-4)

plot(density(log(x[x < 10^-4 & x > 10^-8], base=10)), log='y')
