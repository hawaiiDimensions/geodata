library(sp)
library(rgdal)
setwd('~/Dropbox/hawaiiDimensions/geodata/sites')

## function to take filename `f' and read-in GPX data, standarizing it and outputting it
compileGPX <- function(f) {
    ## read data and remove unneccesary fields
    x <- readOGR(f, 'waypoints')
    x@data <- x@data[, c('name', 'time')]
    names(x@data) <- c('name', 'dateCreated')
    
    ## re-project if needed
    if(proj4string(x) != '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0') {
        spTransform(x, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
    }
    
    ## standarize site names
    site.name <- gsub('\\..*|.*/', '', f)
    plots <- gsub('[[:alpha:]]|.*[[:space:]]|.*_|.*-', '', x@data$name)
    plots <- paste(sapply(max(2, nchar(plots)) - nchar(plots), function(n) paste(rep('0', n), collapse='')), plots, sep='')
    if(nchar(plots[1]) > 2 & all(substring(plots, 1, 1) == '0')) plots <- substring(plots, 2, 100)
    x@data$name <- paste(site.name, plots, sep='_')
    x <- x[order(x@data$name), ]
    
    return(x)
}

## implement function on all GPX files, binding output into one SpatialPointsDataFrame
out <- lapply(paste('waypoints', list.files('waypoints', pattern='\\.GPX'), sep='/'), compileGPX)
out <- do.call(rbind, out)

## remove spurious kaiholena old plot
out <- out[-grep('kaiholenaOld_71', out$name), ]

## make data be plot name only
out@data <- data.frame(name=out$name)

## write combined data to multiple formats
if('dimensions_plots.gpx' %in% list.files()) system(sprintf('rm %s/dimensions_plots.gpx', getwd()))
writeOGR(out, 'dimensions_plots.gpx', layer='waypoints', driver='GPX')
writeOGR(out, 'dimensions_plots.shp', layer='dimensions_plots', driver='ESRI Shapefile', overwrite_layer=TRUE)
writeOGR(out, 'dimensions_plots.kml', layer='dimensions_plots', driver='KML', overwrite_layer=TRUE)
