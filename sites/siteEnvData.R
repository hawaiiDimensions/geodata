setwd('~/Dropbox/hawaiiDimensions/geodata/sites')

library(sp)
library(rgdal)
library(raster)

## ==========
## load plots
## ==========
plots <- readOGR('.', 'dimensions_plots')

## ===========================
## add data to plots sp object
## ===========================

## ==================
## age and other geol
## ==================

hi.geo.poly <- readOGR('../env_data/geol/Haw_St_geo_20070426_region', 
                       'Haw_St_geo_20070426_region')
hi.geo.poly <- spTransform(hi.geo.poly, CRS(proj4string(plots)))
site.ages <- as.character(over(plots, hi.geo.poly)$AGE_RANGE)
site.ages[grep('A.D.', site.ages)] <- sapply(strsplit(gsub('A.D. ', '', site.ages[grep('A.D.', site.ages)]), '-'), 
                                             function(x) 2016 - mean(as.numeric(x)))
site.ages[grep('yr', site.ages)] <- sapply(strsplit(gsub(',| yr', '', site.ages[grep('yr', site.ages)]), '-'), 
                                           function(x) mean(as.numeric(x)))
site.ages[grep('Ma', site.ages)] <- sapply(strsplit(gsub('Ma', '', site.ages[grep('Ma', site.ages)]), '-'), 
                                           function(x) mean(as.numeric(x)) * 10^6)
site.ages[grep('About', site.ages)] <- sapply(strsplit(gsub('About ', '', site.ages[grep('About', site.ages)]), ' to '), 
                                              function(x) mean(as.numeric(x)) * 10^6)
site.ages <- as.numeric(site.ages)

plots@data$age <- site.ages

## =========
## elevation
## =========

## helper function to read in raster DEM for island and match 
## plots on that island, extracting their elevation
elev <- function(island, plots) {
    info <- readLines(sprintf('../env_data/%sDEM/README.%s', island, island), n = 1)
    info <- strsplit(info, ', ')[[1]]
    
    thisCRS <- sprintf('+proj=utm +zone=%s +datum=%s', gsub('UTM zone ', '', info[2]), info[3])
    
    r <- raster(sprintf('../env_data/%sDEM/%s.bil', island, island), crs = CRS(thisCRS))
    
    plots <- spTransform(plots, CRS(proj4string(r)))
    
    e <- extract(r, plots)
    e <- round(e*ifelse(max(e) > 2000, 0.3048, 1))
    
    return(data.frame(plot_name = plots$name, elevation_m = e))
}

allElev <- rbind(elev('maui', plots[grepl('waikamoi', plots$name), ]), 
                 elev('molokai', plots[grepl('kamakou', plots$name), ]), 
                 elev('kauai', plots[grepl('kokee', plots$name), ]), 
                 read.csv('../env_data/plot_elevation.csv', as.is = TRUE))

plots$elevation_m <- allElev$elevation_m[match(plots$name, allElev$plot_name)]

## =========================================
## write out shape files and csv of env data
## =========================================

writeOGR(plots, 'dimensions_plots.shp', layer='dimensions_plots', driver='ESRI Shapefile', overwrite_layer=TRUE)
plotsTab <- read.csv('dimensions_plots.csv')
plotsTab$age <- plots@data$age[match(plotsTab$plot_name, plots@data$name)]
write.csv(plotsTab, 'dimensions_plots.csv', row.names = FALSE)
