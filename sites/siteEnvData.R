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

hiGeol <- readOGR('../env_data/geol/hawaii_state_geol_ageClean.shp', 'hawaii_state_geol_ageClean')
plotsGeol <- over(plots, spTransform(hiGeol, CRS(proj4string(plots))))

plots$island <- as.character(plotsGeol$ISLAND)

plots$volcano <- as.character(plotsGeol$VOLCANO)
plots$volcano[plots$volcano =='mloa'] <- 'Mauna Loa'
plots$volcano[plots$volcano =='kila'] <- 'Kilauea'
plots$volcano[plots$volcano =='mkea'] <- 'Mauna Kea'
plots$volcano[plots$volcano =='emol'] <- 'East Molokai'
plots$volcano[plots$volcano =='koha'] <- 'Kohala'
plots$volcano[plots$volcano =='kaua'] <- 'Kauai'
plots$volcano[plots$volcano =='hale'] <- 'Haleakala'

plots$rockType <- plotsGeol$ROCK_TYPE

plots$ageMin_mya <- plotsGeol$age_min
plots$ageMax_mya <- plotsGeol$age_max
plots$ageMid_mya <- plotsGeol$age_mid

head(plots)

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

## ======
## precip
## ======



## =========================================
## write out shape files and csv of env data
## =========================================

writeOGR(plots, 'dimensions_plots.shp', layer='dimensions_plots', driver='ESRI Shapefile', overwrite_layer=TRUE)
plotsTab <- read.csv('dimensions_plots.csv')
plotsTab$age <- plots@data$age[match(plotsTab$plot_name, plots@data$name)]
write.csv(plotsTab, 'dimensions_plots.csv', row.names = FALSE)
