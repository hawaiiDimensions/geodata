## ====================================================================
## script to add environmental data to spatial data on Dimensions plots
## then write it back out to csv
## ====================================================================

library(sp)
library(rgdal)
library(raster)

setwd('~/Dropbox/hawaiiDimensions/geodata/sites')

## load plots
plots <- readOGR('.', 'dimensions_plots')

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

## directories with precip data
precipDirs <- list.dirs('../env_data/precip/StateRFGrids_mm2')
precipDirs <- precipDirs[grep('staterf_mm', precipDirs)]

## extract precip values
precip <- lapply(precipDirs, function(d) {
    r <- raster(paste(d, 'w001001.adf', sep = '/'))
    extract(r, spTransform(plots, CRS(proj4string(r))))
})

## add to plots 
precip <- do.call(data.frame, precip)
names(precip) <- paste('AvgPrecip', c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                                      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Den', 'Ann'), 
                       '_mm', sep = '')
plots@data <- cbind(plots@data, precip)


## =========================
## write out csv of env data
## =========================

plotsDat <- plots@data
xy <- as.data.frame(coordinates(plots))
names(xy) <- c('lon', 'lat')
plotsDat <- cbind(name = plotsDat[, 1], xy, projection = proj4string(plots), plotsDat[, -1])[1:3, ]

write.csv(plotsDat, 'dimensions_plots_envData.csv', row.names = FALSE)

## =============
## plot env data
## =============

library(RColorBrewer)
library(socorro)

ageCol <- function(a) {
    rgb(colorRamp(c(brewer.pal(9, 'YlOrRd'), 
                          hsv(1, 0.5, 0.3)))(log(a*10^6)/log(max(plots$ageMax_mya*10^6))),
              maxColorValue = 255)
}

pdf('fig_elevPrecipAge.pdf', width = 5.5, height = 4)

layout(matrix(1:2, nrow = 1), widths = c(4, 1.5))

par(mar = c(3, 3, 0, 0) + 0.5, mgp = c(2.25, 0.75, 0))
plot(plots$elevation_m, plots$AvgPrecipAnn_mm, 
     bg = ageCol(plots$ageMid_mya), pch = 21,
     xlab = 'Elevation (m)', ylab = 'Mean Annual Precipitation (mm)', 
     cex.lab = 1.2, cex = 1.2)

par(mar = c(4, 0.5, 1, 4) + 0.5)
plot(range(plots$ageMin_mya, plots$ageMax_mya)*10^6, xaxs = 'i', yaxs = 'i', log = 'y',
     axes = FALSE, type = 'n', xlab = '', ylab = '')
rect(xleft = par('usr')[1], xright = par('usr')[2], 
     ybottom = 10^seq(par('usr')[3], par('usr')[4], length = 50)[-50], 
     ytop = 10^seq(par('usr')[3], par('usr')[4], length = 50)[-1],
     col = ageCol(10^(seq(par('usr')[3], par('usr')[4], length = 49) - 6)),
     border = NA)
box()
logAxis(4)
mtext('Substrate age (years)', side = 4, line = 2.25, cex = 1.2)

dev.off()
