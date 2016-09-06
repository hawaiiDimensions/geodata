setwd('~/Dropbox/hawaiiDimensions/geodata/sites')

library(sp)
library(rgdal)

## load plots
plots <- readOGR('.', 'dimensions_plots')

## add data to plots sp object

## age
hi.geo.poly <- readOGR('../site_selection/Haw_St_shapefiles/Haw_St_geo_20070426_region', 
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


## write out shape files and csv of env data
writeOGR(plots, 'dimensions_plots.shp', layer='dimensions_plots', driver='ESRI Shapefile', overwrite_layer=TRUE)
plotsTab <- read.csv('dimensions_plots.csv')
plotsTab$age <- plots@data$age[match(plotsTab$plot_name, plots@data$name)]
write.csv(plotsTab, 'dimensions_plots.csv', row.names = FALSE)
