## ============================================================
## script to read in geological info polygons and clean them up
## ============================================================

library(sp)
library(rgdal)

setwd('~/Dropbox/hawaiiDimensions/geodata/env_data/geol')

## read geol data
geoPoly <- readOGR('Haw_St_geo_20070426_region', 'Haw_St_geo_20070426_region')

## time units for age
age.unit <- rep('mya', length(hi.geo.poly$AGE_RANGE))
age.unit[grep('A.D.', hi.geo.poly$AGE_RANGE)] <- 'yr'
age.unit[grep('yr', hi.geo.poly$AGE_RANGE)] <- 'ya'

## age values
age.val <- as.character(hi.geo.poly$AGE_RANGE)
age.val <- gsub(' to ', '-', age.val)
age.val <- gsub('A.D.|[[:alpha:]]| |,|/|>', '', age.val)
age.val[age.val == ''] <- NA
age.val <- sapply(strsplit(age.val, '-'), function(x) c(range(as.numeric(x)), mean(as.numeric(x))))
age.val[, age.unit == 'ya'] <- age.val[, age.unit == 'ya'] / 10^6
age.val[, age.unit == 'yr'] <- (2015 - age.val[, age.unit == 'yr']) / 10^6
age.val <- as.data.frame(t(age.val))
names(age.val) <- c('age_min', 'age_max', 'age_mid')

## add processed age values
geoPoly@data <- cbind(geoPoly@data, age.val)

## write out
writeOGR(geoPoly, 'hawaii_state_geol_ageClean.shp', layer = 'hawaii_state_geol_ageClean', 
         driver='ESRI Shapefile', overwrite_layer=TRUE)

