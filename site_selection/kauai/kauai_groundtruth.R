setwd('~/Dropbox/hawaiiDimensions/site_selection/kauai')

x <- readOGR('Kauai_8m_PTS200/Kauai_8m_PTS200.shp', 'Kauai_8m_PTS200')
x <- spTransform(x, CRS('+proj=longlat +datum=WGS84'))

x@data <- data.frame(name = paste('kokee', 1:20, sep=''), description='', stringsAsFactors=FALSE)

# these.plots <- sample(x@data$name, 6)
east <- c(8, 9, 11, 12, 17, 18)
west <- (1:20)[!(1:20 %in% east)]
these.plots <- c(sample(paste('kokee', east, sep=''), 3), sample(paste('kokee', west, sep=''), 3))

x@data$name[!(x@data$name %in% these.plots)] <- paste('temp', x@data$name[!(x@data$name %in% these.plots)], sep='_')

writeOGR(x, 'kokee_pnts.kml', layer='kokee_pnts', driver='KML', overwrite_layer=TRUE)

plot(x)
text(x, labels=x@data$plot_name)




