library(sp)
library(raster)
library(rgdal)
wd = "~/Trees/NEON-DS-Field-Site-Spatial-Data/"
setwd(wd)
# assign raster to object
dsm <- raster(paste0(wd,"SJER/DigitalSurfaceModel/SJER2013_DSM.tif"))

# view info about the raster.
dsm

# plot the DSM
plot(dsm, main="Lidar Digital Surface Model \n SJER, California")

# import the digital terrain model
dtm <- raster(paste0(wd,"SJER/DigitalTerrainModel/SJER2013_DTM.tif"))

plot(dtm, main="Lidar Digital Terrain Model \n SJER, California")

# use raster math to create CHM
chm <- dsm - dtm

# view CHM attributes
chm

plot(chm, main="Lidar Canopy Height Model \n SJER, California")

# Create a function that subtracts one raster from another
# 
canopyCalc <- function(DTM, DSM) {
  return(DSM -DTM)
}

# use the function to create the final CHM
chm2 <- canopyCalc(dsm,dtm)
chm2

# or use the overlay function
chm3 <- overlay(dsm,dtm,fun = canopyCalc) 
chm3 

# write out the CHM in tiff format. 
writeRaster(chm,paste0(wd,"chm_SJER.tif"),"GTiff")
