library(rgdal)
library(raster)
library(tmap)
library(tmaptools)
library(lidR)
library(RStoolbox)

LAS <- readLAScatalog("C:\\Users\\owner\\Documents\\Trees\\On_My_Own\\laszip_Conversion.las")
projection(LAS) <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "
las_check(LAS)

crs(LAS) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

opt_chunk_size(LAS) <- 500
plot(LAS, chunk_pattern = TRUE)

opt_chunk_buffer(LAS) <- 20
plot(LAS, chunk_pattern = TRUE)

summary(LAS)

opt_output_files(LAS) <- "C:/Users/owner/Document/Trees/On_My_Own/dtm_{XLEFT}_{YBOTTOM}"
dtm <- grid_terrain(LAS, res = 2, knnidw(k = 10, p = 2), keep_lowest = FALSE)

tm_shape(dtm)+
  tm_raster(style= "cont", palette=get_brewer_pal("Greys", plot=FALSE))+
  tm_layout(legend.outside = TRUE)

writeRaster(dtm, "dtm.tif")


opt_output_files(LAS) <- "C:/Users/owner/Document/Trees/On_My_Own/norm_{XLEFT}_{YBOTTOM}"
lasnorm <- normalize_height(LAS, dtm)

opt_output_files(LAs) <- "C:/Users/owner/Document/Trees/On_My_Own/dsm_{XLEFT}_{YBOTTOM}"
dsm <- grid_canopy(LAS, res = 2, pitfree(c(0,2,5,10,15), c(0, 1)))

writeRaster(dsm, "dsm.tif")

ndsm <- dsm - dtm
ndsm[ndsm<0]=0

tm_shape(ndsm)+
  tm_raster(style= "quantile", n=7, palette=get_brewer_pal("Greens", n=7, plot=FALSE))+
  tm_layout(legend.outside = TRUE)