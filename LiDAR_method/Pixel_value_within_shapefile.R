library(data.table)
library(dplyr)
library(rgdal)
library(sf)
library(raster)
library(tictoc)
library(lidR)

Tree_Shapefile <- st_read("C:\\Users\\owner\\Documents\\Trees\\On_My_Own\\tree_crowns\\functions_wins\\art13.shp")

NAIP <- stack("C:\\Users\\owner\\Documents\\Trees\\On_My_Own\\Aerial_Image\\m_3608151_sw_17_060_20181110.tif")
NAIP_br <- brick(NAIP)

Tree_Shape <- st_transform(Tree_Shapefile, st_crs(NAIP_br))
#Crop to the study area
r.crop <- crop(NAIP_br, extent(Tree_Shape))
#get the mean pixel value within each polygon
r.mean = extract(r.crop, Tree_Shapefile, method="simple", fun=mean, sp=T)
# Write results
Tree_Stats <- as.data.frame(r.mean)

fwrite(Tree_Stats, "tree_metrics.csv")

plotRGB(r.crop,
        r = 1, g = 2, b = 3,
        main = "RGB image \nCNear Bridge")

plotRGB(r.crop,
        r = 4, g = 3, b = 2,
        main = "False Color image \nCNear Bridge")


# calculate NDVI using the red (band 1) and nir (band 4) bands
NAIP_NDVI <- (r.crop[[4]] - r.crop[[1]]) / (r.crop[[4]] + r.crop[[1]])

# plot the data
plot(NAIP_NDVI,
     main = "NDVI",
     axes = FALSE, box = FALSE)


