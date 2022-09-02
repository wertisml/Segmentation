library(TreeLS)
library(lidR)
library(rLiDAR)
library(rgdal)
library(data.table)
library(sf)
library(raster)
library(tictoc)
# open sample plot file

LAS = lidR::readLAS("C:\\Users\\owner\\Documents\\Trees\\On_My_Own\\laszip_Conversion.las", 
                    filter = "-keep_xy 1200000 882500 1201900 885000 (xmin ymin xmax ymax)")
shpfile <- "C:\\Users\\owner\\Documents\\Trees\\On_My_Own\\tree_crowns\\tree_crowns.shp"
#inventory <- sf::st_read(shpfile, quiet = TRUE)
inventory <- readOGR(shpfile)

# check the crs of the shapefile
st_crs(inventory)
#rewrite the CRS of the LiDAR to match the CRS of the shapefile
st_crs(LAS) <- 2264

#Normalize the LAS file to remove elevation
LAS <- normalize_height(LAS, knnidw())

#Hopefully this will help it be cut down
inventory1 <- raster::crop(inventory, extent(1200000, 1201900, 882500, 885000))

#Get the metrics from the LiDAR
tic()
base_metrics1 <- plot_metrics(LAS, .stdmetrics, inventory1, radius = 11.28) #z is height
toc()
tree_metrics <- plot_metrics(LAS, .stdtreemetrics, inventory1, radius = 11.28) #all metrics
shape_metrics <- plot_metrics(LAS, .stdshapemetrics, inventory1, radius = 11.28) 

