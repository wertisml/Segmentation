#file:///C:/Users/owner/Downloads/LiDARAnalysisinRandrLiDARforForestryApplications2018.pdf
library(lidR) #https://r-lidar.github.io/lidRbook/outbox.html#outbox-custom-metrics
library(dplyr)
library(sp)
library(sf)
library(EBImage)

#==============================================================================#
#Read in LiDAR data and correct from feet to meters
#==============================================================================#

LAS = lidR::readLAS("C:\\Users\\owner\\Documents\\Trees\\On_My_Own\\laszip_Conversion.las",
                    select = "*")


ft2m = 1200/3937
LAS$X = LAS$X * ft2m
LAS$Y = LAS$Y * ft2m
LAS$Z = LAS$Z * ft2m
st_crs(LAS) <- 6542

#==============================================================================#
#Normalize the LiDAR data to remove the effect of elevation
#==============================================================================#

LAS <- normalize_height(LAS, knnidw())

#==============================================================================#
# Create the metrics used for individual tree segmentation using 
# Dalponte et al., 2016 method, Uses local maxima to segment trees
#==============================================================================#

chm_p2r <- rasterize_canopy(LAS, 0.5, p2r(subcircle = 0.2), pkg = "terra")

kernel <- matrix(1,3,3)
chm_p2r_smoothed <- raster::focal(chm_p2r, w = kernel, fun = median, na.rm = TRUE)

#Window method 1
fx <- function(x) {
  y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
  y[x < 2] <- 3
  y[x > 20] <- 5
  return(y)
}

#Window method 2
art <- function(x) { x * 0.07 + 3}

ttops_chm_p2r_smoothed <- locate_trees(chm_p2r_smoothed, 
                                       lmf(ws=art, hmin = 2, shape = "circular"))

algo <- dalponte2016(chm_p2r_smoothed, 
                     ttops_chm_p2r_smoothed,
                     th_tree = 2, #height in meters to remove data below
                     th_seed = 0.45, #between 0 and 1, pixel is added to a region if its height is greater than the tree height multiplied by this value
                     th_cr = 0.55, #between 0 and 1, pixel is added if its height is greater than the tree height multiplied by this value
                     max_cr = 13) #maximum value of the crown diameter of a detected tree in pixels

las <- segment_trees(LAS, algo) # segment point cloud
#plot(las, bg = "white", size = 4, color = "treeID") # visualize trees

#==============================================================================#
# Create the metrics used for individual tree segmentation using 
# Li et al., 2012 method
#==============================================================================#

algo2 <- li2012()

li <- segment_trees(LAS, algo2)

#plot(li, bg = "white", size = 4, color = "treeID")

crowns_li <- crown_metrics(las, func = .stdtreemetrics, geom = "concave")

#==============================================================================#
# Create the metrics used for individual tree segmentation using 
# watershed segmentation method
#==============================================================================#

watershed <- segment_trees(LAS, lidR::watershed(chm_p2r_smoothed, th_tree = 2))

#plot(watershed, bg = "white", size = 4, color = "treeID")

water_crowns <- crown_metrics(watershed, func = .stdtreemetrics, geom = "concave")

#==============================================================================#
# Create the custom functions used in crown_metrics
#==============================================================================#

stdmetrics_int <- function(i, z = NULL, class = NULL, rn = NULL)
{
  itot <- imax <- imean <- isd <- icv <- iskew <- ikurt <- NULL
  icumzq10 <- icumzq30 <- icumzq50 <- icumzq70 <- icumzq90 <- NULL
  
  n <- length(i)
  itot <- as.double(sum(i))
  imean <- mean(i)
  
  probs <- seq(0.05, 0.95, 0.05)
  iq 	  <- as.list(stats::quantile(i, probs))
  names(iq) <- paste("iq", probs*100, sep = "")
  
  metrics <- list(
    itot = itot,
    imax  = max(i),
    imean = imean,
    isd   = stats::sd(i),
    iskew = (sum((i - imean)^3)/n)/(sum((i - imean)^2)/n)^(3/2),
    ikurt = n * sum((i - imean)^4)/(sum((i - imean)^2)^2)
  )
  
  if (!is.null(class))
  {
    metrics <- c(metrics, list(ipground = sum(i[class == 2])/itot*100)) #look into adding information about other returns
  }
  
  if (!is.null(z))
  {
    zq <- stats::quantile(z, probs = seq(0.0, 1, 0.05))
    
    ipcum <- list(
      ipcumzq05 = sum(i[z <= zq[1]])/itot*100,
      ipcumzq10 = sum(i[z <= zq[2]])/itot*100,
      ipcumzq15 = sum(i[z <= zq[3]])/itot*100,
      ipcumzq20 = sum(i[z <= zq[4]])/itot*100,
      ipcumzq25 = sum(i[z <= zq[5]])/itot*100,
      ipcumzq30 = sum(i[z <= zq[6]])/itot*100,
      ipcumzq35 = sum(i[z <= zq[7]])/itot*100,
      ipcumzq40 = sum(i[z <= zq[8]])/itot*100,
      ipcumzq45 = sum(i[z <= zq[9]])/itot*100,
      ipcumzq50 = sum(i[z <= zq[10]])/itot*100,
      ipcumzq55 = sum(i[z <= zq[11]])/itot*100,
      ipcumzq60 = sum(i[z <= zq[12]])/itot*100,
      ipcumzq65 = sum(i[z <= zq[13]])/itot*100,
      ipcumzq70 = sum(i[z <= zq[14]])/itot*100,
      ipcumzq75 = sum(i[z <= zq[15]])/itot*100,
      ipcumzq80 = sum(i[z <= zq[16]])/itot*100,
      ipcumzq85 = sum(i[z <= zq[17]])/itot*100,
      ipcumzq90 = sum(i[z <= zq[18]])/itot*100,
      ipcumzq95 = sum(i[z <= zq[19]])/itot*100,
      ipcumzq100 = sum(i[z <= zq[20]])/itot*100
    )
    
    metrics <- c(metrics, ipcum)
  }
  
  metrics
}

#==============================================================================#
#Run the crown_metrics to get the tree statistics
#==============================================================================#

returns <- crown_metrics(las, func = ~stdmetrics_int(Intensity, Z), geom = "concave")
crowns <- crown_metrics(las, func = .stdtreemetrics, geom = "concave")
base <- crown_metrics(las, func = .stdmetrics, geom = "concave")
shapes <- crown_metrics(las, func = .stdshapemetrics, geom = "concave")

#==============================================================================#
#Run if you want to see what the plot looks like
#==============================================================================#

plot(crowns["convhull_area"], main = "Crown area (convex hull)")
plot(sf::st_geometry(crowns), reset = FALSE)

col <- height.colors(50)
plot(chm_p2r_smoothed, main = "CHM P2R 0.5 smoothed", col = col); plot(sf::st_geometry(crowns), reset = FALSE, add= TRUE)

#==============================================================================#
#Combine the dataframes into one
#==============================================================================#
base <- base[ -c(45:49) ]

tree_metrics <- cbind(crowns, base)
tree_metrics <- cbind(tree_metrics, shapes)
tree_metrics <- cbind(tree_metrics, returns)
#Need to fix this now that there is a new dataframe added in
tree_metrics[,95:97] <- NULL
tree_metrics[,67:73] <- NULL
tree_metrics[,57] <- NULL
tree_metrics[,5] <- NULL

#==============================================================================#
# Post metric statistics
#==============================================================================#

# Canopy base height

tree <- as.data.frame(tree_metrics)

# The columns containing the intensity percentage cumulative height percentile
intes_height <- tree[,65:84]

# Calculating the intensity differnce between each height percentile 
n = 10
for(i in 1:ncol(intes_height)){
  if(i < 20) {
    
    intes_height[[paste0("diff_", n)]] = intes_height[,i+1] - intes_height[,i] 
    
    n = n+5
    i = i+1
  }
}

# Determine the column with the biggest intensity change and get the height percentile
intes_height <- intes_height %>%
  mutate(Largest_Column = colnames(.[,21:39])[apply(.[,21:39],1,which.max)],
         height = as.numeric(sub(".*_", ".", Largest_Column)))

# Multiply the height percentile by the height of the tree
tree_metrics$Canopy_Base <- tree_metrics$Z * intes_height$height

#==============================================================================#
# Write off the dataframe into a shapefile
#==============================================================================#

st_write(tree_metrics,
         dsn = "~/Trees/On_My_Own/tree_crowns",
         layer = "crown_outline_concave",
         driver = "ESRI Shapefile")

