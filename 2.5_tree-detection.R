# tree detection and crowns ----------------------------------------

library(lidR)
library(raster)
library(sf)
library(tidyverse)


# trial on one site -------------------------------------------------------

las <- readLAS('pointclouds/norm/Plot12H_ground_norm.las', select = "xyzr", filter = "-drop_z_below 0")

#chm <- grid_canopy(las, 0.5, pitfree(subcircle = 0.2))
chm_der<-raster('pointclouds/chm/Plot12H_ground_norm_chm.tif')

plot(chm, col = height.colors(50))
plot(chm_der, col = height.colors(50))

#on ptcloud

ttops <- find_trees(las, lmf(ws = 3))

plot(chm_der, col = height.colors(50))
plot(ttops, add = TRUE)

#on chm

ttops_chm <- find_trees(chm_der, lmf(3))

plot(chm_der, col = height.colors(50))
plot(ttops_chm, add = TRUE)

st_write(ttops_chm%>%st_as_sf(), 'outputs/treetops_12H.gpkg')

#segment chm

algo <- dalponte2016(chm_der, ttops)
crowns <- algo()

plot(crowns, col = pastel.colors(200))

#segment las

algo1 <- dalponte2016(chm_der, ttops_chm)

las <- segment_trees(las, algo1, attribute = "IDdalponte")

crowns <- delineate_crowns(las, attribute = "IDdalponte", type = "concave")

plot(chm_der, col = height.colors(50))
plot(crowns, add = TRUE)

st_write(crowns%>%st_as_sf(), 'outputs/crowns_12H.gpkg')


#filter to redo

las_acacia<-mask(chm_der, crowns, inverse = T)

ttops_acacia <- find_trees(las_acacia, lmf(1))

plot(chm_der, col = height.colors(50))
plot(ttops_acacia, add = TRUE)


# detect all --------------------------------------------------------------

cloud_norm<-readLAScatalog('pointclouds/norm', select = "xyzr", filter = "-drop_z_below 0")

opt_output_files(cloud_norm) <- paste0('Y:/Anu/DRONE_ANUCH4/pointclouds/treetops_5/{ORIGINALFILENAME}_ttops') 

ttops_all<- find_trees(cloud_norm, lmf(ws = 2, hmin = 5))

gc()

opt_output_files(cloud_norm) <- paste0('Y:/Anu/DRONE_ANUCH4/pointclouds/treetops_1/{ORIGINALFILENAME}_ttops_1') 

ttops_all<- find_trees(cloud_norm, lmf(ws = 2, hmin = 1))

gc()

opt_output_files(cloud_norm) <- paste0('Y:/Anu/DRONE_ANUCH4/pointclouds/treetops_0.5/{ORIGINALFILENAME}_ttops_05') 

ttops_all<- find_trees(cloud_norm, lmf(ws = 2, hmin = 0.5))

gc()

opt_output_files(cloud_norm) <- paste0('Y:/Anu/DRONE_ANUCH4/pointclouds/treetops_0.25/{ORIGINALFILENAME}_ttops_025') 

ttops_all<- find_trees(cloud_norm, lmf(ws = 2, hmin = 0.25))

gc()


