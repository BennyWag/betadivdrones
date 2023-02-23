# predict to test sites and extract data ----------------------------------

library(raster)
library(rgdal)
library(caret)
library(corrplot)
library(randomForest)
library(exactextractr)
library(sf)
library(spatialEco)
library(broom)
library(cvms)
library(MuMIn)
library(patchwork)
library(landscapemetrics)
library(tidyverse)

proj_32755<-'+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs'

names<-c('red', 'green', 'blue', 'chm')

colors <- c('darkgreen', 'blue', 'tomato4')

loadrasterlist_select<-function(rasterdir, func = stack, bands = NULL, bandnames){
  
  z<-getwd()
  setwd(rasterdir)
  
  #
  print('listing files in raster folder') 
  temp<-list.files(pattern = '*.tif', full.names=FALSE)
  
  #
  print('compiling rater names')
  names_plots<-temp
  names_plots<-str_replace(names_plots,'.tif','')
  names_band<-bandnames
  
  #
  print('load all rasters into list')
  allrasters <- lapply(temp, func, bands = bands)
  
  #
  print('set stack names')
  allrasters<-lapply(allrasters, setNames, names_band)
  names(allrasters)<-names_plots
  
  setwd(z)
  return(allrasters)
  
}

saverasterlist<-function(list, outdir){
  
  x<-getwd()
  
  setwd(outdir)
  
  mapply(writeRaster, list, names(list), bylayer=F, 'GTiff', options="COMPRESS=LZW")
  
  setwd(x)
  
  gc()
  memory.size(max=F)
  
}

raster_stats<-function(class_raster){
  
  print('creating data frame')
  rast_df<-class_raster%>%as.data.frame(xy = T, na.rm = T)
  
  rast_df$ID<-1
  
  res<-res(class_raster)[1]
  
  print('tallying class area')
  rast_area<-rast_df%>%group_by(layer)%>%
    summarise(npixel = sum(ID))%>%
    mutate(sumA = sum(npixel), per = 100*npixel/sumA)%>%rename(class = layer)
  
  print('getting landscape metrics')
  landmetrics<-calculate_lsm(class_raster, what = c("lsm_c_np", 'lsm_c_clumpy'),
                             directions = 8)
  
  landmetrics_wide<-pivot_wider(landmetrics, names_from = c('metric'), values_from = c('value'))%>%select(3, 5, 6)
  
  print('binding')
  
  metrics_combined<-inner_join(rast_area, landmetrics_wide, by = 'class')
  
  print('done')
  return(metrics_combined)
  
}


rasterOptions(maxmemory = 2e+10, 
              tmpdir = 'Y:/raster_dump', 
              timer = T, progress = 'text') #for VM only!


model<-readRDS('outputs/rf_model.rds')

alltest<-loadrasterlist_select('pointclouds/testsites/chm_reproj',
                               bands = 1:4,
                               bandnames = names)

# test prediction ---------------------------------------------------------

plot<-alltest$`1`

beginCluster()
pred<-clusterR(plot, raster::predict, 
               args = list(model = model, inf.rm = T), 
               progress = 'text')
endCluster()

plot(pred, col = colors)


#function for listing

predlist<-list(plot, alltest$`15`, alltest$`231`)

#dir.create('pointclouds/testsites/testpreds')

pred_n_save<-function(raster, rastname, mod = model, dir){
  
  pred<-clusterR(raster, raster::predict, 
                 args = list(model = mod, inf.rm = T), 
                 progress = 'text')  
  
  pred_agg<-aggregate(pred, 2, fun = min, na.rm = F)
  
  x<-getwd()
  
  setwd(dir)
  
  writeRaster(pred_agg, filename = rastname, 'GTiff', options="COMPRESS=LZW", overwrite = F)
  
  gc()
  
  setwd(x)
  
}


#func testing

#one

beginCluster()

pred_n_save(plot, rastname = 'A_agg', dir = 'pointclouds/testsites/testpreds')

endCluster()

#list

beginCluster()

map2(predlist, list('A2', 'B', 'C'), 
     pred_n_save, 
     mod = model,
     dir = 'pointclouds/testsites/testpreds')

endCluster()

#pred to all

prednames<-names(alltest)

beginCluster()

map2(alltest, prednames, 
     pred_n_save, 
     mod = model,
     dir = 'pointclouds/testsites/testpreds')

endCluster()


# get metrics -------------------------------------------------------------

allpreds<-loadrasterlist_select('pointclouds/testsites/testpreds',
                               func = raster,
                               bandnames = 'layer')

#check

plot(allpreds$`101`, col = colors)

pred_test<-raster_stats(allpreds$`101`)

#get all

metrics_plots<-lapply(allpreds, raster_stats)

#combine

metrics_plots_bind<-bind_rows(metrics_plots, .id = 'plot')

#wide format

metrics_wide<-pivot_wider(metrics_plots_bind, names_from = 2, values_from = 5:7)%>% 
  group_by(plot)%>%summarise_all(sum, na.rm = T)%>%
  rename(perc_euc = 4,
         perc_ac = 5,
         perc_unk = 6,
         agg_euc = 7,
         agg_ac = 8,
         agg_unk = 9,
         patch_euc = 10,
         patch_ac = 11,
         patch_unk = 12)%>%
  select(1,4:12)

write.csv(metrics_wide, 'outputs/1ha_grid_full/data/testsites_metrics.csv')


# add ahmi ----------------------------------------------------------------

ahmi<-raster('data/ahmi_81-20.tif')

#get centroids for all

test_bbox<-st_bbox(allpreds$`1`)%>%st_as_sfc()%>%
  st_transform(crs = crs(allpreds$`1`))%>%
  st_centroid()%>%
  st_as_sf()%>%
  rename(geom = x)%>%
  mutate(type = 'centroid')

raster_centroid<-function(raster){
  
  bbox<-st_bbox(raster)%>%st_as_sfc()%>%
    st_transform(crs = crs(raster))%>%
    st_centroid()%>%
    st_as_sf()%>%
    rename(geom = x)%>%
    mutate(type = 'centroid')
  
}

all_centroids<-map(allpreds, raster_centroid)


centroids_bind<-bind_rows(all_centroids, .id = 'plot')

#get ahmi

centroids_bind$AHMI<-raster::extract(ahmi, centroids_bind)

#combine

metrics_ahmi<-read.csv('outputs/1ha_grid_full/data/testsites_metrics.csv')%>%
  mutate(plot = as.character(plot))

metrics_bind<-inner_join(metrics_ahmi, centroids_bind, by = 'plot')%>%select(2:11, 14)

write.csv(metrics_bind, 'outputs/1ha_grid_full/data/testsites_metrics_AHMI.csv', row.names = F)


