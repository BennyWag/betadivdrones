# get tree metrics from drone data ----------------------------------------

library(raster)
library(sf)
library(exactextractr)
library(tidyverse)

loadshapelist<-function(dir, extension = '.gpkg'){
  
  z<-getwd()
  setwd(dir)
  
  #
  print('listing files in raster folder') 
  temp<-list.files(pattern = paste0('*',extension), full.names=FALSE)
  
  #
  print('compiling rater names')
  names_plots<-temp
  names_plots<-str_replace(names_plots, extension,'')
  
  #
  print('load all rasters into list')
  allshapes <- lapply(temp, st_read)
  
  #
  print('set stack names')
  names(allshapes)<-names_plots
  
  setwd(z)
  return(allshapes)
  
}

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

rasterOptions(progress = 'text')

# load data ---------------------------------------------------------------

#spatial preds

allpreds<-loadrasterlist_select('outputs/preds/lowres', 
                                func = raster,
                                bandnames = 'layer')


allchm<-loadrasterlist_select('outputs/rgb+chm', 
                                  func = stack, 
                                  bands = 4,
                                  bandnames = 'chm')


#trees

ttops_5<-loadshapelist('pointclouds/treetops_5', extension = '.shp')
names(ttops_5)<-names(allpreds)

ttops_1<-loadshapelist('pointclouds/treetops_1', extension = '.shp')
names(ttops_1)<-names(allpreds)

ttops_05<-loadshapelist('pointclouds/treetops_0.5', extension = '.shp')
names(ttops_05)<-names(allpreds)

ttops_025<-loadshapelist('pointclouds/treetops_0.25', extension = '.shp')
names(ttops_025)<-names(allpreds)

# 1 = Euc, 2 = Ac, 3 = Other


# extract species and height  ---------------------------------------------

extract_tree<-function(raster, chm, mask){
  
  pb$tick()$print()
  mask$pred_class<-raster::extract(raster, mask)
  mask$chm_height<-raster::extract(chm, mask)

  return(mask)
  
}

#run

pb <- progress_estimated(length(allpreds))
trees_5m<-pmap(list(allpreds, allchm, ttops_5), extract_tree)
gc()

pb <- progress_estimated(length(allpreds))
trees_1m<-pmap(list(allpreds, allchm, ttops_1), extract_tree)
gc()

pb <- progress_estimated(length(allpreds))
trees_05m<-pmap(list(allpreds, allchm, ttops_05), extract_tree)
gc()

pb <- progress_estimated(length(allpreds))
trees_025m<-pmap(list(allpreds, allchm, ttops_025), extract_tree)
gc()


# tally n Euc and Ac and mean height --------------------------------------

#test

trees11H<-trees_1m$Plot11H

trees11H_tally<-trees11H%>%st_drop_geometry()%>%
  filter(pred_class<3)%>%
  add_count(pred_class)%>%
  group_by(pred_class)%>%
  summarise(m_height = mean(chm_height),
            count = max(n))%>%
  mutate(ID = 'plot')%>%
  pivot_wider(names_from = 1, values_from = 2:3)%>%
  select(n_euc = 4, n_ac = 5, height_euc = 2, height_ac = 3)

#function and run on all

trees_tally<-function(table){
  
  table_tally<-table%>%st_drop_geometry()%>%
    filter(pred_class<3)%>%
    add_count(pred_class)%>%
    group_by(pred_class)%>%
    summarise(m_height = mean(chm_height),
              count = max(n))%>%
    mutate(ID = 'plot')%>%
    pivot_wider(names_from = 1, values_from = 2:3)%>%
    select(n_euc = 4, n_ac = 5, height_euc = 2, height_ac = 3)
  
  return(table_tally)
}


trees_tally_5m<-map(trees_5m[-c(2)], trees_tally)
trees_tally_5m_df<-bind_rows(trees_tally_5m, .id = 'plot')%>%
  rename(n_euc_5 = 2, n_ac_5 = 3, height_euc_5 = 4, height_ac_5 = 5)

trees_tally_1m<-map(trees_1m[-c(2)], trees_tally)
trees_tally_1m_df<-bind_rows(trees_tally_1m, .id = 'plot')%>%
  rename(n_euc_1 = 2, n_ac_1 = 3, height_euc_1 = 4, height_ac_1 = 5)

trees_tally_05m<-map(trees_05m[-c(2)], trees_tally)
trees_tally_05m_df<-bind_rows(trees_tally_05m, .id = 'plot')%>%
  rename(n_euc_05 = 2, n_ac_05 = 3, height_euc_05 = 4, height_ac_05 = 5)

trees_tally_025m<-map(trees_025m[-c(2)], trees_tally)
trees_tally_025m_df<-bind_rows(trees_tally_025m, .id = 'plot')%>%
  rename(n_euc_025 = 2, n_ac_025 = 3, height_euc_025 = 4, height_ac_025 = 5)

#bind all

trees_tally_all<-cbind(trees_tally_5m_df, trees_tally_1m_df, trees_tally_05m_df, trees_tally_025m_df)%>%
  select(!c(6,11,16))

write.csv(trees_tally_all, 'outputs/trees_tally.csv', row.names = F)
