
# test sites chm extraction -----------------------------------------------

library(lidR)
library(raster)
library(sf)
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

proj_32755<-'+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs'

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

rasterOptions(maxmemory = 2e+10, 
              tmpdir = 'Y:/raster_dump', 
              timer = T, progress = 'text') #for VM only!

proj_32755<-'+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs'


# run test ----------------------------------------------------------------

plot1<-stack('outputs/1ha_grid_full/Plot1H_3.tif', bands = 1:3)
plot2<-stack('outputs/1ha_grid_full/Plot15H_40.tif', bands = 1:3)

plotlist<-list(plot1, plot2)
names(plotlist)<-c('Plot1H_3', 'Plot15H_40')

#get outline

plot1_bbox<-st_bbox(plot1)%>%st_as_sfc()%>%st_transform(crs = proj_32755)%>%st_sf()%>%
  mutate(plot = 'Plot1H_3')%>%select(2)
  
plot2_bbox<-st_bbox(plot2)%>%st_as_sfc()%>%st_transform(crs = proj_32755)%>%st_sf()%>%
  mutate(plot = 'Plot15H_40')%>%select(2)

plot(plot1_bbox)

#get center

plot1_center<-st_centroid(plot1_bbox)

plot(plot1_center, add = T)


# clip --------------------------------------------------------------------

test_all<-loadrasterlist_select('outputs/1ha_grid_full', 
                                bands = 1:3, 
                                bandnames = c('red', 'green', 'blue'))

bbox_poly<-function(raster, crs_metric){
  
  bbox<-st_bbox(raster)%>%st_as_sfc()%>%st_transform(crs = crs_metric)%>%st_sf()%>%
    mutate(poly = 'polygon')
  
}

bbox_all<-map(test_all, bbox_poly, crs_metric = proj_32755)


bbox_all_bind<-bind_rows(bbox_all, .id = 'plot')

#points + crop

ctg<-readLAScatalog('pointclouds')

plot(ctg)
plot(bbox_all_bind$geom, col = 'red', add = T)

opt_output_files(ctg) <- 
  paste0('Y:/Anu/DRONE_ANUCH4/pointclouds/testsites/clipped/{plot}') 

cropped <- clip_roi(ctg, bbox_all_bind)


# ground detection + norm-------------------------------------------------------

opt_output_files(cropped) <- 
  'Y:/Anu/DRONE_ANUCH4/pointclouds/testsites/ground/{ORIGINALFILENAME}_ground'

ground<-classify_ground(cropped, csf())

#ground<-readLAScatalog('pointclouds/ground')

#ground<-readLAScatalog('pointclouds/testsites/ground_split')

opt_output_files(ground) <- 
  'Y:/Anu/DRONE_ANUCH4/pointclouds/testsites/norm/{ORIGINALFILENAME}_norm'

norm<-normalize_height(ground, tin())

# chm ---------------------------------------------------------------------

norm<-readLAScatalog('pointclouds/testsites/norm_split')

opt_output_files(norm) <- 
  'Y:/Anu/DRONE_ANUCH4/pointclouds/testsites/chm/{ORIGINALFILENAME}_chm'

chm <- grid_canopy(norm, 0.5, p2r(0.5))

gc()

norm<-readLAScatalog('pointclouds/testsites/norm_split2')

opt_output_files(norm) <- 
  'Y:/Anu/DRONE_ANUCH4/pointclouds/testsites/chm/{ORIGINALFILENAME}_chm'

chm <- grid_canopy(norm, 0.5, p2r(0.5))

gc()

norm<-readLAScatalog('pointclouds/testsites/norm_split3')

opt_output_files(norm) <- 
  'Y:/Anu/DRONE_ANUCH4/pointclouds/testsites/chm/{ORIGINALFILENAME}_chm'

chm <- grid_canopy(norm, 0.5, p2r(0.5))

gc()

norm<-readLAScatalog('pointclouds/testsites/norm_split4')

opt_output_files(norm) <- 
  'Y:/Anu/DRONE_ANUCH4/pointclouds/testsites/chm/{ORIGINALFILENAME}_chm'

chm <- grid_canopy(norm, 0.5, p2r(0.5))

gc()


# combine --------------------------------------------------------------------

plotlist<-loadrasterlist_select(rasterdir = 'outputs/1ha_grid_full', 
                                bands = 1:3, 
                                bandnames = c('red', 'green', 'blue'))

chms<-loadrasterlist_select(rasterdir = 'pointclouds/testsites/chm', 
                            bands = 1, 
                            bandnames = 'chm')

#remove missing plots

plotnames<-names(plotlist)%>%as.data.frame()%>%rename(name = 1)
chm_names<-names(chms)%>%str_remove('_ground_norm_chm')%>%as.data.frame()%>%rename(name = 1)

missing<-plotnames%>%filter(!name %in% chm_names$name)

plotlist_filt<-plotlist%>%discard(names(.) %in% missing$name)

#chms<-rev(chms)

names(chms)<-names(plotlist_filt)

#transform to latlong

project_save<-function(raster, crs, rastname, dir){
  
  projd<-projectRaster(raster, crs = crs)
  
  x<-getwd()
  
  setwd(dir)
  
  writeRaster(projd, filename = rastname, 'GTiff', options="COMPRESS=LZW", overwrite = F)

  setwd(x)
  
}

#test

project_save(chms$Plot14H_10, rastname = '14h', crs = crs(plotlist_filt$Plot14H_10), dir = 'pointclouds/testsites/chm_latlong')

#all

map2(chms, names(chms), project_save,
    crs = crs(plotlist_filt$Plot14H_10), dir = 'pointclouds/testsites/chm_latlong')



#resample and save

chms_trans<-loadrasterlist_select(rasterdir = 'pointclouds/testsites/chm_latlong', 
                                  bands = 1, 
                                  bandnames = 'chm')

sample_save<-function(raster, target, rastname, dir){
  
  sampld<-raster::resample(raster, target)
  
  stack<-stack(target, sampld)
  
  x<-getwd()
  
  setwd(dir)
  
  writeRaster(stack, filename = rastname, 'GTiff', options="COMPRESS=LZW", overwrite = F)
  
  gc()
  
  setwd(x)
  
}

#test

sample_save(chms_trans$Plot14H_10, plotlist_filt$Plot14H_10, names(chms_trans)[1],
            dir = 'pointclouds/testsites/chm_reproj')

#listtest

list_test<-list(list(chms_trans$Plot14H_10, chms_trans$Plot14H_11),
                list(plotlist_filt$Plot14H_10, plotlist_filt$Plot14H_11),
                list('one', 'two'))

pmap(list_test, sample_save,
     dir = 'pointclouds/testsites/chm_reproj')


plotnames<-names(plotlist_filt) #why theh ell does this not work??

list<-list(chms_trans, plotlist_filt, as.list(as.character(1:415)))

pmap(list, sample_save,
     dir = 'pointclouds/testsites/chm_reproj')



