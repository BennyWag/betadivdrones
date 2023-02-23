
# crop rasters ------------------------------------------------------------

library(raster)
library(sf)
library(tidyverse)

# functions ---------------------------------------------------------------

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

saverasterlist<-function(list, outdir){
  
  x<-getwd()
  
  setwd(outdir)
  
  mapply(writeRaster, list, names(list), bylayer=F, 'GTiff', options="COMPRESS=LZW")
  
  setwd(x)
  
  gc()
  memory.size(max=F)
  
}

proj_32755<-'+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs'

rasterOptions(progress = 'text')

# load rasters and plots --------------------------------------------------------

all_rasters<-loadrasterlist_select(rasterdir = 'rasters', 
                                   bands = 1:3, 
                                   bandnames = c('red', 'green', 'blue'))

all_plots<-loadshapelist(dir = 'shapes')


# transform plots and create bbox -----------------------------------------

all_plots_transform<-map(all_plots, st_transform, crs = proj_32755)

crs(all_plots$Plot10H)
crs(all_plots_transform$Plot10H)


test<-st_buffer(all_plots_transform$Plot10H, dist = 70.711, endCapStyle = 'SQUARE')

st_area(test)

all_plots_buffer<-map(all_plots_transform, st_buffer, dist = 70.711, 
                      endCapStyle = 'SQUARE')

all_plots_buffer_latlong<-map(all_plots_buffer, 
                              st_transform, crs = crs(all_rasters$Plot10H))


# crop rasters and save--------------------------------------------------------

all_rasters_crop<-map2(all_rasters, all_plots_buffer_latlong, raster::crop)

saverasterlist(all_rasters_crop, 'outputs/crop')

# do 15M

# M15<-stack('rasters/Plot15M.tif', bands = 1:3)
# M15_plot<-st_read('shapes/Plot15M.gpkg')%>%
#   st_transform(crs = proj_32755)%>%
#   st_buffer(dist = 70.711, endCapStyle = 'SQUARE')%>%
#   st_transform(crs = crs(M15))
# 
# M15_crop<-crop(M15, M15_plot)
# names(M15_crop)<-c('red', 'green', 'blue')
# 
# writeRaster(M15_crop, 'outputs/crop/Plot15M.tif', options="COMPRESS=LZW")


