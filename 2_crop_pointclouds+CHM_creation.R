# crop pointclouds and create CHMs ----------------------------------------

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

rasterOptions(progress = 'text')

# load data ---------------------------------------------------------------

#shapes

all_plots<-loadshapelist(dir = 'shapes')

all_plots_transform<-map(all_plots, st_transform, crs = proj_32755)

all_plots_buffer<-map(all_plots_transform, st_buffer, dist = 70.711, 
                      endCapStyle = 'SQUARE')


all_plots_bind<-bind_rows(all_plots_buffer, .id = 'plot')%>%select(plot, geom)

#points + crop

ctg<-readLAScatalog('pointclouds')

plot(ctg)
plot(all_plots_bind$geom, col = 'red', add = T)

opt_output_files(ctg) <- paste0('/Users/Ben/OneDrive - The University of Melbourne/DRONE_ANUCH4/pointclouds/cropped/{plot}') 

cropped <- clip_roi(ctg, all_plots_bind)


# ground detection + norm-------------------------------------------------------

opt_output_files(cropped) <- '/Users/Ben/OneDrive - The University of Melbourne/DRONE_ANUCH4/pointclouds/ground/{ORIGINALFILENAME}_ground'

ground<-classify_ground(cropped, csf())

#ground<-readLAScatalog('pointclouds/ground')

opt_output_files(ground) <- '/Users/Ben/OneDrive - The University of Melbourne/DRONE_ANUCH4/pointclouds/norm/{ORIGINALFILENAME}_norm'

norm<-normalize_height(ground, tin())


# chm ---------------------------------------------------------------------

opt_output_files(norm) <- '/Users/Ben/OneDrive - The University of Melbourne/DRONE_ANUCH4/pointclouds/chm/{ORIGINALFILENAME}_chm'

chm <- grid_canopy(norm, 0.5, p2r(0.5))

# add to RGB --------------------------------------------------------------

rgb<-loadrasterlist_select(rasterdir = 'outputs/crop', 
                                   bands = 1:3, 
                                   bandnames = c('red', 'green', 'blue'))

chms<-loadrasterlist_select(rasterdir = 'pointclouds/chm', 
                            bands = 1, 
                            bandnames = 'chm')

names(chms)<-names(rgb)

chms_transform<-map(chms, projectRaster, crs = crs(rgb$Plot10H))

chm_align<-map2(chms_transform, rgb, raster::resample)

stack_all<-map2(rgb, chm_align, stack)

#save

saverasterlist(stack_all, 'outputs/rgb+chm')

