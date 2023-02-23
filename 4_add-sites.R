proj_32755<-'+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs'

all_rasters<-loadrasterlist_select(rasterdir = 'rasters',
                                     bands = 1:3,
                                     bandnames = c('red', 'green', 'blue'))


split_raster<-function(raster, crs_metric, cellsize = 100, rastnames){
  
  bbox<-st_bbox(raster)%>%st_as_sfc()%>%st_transform(crs = crs_metric)
  
  grid<-st_make_grid(bbox, cellsize = cellsize)
  
  names(grid)<-1:length(grid)
  
  grid_all <- sf::st_sf(grid, 'ID' = seq(length(grid)), grid)%>%st_transform(crs = crs(raster))%>%split(seq(nrow(.)))
  
  plotlist<-rep(list(raster), length(grid))
  
  rast_names<-expand.grid(rastnames, 1:length(grid))%>%mutate(names_cont = paste0(Var1,'_',Var2))
  
  rastersplit<-map2(plotlist, grid_all, raster::crop)
  names(rastersplit)<-rast_names$names_cont
  
  return(rastersplit)
  
}


split_raster_save<-function(raster, crs_metric, cellsize = 100, rastnames, outdir){
  
  bbox<-st_bbox(raster)%>%st_as_sfc()%>%st_transform(crs = crs_metric)
  
  grid<-st_make_grid(bbox, cellsize = cellsize)
  
  names(grid)<-1:length(grid)
  
  grid_all <- sf::st_sf(grid, 'ID' = seq(length(grid)), grid)%>%st_transform(crs = crs(raster))%>%split(seq(nrow(.)))
  
  plotlist<-rep(list(raster), length(grid))
  
  rast_names<-expand.grid(rastnames, 1:length(grid))%>%mutate(names_cont = paste0(Var1,'_',Var2))
  
  rastersplit<-map2(plotlist, grid_all, raster::crop)
  names(rastersplit)<-rast_names$names_cont
  
  saverasterlist(rastersplit, outdir = outdir)
  
  gc()
  
}

allrasters_1<-all_rasters[1:10]
rastnames_1<-names(allrasters_1)

map2(allrasters_1, rastnames_1, 
     split_raster_save, 
     crs_metric = proj_32755, cellsize = 100, outdir = 'outputs/1ha_grid')