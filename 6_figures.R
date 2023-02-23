# figures -----------------------------------------------------------------

library(raster)
library(sf)
library(randomForest)
library(caret)
library(MuMIn)
library(tidyverse)
library(ggspatial)
library(RStoolbox)
library(viridis)
library(patchwork)


# variable importance plot ------------------------------------------------

extract_varimp_edit<-function(model, title = NULL){
  
  var.imp <- data.frame(randomForest::importance(model, type=1))
  var.imp$Variables <- row.names(var.imp)%>%as.factor()
  rownames(var.imp)<-1:nrow(var.imp)
  varimp <- var.imp[order(var.imp$MeanDecreaseAccuracy,decreasing = T),]
  
  var.imp2 <- data.frame(randomForest::importance(model, type=2))
  var.imp2$Variables <- row.names(var.imp2)%>%as.factor()
  rownames(var.imp2)<-1:nrow(var.imp2)
  varimp2 <- var.imp2[order(var.imp2$MeanDecreaseGini,decreasing = T),]
  
  imp<-ggplot(varimp, aes(x = reorder(Variables,MeanDecreaseAccuracy), y = MeanDecreaseAccuracy))+
    geom_bar(stat = "summary", fun.y = 'mean', fill = 'black')+
    coord_flip()+
    ylab('Mean decrease Accuracy')+xlab('')+
    ggtitle(title)+
    geom_text(aes(label=round(MeanDecreaseAccuracy, digits = 3)), position=position_dodge(width=0.9), hjust=1, color = 'white')+
    theme_bw()+
    theme(plot.title = element_text(size =18, face='bold'),
          axis.title.y = element_text(size = 16, face = 'bold'),
          axis.title.x = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 15, face = 'bold', color = 'black'), 
          axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 15))
  
  imp2<-ggplot(varimp2, aes(x = reorder(Variables,MeanDecreaseGini), y = MeanDecreaseGini))+
    geom_bar(stat = "summary", fun.y = 'mean', fill = 'black')+
    coord_flip()+
    ylab('Mean decrease Gini')+xlab('')+
    ggtitle(title)+
    geom_text(aes(label=round(MeanDecreaseGini, digits = 3)), position=position_dodge(width=0.9), hjust=1, color = 'white')+
    theme_bw()+
    theme(plot.title = element_text(size =18, face='bold'),
          axis.title.y = element_text(size = 16, face = 'bold'),
          axis.title.x = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 15, face = 'bold', color = 'black'), 
          axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 15))
  
  plot<- imp / imp2 + plot_annotation(tag_levels = 'A')
  
  return(plot)
  
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

model<-readRDS('outputs/rf_model.rds')

extract_varimp_edit(model)

ggsave('Varimp1.svg',path = 'figures/', width = 20, height = 15.6, units = 'cm', dpi = 600)


# RGB + CHM ---------------------------------------------------------------

names<-c('red', 'green', 'blue', 'chm')


allrasters_plot<-loadrasterlist_select('outputs/rgb+chm', 
                                  func = stack, 
                                  bands = 1:4,
                                  bandnames = names)


plot12<-allrasters_plot$Plot12H

plot12

#RGB

ortho_panel<-ggRGB(plot12, r = 1, g = 2, b = 3, ggLayer = F, coord_equal = T)+
  scale_x_continuous(expand = c(0, 0), breaks = c(145.7352, 145.7360, 145.7368),                      
                     labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-37.5944, -37.5950, -37.5956),
                     labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  labs(x = NULL, y = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        plot.margin = unit(c(0,1,0,0), "cm"),
        aspect.ratio = 1)+ # t, r, b, l
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0001, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_fancy_orienteering)
  
#CHM

CHM_sample<-sampleRegular(plot12$chm, size = 5e5, asRaster = TRUE) %>%
  as.data.frame(xy = T, na.rm = T)%>%rename('CHM' = 3)

chm_panel<-ggplot()+
  geom_raster(data = CHM_sample, aes(x = x, y = y, fill = CHM))+
  scale_fill_viridis(option = 'D')+
  scale_x_continuous(expand = c(0, 0), breaks = c(145.7352, 145.7360, 145.7368),                      
                     labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-37.5944, -37.5950, -37.5956),
                     labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  labs(x = NULL, y = NULL, fill = 'Canopy \nheight (m)')+
  theme_bw()+
  coord_equal(expand = F)+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y =  element_blank(),
        legend.text = element_text(size = 11, face = 'bold'),
        legend.background = element_rect(fill=F),
        aspect.ratio = 1)


ortho_panel + chm_panel + plot_annotation(tag_levels = 'A')

  
ggsave('input.pdf',path = 'figures/', width = 35, height = 15, units = 'cm', dpi = 600)


# polygons and classification ---------------------------------------------

allshapes<-loadshapelist('roi')

allpreds_figures<-loadrasterlist_select('outputs/preds/lowres', 
                                func = raster,
                                bandnames = 'layer')

plot12_pred<-allpreds_figures$Plot12H

plot12_roi<-allshapes$Plot12H

colors <- c('indianred1', 'dodgerblue', 'black')

#plot(plot12_pred, col = colors)

#detail

output_detail<-ggRGB(plot12, r = 1, g = 2, b = 3, ggLayer = F, coord_equal = T)+
  geom_sf(data=plot12_roi, aes(fill = as.factor(lc)), size = 1.2,
          color = 'black', show.legend = F, alpha = 0.8)+
  scale_fill_manual(values = colors, labels = c('Eucalyptus', 'Acacia', 'other'))+
  coord_sf(xlim = c(145.7353, 145.736), ylim = c(-37.595, -37.5944))+
  scale_x_continuous(expand = c(0, 0), breaks = c(145.73534, 145.7356, 145.736),                      
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-37.59445, -37.5947, -37.595),
                     labels = scales::number_format(accuracy = 0.0001,
                                                    decimal.mark = '.')) +
  labs(x = NULL, y = NULL, title = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_blank(),
        plot.margin = unit(c(0,1,0,0), "cm"),
        aspect.ratio = 1)

#main

detail_bbox<-st_as_sfc(st_bbox(extent(145.7353, 145.736, -37.595, -37.5944)))%>%
  st_transform(crs = crs(plot12_pred))


pred_df<-sampleRegular(plot12_pred, size = 5e5, asRaster = TRUE) %>%
  as.data.frame(xy = T, na.rm = T)%>%rename('LC' = 3)

output_class<-ggplot()+
  geom_raster(data = pred_df, aes(x = x, y = y, fill = as.factor(LC)))+
  geom_sf(data=detail_bbox, size = 1.2, color = 'darkblue', fill = NA)+
  scale_fill_manual(values = colors, labels = c('Eucalyptus', 'Acacia', 'other'))+
  scale_x_continuous(expand = c(0, 0), breaks = c(145.7352, 145.7360, 145.7368),                      
                     labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-37.5944, -37.5950, -37.5956),
                     labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  labs(x = NULL, y = NULL, title = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(fill=F),
        legend.text = element_text(size = 11, face = 'bold'),
        aspect.ratio = 1)


output_detail + output_class + plot_annotation(tag_levels = 'A')

ggsave('preds.pdf',path = 'figures/', width = 35, height = 15, units = 'cm', dpi = 600)


# examples ----------------------------------------------------------------

allpreds_plot<-loadrasterlist_select('pointclouds/testsites/testpreds',
                                func = raster,
                                bandnames = 'layer')



metrics_check<-read.csv('outputs/1ha_grid_full/data/testsites_metrics.csv')

max_ac<-allpreds_plot$`405` #18, 75, 6, high agg acacia, high acc euc, high n patches euc, low n patches ac
max_euc<-allpreds_plot$`173` #76, 20, 2 # high agg both, equal n patches

low_ac<-allpreds_plot$`307` #low agg acacia, high patch euc, low patch ac
low_euc<-allpreds_plot$`280` #low agg euc, 

plot(max_ac, col = colors)   #low div
plot(max_euc, col = colors)  #medium div
plot(low_ac, col = colors)  #max div
#plot(low_euc, col = colors)  #max div


max_ac_df<-sampleRegular(max_ac, size = 5e5, asRaster = TRUE) %>%
  as.data.frame(xy = T, na.rm = T)%>%rename('LC' = 3)

max_euc_df<-sampleRegular(max_euc, size = 5e5, asRaster = TRUE) %>%
  as.data.frame(xy = T, na.rm = T)%>%rename('LC' = 3)

low_ac_df<-sampleRegular(low_ac, size = 5e5, asRaster = TRUE) %>%
  as.data.frame(xy = T, na.rm = T)%>%rename('LC' = 3)


low_div<-ggplot()+
  geom_raster(data = max_ac_df, aes(x = x, y = y, fill = as.factor(LC)))+
  scale_fill_manual(values = colors, labels = c('Eucalyptus', 'Acacia', 'other'))+
  scale_x_continuous(expand = c(0, 0),                     
                     labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0),
                     labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  labs(x = NULL, y = NULL, title = NULL)+
  coord_fixed(expand = F)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(fill=F),
        legend.text = element_text(size = 11, face = 'bold'),
        aspect.ratio = 1,
        plot.margin = unit(c(0,1,0,0), "cm"))

med_div<-ggplot()+
  geom_raster(data = max_euc_df, aes(x = x, y = y, fill = as.factor(LC)))+
  scale_fill_manual(values = colors, labels = c('Eucalyptus', 'Acacia', 'other'))+
  scale_x_continuous(expand = c(0, 0),                     
                     labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0),
                     labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  labs(x = NULL, y = NULL, title = NULL)+
  coord_fixed(expand = F)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(fill=F),
        legend.text = element_text(size = 11, face = 'bold'),
        aspect.ratio = 1,
        plot.margin = unit(c(0,1,0,0), "cm"))

high_div<-ggplot()+
  geom_raster(data = low_ac_df, aes(x = x, y = y, fill = as.factor(LC)))+
  scale_fill_manual(values = colors, labels = c('Eucalyptus', 'Acacia', 'other'))+
  scale_x_continuous(expand = c(0, 0),                     
                     labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0),
                     labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = '.')) +
  labs(x = NULL, y = NULL, title = NULL)+
  coord_fixed(expand = F)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = 'bold'), 
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(fill=F),
        legend.text = element_text(size = 11, face = 'bold'),
        aspect.ratio = 1)


low_div + med_div + high_div + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')


ggsave('test.pdf',path = 'figures/', width = 35, height = 15, units = 'cm', dpi = 600)

