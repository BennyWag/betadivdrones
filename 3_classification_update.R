# supervised classification -----------------------------------------------

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

extract_data<-function(raster, mask){
  
  lc<-exact_extract(raster, mask, fun = NULL)
  
  for( i in seq_along(lc)){
    
    lc[[i]]$ID <- seq_along(lc)[i]
    
  }
  
  table<-dplyr::bind_rows(lc)%>%dplyr::select(-5)
  
  table$lc <- as.factor(mask$lc[table$ID])
  
  return(as.tibble(table))
}

extract_varimp<-function(model, title = NULL){
  
  var.imp <- data.frame(importance(model, type=1))
  var.imp$Variables <- row.names(var.imp)%>%as.factor()
  rownames(var.imp)<-1:nrow(var.imp)
  varimp <- var.imp[order(var.imp$MeanDecreaseAccuracy,decreasing = T),]
  
  var.imp2 <- data.frame(importance(model, type=2))
  var.imp2$Variables <- row.names(var.imp2)%>%as.factor()
  rownames(var.imp2)<-1:nrow(var.imp2)
  varimp2 <- var.imp2[order(var.imp2$MeanDecreaseGini,decreasing = T),]
  
  imp<-ggplot(varimp, aes(x = reorder(Variables,MeanDecreaseAccuracy), y = MeanDecreaseAccuracy))+
    geom_bar(stat = "summary", fun.y = 'mean', fill = 'darkblue')+
    coord_flip()+
    ylab('Mean decrease Accuracy')+xlab('')+
    ggtitle(title)+
    geom_text(aes(label=round(MeanDecreaseAccuracy, digits = 3)), position=position_dodge(width=0.9), hjust=1, color = 'white')+
    theme_bw()+
    theme(plot.title = element_text(size =18, face='bold'),
          axis.title.y = element_text(size = 16, face = 'bold'),
          axis.title.x = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 15, face = 'bold'), 
          axis.text.y = element_text(size = 15, face = 'bold'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 15))
  
  imp2<-ggplot(varimp2, aes(x = reorder(Variables,MeanDecreaseGini), y = MeanDecreaseGini))+
    geom_bar(stat = "summary", fun.y = 'mean', fill = 'darkblue')+
    coord_flip()+
    ylab('Mean decrease Gini')+xlab('')+
    ggtitle(title)+
    geom_text(aes(label=round(MeanDecreaseGini, digits = 3)), position=position_dodge(width=0.9), hjust=1, color = 'white')+
    theme_bw()+
    theme(plot.title = element_text(size =18, face='bold'),
          axis.title.y = element_text(size = 16, face = 'bold'),
          axis.title.x = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 15, face = 'bold'), 
          axis.text.y = element_text(size = 15, face = 'bold'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 15))
  
  plot<- imp / imp2
  
  return(plot)
  
}

plot_performance<-function(pred_raster, mask){
  
  test <- exact_extract(pred_raster, mask, fun = NULL) 
  
  for( i in seq_along(test)){
    
    test[[i]]$ID <- seq_along(test)[i]
    
  }
  
  test_table<-dplyr::bind_rows(test)%>%dplyr::select(-2)
  
  test_table$lc <- as.factor(mask$lc[test_table$ID])
  
  
  testProbs <- data.frame(
    obs = as.factor(test_table$lc),
    pred = as.factor(test_table$value))
  
  confMatrix <- confusionMatrix(testProbs$obs, testProbs$pred)
  confMatrix
  
  return(confMatrix)
}

saverasterlist<-function(list, outdir){
  
  x<-getwd()
  
  setwd(outdir)
  
  mapply(writeRaster, list, names(list), bylayer=F, 'GTiff', options="COMPRESS=LZW")
  
  setwd(x)
  
  gc()
  memory.size(max=F)
  
}

names<-c('red', 'green', 'blue', 'chm')

colors <- c('darkgreen', 'blue', 'tomato4')

rasterOptions(progress = 'text')

# rasterOptions(maxmemory = 2e+10, 
#               tmpdir = 'Y:/raster_dump', 
#               timer = T, progress = 'text') #for VM only!

# load data ---------------------------------------------------------------

allshapes<-loadshapelist('roi')

names<-c('red', 'green', 'blue', 'chm')

allrasters<-loadrasterlist_select('outputs/rgb+chm', 
                                  func = stack, 
                                  bands = 1:4,
                                  bandnames = names)


# compile modeling data ---------------------------------------------------

all_data<-map2(allrasters, allshapes, extract_data)

#combine to dataframe

all_data_df<-bind_rows(all_data, .id = 'plot')

#save

#write.csv(all_data_df, 'outputs/modeling_data_all.csv', row.names = F)

#subset


check<-all_data_df%>%group_by(plot, as.factor(lc))%>%count(lc) #optional

all_data_df_sample<-all_data_df%>%group_by(plot, as.factor(lc))%>%sample_n(3000, replace = F) #optional

write.csv(all_data_df_sample, 'outputs/modeling_data_sample.csv', row.names = F)


# subset and model --------------------------------------------------------

all_data_df_sample<-read.csv('outputs/modeling_data_sample.csv', stringsAsFactors = T)

#remove plots 10M and H

model_subset<-all_data_df_sample%>%filter(!plot %in% c('Plot10M', 'Plot10H'))%>%
  na.omit()%>%group_by(plot, lc)%>%sample_n(1500)

summary(as.factor(model_subset$lc))

summary(as.factor(model_subset$plot)) #needs to be equal

dataset_modeling<-model_subset%>%ungroup()%>%dplyr::select(2:5,7)

#subset train/test

train <- createDataPartition(dataset_modeling$lc, time=1, p = 0.8, list=F)
train_subset  <- dataset_modeling[train,]
test_subset  <- dataset_modeling[-train,]

#build model

model<-randomForest(lc ~ ., data=train_subset, ntree = 500, importance=TRUE)

model
plot(model)
varImpPlot(model)

extract_varimp(model)

# validation --------------------------------------------------------------

test_subset$pred<-predict(model, test_subset)

confMatrix_model <- confusionMatrix(as.factor(test_subset$lc), test_subset$pred)

confMatrix_model

confMatrix_model$table

stats<-confMatrix_model[["byClass"]]%>%as_tibble()%>%rownames_to_column('lc')
stats_overall<-confMatrix_model[["overall"]]%>%as.data.frame()%>%rownames_to_column()%>%rename(value = 2, metric = 1)

stats<-stats%>%mutate(TSS = (Sensitivity + Specificity)-1)

# spatial prediction ------------------------------------------------------

#masking option

chm<-allrasters$Plot11L$chm
plot(chm)
chm_re<-raster::reclassify(chm, c(-Inf,0,NA,0,Inf,1))
plot(chm_re)

plot<-allrasters$Plot11L
plot_mask<-mask(plot, chm_re)


#pred to one

beginCluster()
pred<-clusterR(plot, raster::predict, 
               args = list(model = model, inf.rm = T), 
               progress = 'text')
endCluster()

plot(pred, col = colors)

#validate

pred_val<-plot_performance(pred, allshapes$Plot11L)

pred_val$table

#save

writeRaster(pred, 'outputs/Plot11L_pred.tif')

#pred all (run on server!)

all_predict<-list()

for (i in allrasters){
  
  beginCluster()
  pred<-clusterR(i, raster::predict, 
                 args = list(model = model, inf.rm = T), 
                 progress = 'text')
  endCluster()
  
  all_predict[[length(all_predict)+1]] <- pred
  
}

names(all_predict)<-names(allrasters)

#save original

#dir.create('outputs/preds')

saverasterlist(all_predict, 'outputs/preds')

#reduce res

aggregate(testsite, 2, fun = min, na.rm = F)

all_predict_agg<-map(all_predict, aggregate, 2, fun = min, na.rm = F)

#dir.create('outputs/preds/lowres')

saverasterlist(all_predict_agg, 'outputs/preds/lowres')


# get landscapemetrics ----------------------------------------------------

allpreds<-loadrasterlist_select('outputs/preds/lowres', 
                                func = raster,
                                bandnames = 'layer')

plot(allpreds$Plot10H, col = colors)

#function

raster::area(allpreds$Plot10H)

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

#test

pred_test<-raster_stats(allpreds$Plot12H)

#get all

metrics_plots<-lapply(allpreds, raster_stats)

#combine

metrics_plots_bind<-bind_rows(metrics_plots, .id = 'plot')

#save

#dir.create('outputs/preds/data')

write.csv(metrics_plots_bind, 'outputs/preds/data/plot_metrics.csv')


# predict biodiv ----------------------------------------------------------

plot_data<-read.csv('data/plot_data.csv')%>%rename(plot = 1)%>%
  mutate(plot = toupper(plot),
         plot = paste0('Plot', plot))%>%
  filter(!plot %in% c('Plot10M'))

drone_data<-read.csv('outputs/trees_tally.csv')

#make metrics data long and combine

metrics_wide<-pivot_wider(metrics_plots_bind, names_from = 2, values_from = 5:7)%>%
  filter(!plot %in% c('Plot10M'))%>%
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
  select(1,4:12)%>%
  mutate_at(c(2:10), ~(log(.) %>% as.vector)) #log or scale?

#not transformed

metrics_wide_raw<-pivot_wider(metrics_plots_bind, names_from = 2, values_from = 5:7)%>%
  filter(!plot %in% c('Plot10M'))%>%
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

#combine

model_data<-left_join(metrics_wide, plot_data%>%
                        select(plot, jaccard_biodiversity), by = 'plot')

model_data<-left_join(model_data, drone_data, by = 'plot')

write.csv(model_data, 'outputs/model_data.csv', row.names = F)

#raw

model_data_raw<-left_join(metrics_wide_raw, plot_data%>%
                        select(plot, jaccard_biodiversity), by = 'plot')

model_data_raw<-left_join(model_data_raw, drone_data, by = 'plot')

write.csv(model_data_raw, 'outputs/model_data_raw.csv', row.names = F)

#reduce for moleding

model_data_red<-model_data%>%select(2:11, 24:27)

#model

div_mod<-glm(jaccard_biodiversity~., family = 'gaussian', 
             data = model_data_red, na.action = na.fail)

div_dredge<-dredge(div_mod, trace = T)
summary(get.models(div_dredge, 1)[[1]])
summary(get.models(div_dredge, 2)[[1]])


