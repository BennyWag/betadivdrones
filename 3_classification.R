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

rasterOptions(maxmemory = 2e+10, 
              tmpdir = 'Y:/raster_dump', 
              timer = T, progress = 'text') #for VM only!

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


# reduce resolution -------------------------------------------------------

testsite<-raster('outputs/Plot12H_pred.tif')

plot(testsite, col = colors)

#reduce by fact 2

testsite_2<-aggregate(testsite, 2, fun = min, na.rm = F)

plot(testsite_2, col = colors)
writeRaster(testsite_2, 'outputs/Plot12H_pred_agg2.tif')

#test perf

testsite_perf<-plot_performance(testsite, allshapes$Plot12H)
testsite_perf_agg<-plot_performance(testsite_2, allshapes$Plot12H)

testsite_tss<-testsite_perf$byClass%>%
  as_tibble()%>%rownames_to_column('lc')%>%mutate(TSS = (Sensitivity + Specificity)-1)

testsite_tss_agg<-testsite_perf_agg$byClass%>%
  as_tibble()%>%rownames_to_column('lc')%>%mutate(TSS = (Sensitivity + Specificity)-1)

#reduce by fact 3

testsite_3<-aggregate(testsite, 3, fun = min, expand = F)

plot(testsite_3, col = colors)
writeRaster(testsite_3, 'outputs/Plot12H_pred_agg3.tif', overwrite = T)

testsite_perf_agg3<-plot_performance(testsite_3, allshapes$Plot12H)

testsite_tss_agg3<-testsite_perf_agg3$byClass%>%
  as_tibble()%>%rownames_to_column('lc')%>%mutate(TSS = (Sensitivity + Specificity)-1)
