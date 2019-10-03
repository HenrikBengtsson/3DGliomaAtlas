# Created: 2019.09.02
# By: Stephanie R Hilz
# Usage: Given a patientID and sf#, plots model of tumor with samples

library(misc3d)
library(rgl)
library(gtools)#might not need, test
library(dplyr)#might not need, test

plotTemplate <- function(tumorModel){
  print('Processing tumor model')
  dtemp <- dim(tumorModel)
  print('Creating tumor contour and plotting tumor')
  tumor <- contour3d(tumorModel, x=1:dtemp[1], y=1:dtemp[2], z=1:dtemp[3], level = 1, alpha = .2, add = FALSE, draw = TRUE, color = 'yellow')
}

plot3dSamples <- function(sampleModels, colors){
  for (name in names(sampleModels)){
    if(!is.na(colors[as.character(name)])){
      sampleColor <- colors[name]
    } else {
      sampleColor <- '#FFFF00'
      print(name)
    }
    x <- sampleModels[[name]]
    print(paste0('Processing sample',name))
    dtemp <- dim(x)
    sample <- contour3d(x, x=1:dtemp[1], y=1:dtemp[2], z=1:dtemp[3], level = 1, alpha = 1, color=sampleColor, add = TRUE, draw = TRUE)
    text3d(which(x == 1, arr.ind=TRUE)[1,], texts = name, cex=1, adj=-.3)
  }
}

plot3DmodelMain <- function(patientID, sf, colors){
  # Specify patient and load the config file for that patient. Config file contains paths to imaging files + ordering of samples + sample names + colors
  modelsPath <- paste0(dataPath, '/3Dmodels/',patientID,'/',sf)
  
  # Retreive sample model files
  sampleModelFiles <- mixedsort(list.files(modelsPath, pattern="sample*"))
  
  # Create names object
  names <- gsub('.rds','',gsub('sample','',sampleModelFiles))
  
  # Read in sample models
  sampleModels <- lapply(paste0(modelsPath, '/', sampleModelFiles), readRDS)#this should be a list of all subdirectories in the main sample dir, each containing dicoms for a single data point
  
  # Name sample models object 
  names(sampleModels) <- names
  
  # Read in tumor model for patient
  tumorModel <- readRDS(paste0(modelsPath, '/tumor_t2.rds'))
  
  # Plot background of brain and tumor
  plotTemplate(tumorModel) 
  
  # Plot samples
  plot3dSamples(sampleModels, colors)
}
