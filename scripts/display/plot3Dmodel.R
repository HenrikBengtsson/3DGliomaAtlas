# Created: 2019.09.02
# By: Stephanie R Hilz
# Usage: Given a patientID and sf#, plots model of tumor with samples

library(misc3d)
library(rgl)
library(gtools)#might not need, test
library(dplyr)#might not need, test

plotTemplate <- function(tumorModel, brainModel){
  dtemp <- dim(tumorModel)
  print('Creating brain contour and plotting brain')
  brain <- contour3d(brainModel, x=1:dtemp[1], y=1:dtemp[2], z=1:dtemp[3], level = 1, alpha = .03, add = FALSE, draw = TRUE, color = '#B08883')
  print('Creating tumor contour and plotting tumor')
  tumor <- contour3d(tumorModel, x=1:dtemp[1], y=1:dtemp[2], z=1:dtemp[3], level = 1, alpha = .2, add = TRUE, draw = TRUE, color = 'yellow')
}

plot3dSamples <- function(sampleCoordinates, colors){
  sampleColors <- c()
  for (name in rownames(sampleCoordinates)){
    if(!is.na(colors[as.character(name)])){
      singleColor <- colors[name]
    } else {
      singleColor <- '#a7a457'
      print(name)
    }
    sampleColors <- append(sampleColors, singleColor)
  }
  points3d(x=sampleCoordinates[,1], y=sampleCoordinates[,2], z=sampleCoordinates[,3], level = 1, size = 7, color=sampleColors)
  text3d(x=sampleCoordinates[,1], y=sampleCoordinates[,2], z=sampleCoordinates[,3], texts = rownames(sampleCoordinates), cex=1, adj=-.3)
}


plot3DmodelMain <- function(patientID, sf, colors){
  # Specify patient and load the config file for that patient. Config file contains paths to imaging files + ordering of samples + sample names + colors
  modelsPath <- paste0('data/models/',patientID,'/',sf)
  
  # Read in sample models
  sampleCoordinates <- readRDS(paste0(modelsPath, '/coordinates_samples.rds'))
  
  # Read in tumor model for patient
  tumorModel <- readRDS(paste0(modelsPath, '/tumor_t2.rds'))
  
  # Read in brain model for patient
  brainModel <- readRDS(paste0(modelsPath, '/brain.rds'))
  
  # Plot background of brain and tumor
  plotTemplate(tumorModel, brainModel) 
  
  # Plot samples
  plot3dSamples(sampleCoordinates, colors)
}
