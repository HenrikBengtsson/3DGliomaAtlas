library(gtools)

# Input to dataset filenames conversion
input_to_filename <- list('rna.rds', 'purity.rds', 'cn.rds', 'cn.rds', 'per_nec.rds', 'bv_hyper.rds', 'Histology')
names(input_to_filename) <- c('RNAseq', 'Tumor Cell Proportion', 'Copy Number', 'Amplification', 'Percent Necrosis', 'BV Hyperplasia', 
                              'Histology') # Both Copy Number & Amplification read from cn.rds file

getDatasets <- function(datasets_path){
  #' Creates dataframe containing the datasets that exist for each tumor
  tumor_datasets <- data.frame(patient = c(), sf = c())
  patients <- list.files(paste0(datasets_path))
  for (p in patients){
    sfnums <- list.files(paste0(datasets_path, p))
    for (sf in sfnums){
      datasets <- list.files(paste0(datasets_path, p, '/', sf))
      temp <- data.frame(patient = c(p), sf = c(sf))
      temp2 <- data.frame(t(rep(1, length(datasets))))
      colnames(temp2) <- datasets
      tumor_datasets <- smartbind(tumor_datasets, cbind(temp, temp2))
    }
  }
  return(tumor_datasets)
}

cn_to_amp <- function(cn_df, threshold){
  # Binarizes copy number matrix, if TCN > threshold 
  amp_df <- apply(cn_df, c(1, 2), function(x) ifelse(x > threshold, 1, 0))
  return(amp_df)
}

getDataValues <- function(patient, tumor, dataset, type, gene, threshold, conversion){
  input_to_filename <- 
  if (dataset=="Histology"){
    if (type=='BV Hyperplasia'){
      fname <- 'bv_hyper.rds'
    } else {
      fname <- 'per_nec.rds'
    }
  } else {
    fname <-  names(conversion[which(conversion==dataset)])
  }
  data <- readRDS(paste0('data/datasets/', patient, '/', tumor, '/', fname))#data has rownames=gene names and colnames=sample names of format PNNNvN
  if (dataset=='Amplification'){
    data <- cn_to_amp(data, threshold)
  }
  if (is.null(dim(data))){ # Handling purity & histology datasets (vector instead of dataframe)
    vector <- as.numeric(data)
  } else { # All other datasets
    vector <- as.numeric(data[gene,])
  }
  names(vector) <- gsub('P[0-9]{3}v', '', names(data))
  return(vector)
}