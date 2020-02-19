library(gtools)

# Input to dataset filenames conversion
input_to_filename <- list('rna.rds', 'purity.rds', 'cn.rds', 'cn.rds', 'per_nec.rds', 'bv_hyper.rds', 'Histology')
names(input_to_filename) <- c('RNAseq', 'Tumor Cell Proportion', 'Gene Copy Number', 'Gene Amplification', 'Percent Necrosis', 'BV Hyperplasia', 
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