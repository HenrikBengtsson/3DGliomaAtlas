library(gtools)

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

# Input to dataset filenames conversion
input_to_filename <- list('rna.rds', 'purity.rds', 'cn.rds', 'per_nec.rds', 'bv_hyper.rds', 'Histology')
names(input_to_filename) <- c('RNA', 'Purity', 'Copy Number', 'Percent Necrosis', 'BV Hyperplasia', 
                              'Histology')