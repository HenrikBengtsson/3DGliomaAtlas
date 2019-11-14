atlas_path <- '/media/sf_ucsf/myCostelloLab/3DGliomaAtlas/'

sample_data <- readRDS(paste0(atlas_path, 'data/metadata/', 'sampledata_v7.rds'))
sample_data$SFNumber <- tolower(sample_data$SFNumber)
sample_data$Sample <- apply(sample_data, 1, function(x) paste0(x['Patient'], x['SampleName']))

tumor_data <- readRDS(paste0(atlas_path, 'data/metadata/', 'tumordata_v7.rds'))

analysis_path <- '/media/sf_ucsf/myCostelloLab/3DGliomaAnalysis/data/'
rna_cpm <- read.table(paste0(analysis_path, 'SID000003_20190529_first_submission.symbol.coding.CPMs.txt'), 
                      sep = '\t', row.names = 1, header = TRUE)
                      
gbm_hist <- read.delim(paste0(analysis_path, '20190903_GBM_histology_radiology_metrics.txt'))
# saveRDS(gbm_hist, paste0(dataset_path, '20190903_GBM_histology_radiology_metrics.rds'))
gbm_hist$SF <- tolower(gbm_hist$SF)
gbm_hist$Sample <- apply(gbm_hist, 1, function(x) paste0(x['Patient'], x['SampleName']))
rownames(gbm_hist) <- gbm_hist$Sample

dataset_path <- paste0(atlas_path, 'data/datasets/')
cn_path <- "/mnt/cluster/data1/shilz/data/copynumber/"

# write.table(rna_cpm, 'rna_cpm.txt', sep = '\t')
# write.table(sample_data, 'sample_data.txt', sep = '\t')

library(stringr)

# Make patient dataset directories and SFNumber subdirectories
patients <- unique(str_extract(sample_data$Patient, 'P[0-9]+'))
patient_dataset_paths <- paste0(dataset_path, gsub('P', 'Patient', patients))
lapply(patient_dataset_paths, function(x) dir.create(x))

for (p in patients){
  sfnums <- unique(sample_data[sample_data$Patient==p, 'SFNumber'])
  for (s in sfnums){
    # print(s)
    dir.create(paste0(dataset_path, gsub('P', 'Patient', p), '/', s))
  }
}

# Map SF numbers to patients
p_to_sf <- vector(mode = 'list', length = length(patients))
names(p_to_sf) <- patients
for (p in names(p_to_sf)){
  sfnums <- unique(sample_data[sample_data$Patient==p, 'SFNumber'])
  p_to_sf[[p]] <- sfnums
}

# Make RNA dataframes for each patient
for (p in names(p_to_sf)){
  for (sf in p_to_sf[[p]]){
    
    patient <- gsub('P', 'Patient', p)
    print(paste(p, sf))
    
    # Create purity vectors
    facets <- sample_data[sample_data$Patient==p & sample_data$SFNumber==sf,'FACETS']
    facets[is.na(facets)] <- 0.1 # Replace NA purities with 0.1 (Estimate for low purity that FACETS can't handle)
    names(facets) <- sample_data[sample_data$Patient==p & sample_data$SFNumber==sf,'Sample']
    purity_file <- paste0(dataset_path, patient, '/', sf, '/purity.rds')
    saveRDS(facets, purity_file)
    
    # Create RNAseq expression dataframes
    rnaseq_id <- sample_data[sample_data$Patient==p & sample_data$SFNumber==sf,'RNAseq_ID']
    if (any(!is.na(rnaseq_id))){
      samples <- paste0(p, sample_data[sample_data$SFNumber==sf,'SampleName'])
      with_rna <- samples[!is.na(rnaseq_id)]
      rna_file <- paste0(dataset_path, patient, '/', sf, '/rna.rds')
      saveRDS(rna_cpm[order(rownames(rna_cpm)), with_rna], rna_file)
    }
    
    # Create CN dataframes
    cn_file <- str_extract(list.files(paste0(cn_path, patient)), '.*TCNByGeneBySegment.rds')
    cn_by_gene <- grep(paste0(patient, '.*TCNByGeneBySegment.rds'), list.files(paste0(cn_path, patient)), value = TRUE)
    if (length(cn_by_gene) > 0){
      cn <- readRDS(paste0(cn_path, patient, '/', cn_by_gene))
      cn <- as.data.frame(cn)
      colnames(cn) <- gsub('Primary', p, colnames(cn))
      colnames(cn) <- gsub('-', '', colnames(cn))
      colnames(cn) <- gsub(paste0('^', p, '$'), paste0(p, 'surg'), colnames(cn))
      rownames(cn) <- gsub('_[0-9]+', '', rownames(cn))
      cn_file <- paste0(dataset_path, patient, '/', sf, '/cn.rds')
      saveRDS(cn[order(rownames(cn)),], cn_file)
    }
    
    # Create histology vectors
    if (p %in% gbm_hist$Patient & sf %in% gbm_hist$SF){
      filtered_hist <- gbm_hist[gbm_hist$Patient==p & gbm_hist$SF==sf,]
      per_nec <- as.vector(t(filtered_hist[, c('Percent.necrosis')]))
      names(per_nec) <- rownames(filtered_hist)
      saveRDS(per_nec, paste0(dataset_path, patient, '/', sf, '/per_nec.rds'))
      bv_hyper <- as.vector(t(filtered_hist[, c('Target.BV.hyperplasia')]))
      names(bv_hyper) <- rownames(filtered_hist)
      saveRDS(bv_hyper, paste0(dataset_path, patient, '/', sf, '/bv_hyper.rds'))
    }
    
  }
}

