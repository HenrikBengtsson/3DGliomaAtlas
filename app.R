# Load packages
library(shiny)
library(rgl)

# Load functions
source("scripts/display/plot3Dmodel.R", local = TRUE)
source("scripts/display/colorByFeature.R", local = TRUE)
source("scripts/processing/processing.R", local = TRUE)

# Load data
sampleData <- readRDS("data/metadata/sampledata_v8.rds")
tumorData <- readRDS("data/metadata/tumordata_v8.rds")
tumorDatasets <- getDatasets("data/datasets/")

# User interface
ui <- navbarPage("3DGliomaAtlas",
  source("scripts/tabs/dataTab.R", local = TRUE)$value,
  source("scripts/tabs/aboutTab.R", local = TRUE)$value,
  options(shiny.sanitize.errors = TRUE),
  tags$head(tags$style(type="text/css",".shiny-output-error{visibility: hidden; }")),
  tags$head(tags$style(".shiny-output-error:before{content: 'Loading 3D model...';visibility: visible; }"))
)

# Server logic
server <- function(input, output){
  
  output$tumorUI <- renderUI({
    if (is.null(input$patient))
      return()
    
    sfNums <- tumorDatasets[tumorDatasets$patient==input$patient, 'sf']
    switch(input$patient, selectInput("tumor", "Tumor", choices = sfNums, selected = sfNums[1]))
  })
  
  output$datasetUI <- renderUI({
    datasetConversion <- c(cn.rds='Copy Number', purity.rds='Tumor Cell Proportion', rna.rds='RNAseq', by_hyper.rds='Histology', per_nec.rds='Histology')
    availableDatasets <- as.character(datasetConversion[colnames(tumorDatasets[which(tumorDatasets[which(tumorDatasets$patient==input$patient),]==1)])])
    switch(input$patient, selectInput("dataset", "Dataset", choices = availableDatasets))
  })
  
  output$typeUI <- renderUI({
    if (input$dataset!="Histology")
      return()
    
    switch(input$dataset,
           "Histology" = selectInput("type", "Type", choice = c("Percent Necrosis", "BV Hyperplasia"), 
                                     selected = "Percent Necrosis")
    )
  })
  
  output$thresholdUI <- renderUI({
    if (input$dataset!="Amplification")
      return()
    
    switch(input$dataset, sliderInput("threshold", "Threshold", min = 0, max = 15, value = 5, step = 0.1)
    )
  })
  
  output$geneUI <- renderUI({
    if (is.null(input$tumor))
      return()
    
    if (input$dataset=="Histology"){
      fname <- input_to_filename[input$type]
    } else {
      fname <- input_to_filename[input$dataset]
    }
    data <- readRDS(paste0('data/datasets/', input$patient, '/', input$tumor, '/', fname))
    if (input$dataset %in% c('Tumor Cell Proportion', 'Histology')){ # Don't need to select gene for purity or histology
      return()
    } else {
      switch(input$tumor, selectInput("gene", "Gene", choices = rownames(data), selected = rownames(data)[1]))
    }
  })
  
  output$temp_print_text <- renderText({
    if (input$dataset=="Histology"){
      fname <- names(datasetConversion[which(datasetConversion==input$type)])
    } else {
      fname <- names(datasetConversion[which(datasetConversion==input$dataset)])
    }
    data <- readRDS(paste0('data/datasets/', input$patient, '/', input$tumor, '/', fname))#data has rownames=gene names and colnames=sample names of format PNNNvN
    if (input$dataset=='Amplification'){
      data <- cn_to_amp(data, input$threshold)
    }
    if (is.null(dim(data))){ # Handling purity & histology datasets (vector instead of dataframe)
      vector <- as.numeric(data)
    } else { # All other datasets
      vector <- as.numeric(data[input$gene,])
    }
    as.character(vector)
  })
  
  output$model3D <- renderRglwidget({ #ended with trying to get this to render in the main panel
    try(rgl.close(), silent = TRUE)
    if (input$dataset=="Histology"){
      fname <- input_to_filename[input$type]
    } else {
      fname <- input_to_filename[input$dataset]
    }
    data <- readRDS(paste0('data/datasets/', input$patient, '/', input$tumor, '/', fname))#data has rownames=gene names and colnames=sample names of format PNNNvN
    if (input$dataset=='Amplification'){
      data <- cn_to_amp(data, input$threshold)
    }
    if (is.null(dim(data))){ # Handling purity & histology datasets (vector instead of dataframe)
      vector <- as.numeric(data)
    } else { # All other datasets
      vector <- as.numeric(data[input$gene,])
    }
    names(vector) <- gsub('P[0-9]{3}v', '', names(data))
    colors <- colorByFeatureMain(vector)
    plot3DmodelMain(input$patient, input$tumor, colors)
    rglwidget()
  })
}

# Run the app
shinyApp(ui, server)

