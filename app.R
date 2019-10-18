# Load packages
library(shiny)
library(rgl)

# Load data
sampleData <- readRDS("data/metadata/sampledata_v7.rds")
tumorData <- readRDS("data/metadata/tumordata_v7.rds")
withRNA <- sort(as.vector(unique(sampleData$Patient[!is.na(sampleData$RNAseq_ColRaw)])))
withPurity <- sort(as.vector(unique(sampleData$Patient[!is.na(sampleData$FACETS)])))
patientSFNum <- unique(sampleData[,c('Patient','SFNumber')])

# Load functions
source("scripts/display/plot3Dmodel.R", local = TRUE)
source("scripts/display/colorByFeature.R", local = TRUE)

# User interface
ui <- navbarPage("3DGliomaAtlas",
  source("scripts/tabs/dataTab.R", local = TRUE)$value,
  source("scripts/tabs/aboutTab.R", local = TRUE)$value
)

# Server logic
server <- function(input, output){

  output$patientUI <- renderUI({
    if (is.null(input$dataset))
      return()
    
    switch(input$dataset,
           "RNA" = selectInput("patient", "Patient",
                               choices = withRNA,
                               selected = withRNA[2]),
           "Purity" = selectInput("patient", "Patient",
                               choices = withPurity,
                               selected = withPurity[2])
    )
  })
  
  output$tumorUI <- renderUI({
    if (is.null(input$patient))
      return()

    # Note: results in connection issue errors
    sfNums <- patientSFNum[patientSFNum$Patient==input$patient,'SFNumber']
    switch(input$patient, selectInput("tumor", "Tumor", choices = sfNums, selected = sfNums[1])
    )
  })
  
  output$geneUI <- renderUI({
    if (is.null(input$tumor))
      return()
    
    pstr <- gsub('P', 'Patient', input$patient)
    data <- readRDS(paste0('data/datasets/', pstr, '/', tolower(input$tumor), '/', tolower(input$dataset), '.rds'))
    if (input$dataset=='Purity'){ # Don't need to select gene for purity
      return()
    } else {
      switch(input$tumor, selectInput("gene", "Gene", choices = rownames(data), selected = rownames(data)[1]))
    }
  })
  
  output$temp_print_text <- renderText({
    # Note: couldn't figure out how to read in dataset outside of render function calls
    pstr <- gsub('P', 'Patient', input$patient)
    data <- readRDS(paste0('data/datasets/', pstr, '/', tolower(input$tumor), '/', tolower(input$dataset), '.rds'))#data has rownames=gene names and colnames=sample names of format PNNNvN
    if (is.null(dim(data))){ # Handling purity dataset (vector instead of dataframe)
      vector <- as.numeric(data)
    } else { # All other datasets
      vector <- as.numeric(data[input$gene,])
    }
    as.character(vector)
  })
  
  output$model3D <- renderRglwidget({ #ended with trying to get this to render in the main panel
    try(rgl.close(), silent = TRUE)
    pstr <- gsub('P', 'Patient', input$patient)
    sf <- tolower(input$tumor)
    data <- readRDS(paste0('data/datasets/', pstr, '/', tolower(input$tumor), '/', tolower(input$dataset), '.rds'))#data has rownames=gene names and colnames=sample names of format PNNNvN
    if (is.null(dim(data))){ # Handling purity dataset (vector instead of dataframe)
      vector <- as.numeric(data)
      names(vector) <-  gsub('v','',gsub(input$patient,'',names(data)))
    } else { # All other datasets
      vector <- as.numeric(data[input$gene,])
      names(vector) <- gsub('v','',gsub(input$patient,'',colnames(data)))
    }
    colors <- colorByFeatureMain(vector)
    plot3DmodelMain(pstr, sf, colors)
    rglwidget()
  })
}

# Run the app
shinyApp(ui, server)

