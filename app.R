# Load packages
library(shiny)
library(rgl)
library(shinyRGL)
library(rglwidget)

# Load data
sampleData <- readRDS("data/metadata/sampledata_v7.rds")
tumorData <- readRDS("data/metadata/tumordata_v7.rds")
withRNA <- sort(as.vector(unique(sampleData$Patient[!is.na(sampleData$RNAseq_ColRaw)])))
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
                               selected = withRNA[1])
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
    switch(input$tumor, selectInput("gene", "Gene", choices = rownames(data), selected = rownames(data)[1])
    )
  })
  
  #output$tumor <- renderText({
  #  # Note: couldn't figure out how to read in dataset outside of render function calls
  #  pstr <- gsub('P', 'Patient', input$patient)
  #  data <- readRDS(paste0('data/datasets/', pstr, '/', tolower(input$tumor), '/', tolower(input$dataset), '.rds'))#data has rownames=gene names and colnames=sample names of format PNNNvN
  #  as.character(data[input$gene,])
  #})
  
  output$model3D <- renderWebGL({ #ended with trying to get this to render in the main panel
    pstr <- gsub('P', 'Patient', input$patient)
    sf <- tolower(input$tumor)
    data <- readRDS(paste0('data/datasets/', pstr, '/', tolower(input$tumor), '/', tolower(input$dataset), '.rds'))#data has rownames=gene names and colnames=sample names of format PNNNvN
    vector <- as.numeric(data[input$gene,])#works for RNA, will at some point need to change this to a function that can pull out the vector for any dataset
    names(vector) <- gsub('v','',gsub(pdiddy,'',colnames(data)))
    colors <- colorByFeatureMain(vector)
    plot3DmodelMain(pstr, sf, colors)
    rglwidget()
  })

}

# Run the app
shinyApp(ui, server)

