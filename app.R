# Load packages
library(shiny)

# Load data
sampleData <- readRDS("data/metadata/sampledata_v7.rds")
tumorData <- readRDS("data/metadata/tumordata_v7.rds")
withRNA <- sort(as.vector(unique(sampleData$Patient[!is.na(sampleData$RNAseq_ColRaw)])))
patientSFNum <- unique(sampleData[,c('Patient','SFNumber')])


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

    sfNums <- patientSFNum[patientSFNum$Patient==input$patient,'SFNumber']
    switch(input$patient, selectInput("tumor", "Tumor", choices = sfNums, selected = sfNums[1])
    )
  })
  
  output$geneUI <- renderUI({
    if (is.null(input$tumor))
      return()
    
    pstr <- gsub('P', 'Patient', input$patient)
    # Note: results in connection issue errors for some reason
    data <- readRDS(paste0('data/datasets/', pstr, '/', tolower(input$tumor), '/', tolower(input$dataset), '.rds'))
    switch(input$tumor, selectInput("gene", "Gene", choices = rownames(data), selected = rownames(data)[1])
    )
  })
  
  output$temp_print_text <- renderText({
    as.character(data[input$gene,])
  })

}

# Run the app
shinyApp(ui, server)