# UI-elements for data tab

tabPanel(title = "Data", id = "data",
         
  sidebarLayout(
    sidebarPanel(
      
      # User input - select dataset
      selectInput("dataset",
                  label = "Dataset",
                  choices = c("RNA", "Purity", "Copy Number", "Amplification", "Histology"), 
                  selected = "RNA"),
      # CN instead of "Copy Number" bc of rds naming convention (need to change later)
      
      # User input - select type ID (type of histological data)
      uiOutput("typeUI"),
            
      # User input - select patient ID
      uiOutput("patientUI"),
      
      # User input - select tumor
      uiOutput("tumorUI"),
      
      # User input - select copy number threshold
      uiOutput("thresholdUI"),
      
      # User input - select gene
      uiOutput("geneUI"),
      
      # Display data selected (at some point we can make this look a bit nicer and add in sample IDs)
      tags$p("Vector:"), # Placeholder
      
      verbatimTextOutput("temp_print_text") # Placeholder
    ),
    
    mainPanel(rglwidgetOutput("model3D"))
  )
  
)
