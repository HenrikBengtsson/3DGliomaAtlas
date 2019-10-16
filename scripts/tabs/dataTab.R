# UI-elements for data tab

tabPanel(title = "Data", id = "data",
         
  sidebarLayout(
    sidebarPanel(
      
      # User input - select dataset
      selectInput("dataset",
                  label = "Dataset",
                  choices = c("RNA"),
                  selected = "RNA"),
      
      # User input - select patient ID
      uiOutput("patientUI"),
      
      # User input - select tumor
      uiOutput("tumorUI"),
      
      # User input - select gene
      uiOutput("geneUI"),
      
      # Display data selected (at some point we can make this look a bit nicer and add in sample IDs)
      tags$p("Vector:"), # Placeholder
      
      verbatimTextOutput("temp_print_text") # Placeholder
    ),
    
    mainPanel(rglwidgetOutput("model3D"))
  )
  
)
