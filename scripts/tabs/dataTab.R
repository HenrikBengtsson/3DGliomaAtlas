# UI-elements for data tab

tabPanel(title = "Data", id = "data",
         
  sidebarLayout(
    sidebarPanel(
      
      selectInput("dataset",
                  label = "Dataset",
                  choices = c("RNA"),
                  selected = "RNA"),
      
      uiOutput("patientUI"),
      
      uiOutput("tumorUI"),
      
      uiOutput("geneUI"),
      
      tags$p("Vector:"), # Placeholder
      
      verbatimTextOutput("temp_print_text") # Placeholder
    ),
    
    mainPanel(plotOutput("map"))
  )
  
)
