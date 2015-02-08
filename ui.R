# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# http://shiny.rstudio.com
library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Batch Results Summarizer"),
  
  # Sidebar for selecting functions or columns
  sidebarLayout(
    sidebarPanel(
      
      fileInput('file1', 'Upload Batch Results',
                accept=c('text/csv', 'text/txt', '.txt',
                         'text/comma-separated-values, text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      
      downloadButton('downloadData', 'Download Summary of Batch')
      
    ),
    
    # add another sidebarPanel here to select mapping file of names to newnames?
    
    # add a sidebarPanel to specify formulas, probs, mythreshold, na.rm, mywts, mycolnames, mythreshnames, etc.?
    
    
    
    mainPanel(
      
      #colsout <- outlist$cols
      
      verbatimTextOutput("Summary rows "),
      
      tableOutput(rowsout)
      
      #, tableOutput(outlist$cols)
    )
  )
)
)


