# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# http://shiny.rstudio.com
library(shiny)

# notes:
# Output function     reactive output created
# htmlOutput	        raw HTML
# imageOutput	        image
# plotOutput	        plot
# tableOutput	        table
# textOutput	        text
# uiOutput	          raw HTML
# verbatimTextOutput	text
#
# You can add output to the user-interface in the same way that you added HTML elements and widgets. 
# Place the output function inside sidebarPanel or mainPanel in the ui.R script.

shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Batch Results Summarizer")),
  
  # Sidebar for selecting functions or columns
  sidebarLayout(
    
    sidebarPanel(
      helpText(h3("Upload Batch")),
      
      fileInput('file1', 'Select file of batch results to upload and summarize',
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
                   '"')
    ),
    
    #       # Possiby add another sidebarPanel here to select mapping file of names to newnames.
    #       # add a sidebarPanel to specify formulas, probs, mythreshold, na.rm, mywts, mycolnames, mythreshnames, etc.?
    
    mainPanel( 
      #h3("View Summary Statistics"),
      tabsetPanel(
        tabPanel("Summary rows", tableOutput("rowsout")),
        tabPanel("Summary cols", tableOutput("colsout")), 
        tabPanel("Plot", plotOutput('barplotdemog')) 
      )
    )
    #h3("Download the Summary")
    
    #verbatimTextOutput("Summary rows "),
    
    
    #tableOutput("rowsout")
    
    #, tableOutput(outlist$cols)
  )
)
)
