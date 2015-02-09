# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# http://shiny.rstudio.com
library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Batch Results Summarizer")),
  
  # Sidebar for selecting functions or columns
#  sidebarLayout(
#    
#     sidebarPanel(
#     ),
#       # Possiby add another sidebarPanel here to select mapping file of names to newnames.
#       # add a sidebarPanel to specify formulas, probs, mythreshold, na.rm, mywts, mycolnames, mythreshnames, etc.?

#    mainPanel(
#      h3("View Summary Statistics"),
      tabsetPanel(
      
       tabPanel( "Upload a batch",
                  #helpText(h3("Upload Batch")),
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
                               '"'), 
                  tableOutput("fulltableout")),
        
        tabPanel("Summary rows", tableOutput("rowsout")),
        tabPanel("Summary cols", tableOutput("colsout")), 
        tabPanel("Barplots", 
                 plotOutput('barplots')
                 ), 
        tabPanel("Histograms", 
                 plotOutput('histograms'),

#                 checkboxes for selection of type of histogram
#                 # this presumes new variable names are as in default file

#                  myvar.base <- 'VSI.eo'  # *** BUT IF IT IS A SUMMARY STAT LIKE ??? this won't work in hist(fulltable[ , myvar]) since it is in outlist$rows not in fulltable

                 fluidRow(
                   h4("Histogram settings"),
                   
                   # *** WILL EXPLAND THIS TO THE FULL GENERIC LIST OF INDICATORS THAT COULD BE PLOTTED HERE:
                   selectInput('myvar.base', h5('Indicator'), c('VSI.eo','pctmin','pctlowinc','traffic.score','EJ.DISPARITY.traffic.score.eo'), selected=1),
                   
                   column(3,
                          radioButtons('sites.or.people', label=h5('Distribution across sites or people (pop.wtd.)'), list('Sites'='Sites','People'='People') ),
                          br()
                   ),
                   column(2,
                          radioButtons('refzone', label=h5('Percentile Zone'), list('US'='us', 'Region'='region', 'State'='state'), select='us')
                   ),
                   column(2, 
                          radioButtons('refstat', label=h5('Data type'), list('Percentile of population'='pctile', 'Raw data'='raw'))
                   ),
                   column(2,
                          sliderInput('bincount', label=h5('Bins'), 5, 100, step=5, value=10)
                   )
                 )
        ) 
      )
    )

#h3("Download the Summary")

    #, tableOutput(outlist$cols)
  )


# notes:
# Output function     reactive output created
# htmlOutput          raw HTML
# imageOutput          image
# plotOutput	        plot
# tableOutput	        table
# textOutput	        text
# uiOutput	          raw HTML
# verbatimTextOutput	text
