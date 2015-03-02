library(shiny) # http://shiny.rstudio.com

shinyUI(
  fluidPage(

    titlePanel(
      h2(textOutput('titletext')),
      windowTitle = 'Batch Summarizer'
      ),
    
    tabsetPanel(
      
      # tabPanel('test', tableOutput('testout') ),
      id='tabset1',
      
      ####################################################################################
      tabPanel(
        
        "Detailed settings",
        
        h4('Specify which fields to compare to user-defined thresholds, in up to 3 groups of fields'),
        
        # ***  use dynamic ui to allow user to flexibly specify multiple thresholds in multiple groups,
        # via selectize to specify groups of columns (threshnames list), groups of thresholds (threshold list), etc.
        
        column(
          4,
          #h5('Selected plus default variables to compare to thresholds:'),
          #textOutput('mythreshnames.toprint'),
          textInput('threshgroup1', label='Name for 1st set of comparisons', value=threshgroup.default[[1]]),
          numericInput('threshold1', label='Threshold value(s) for 1st set of comparisons (e.g. %ile 1-100):', value=threshold.default[[1]][1]), 
          wellPanel(
            uiOutput('thresholdPICKS1', inline=FALSE) 
          )
        ),
        
        column(
          4,
          textInput('threshgroup2', label='Name for 2d set of comparisons', value=threshgroup.default[[2]]),
          numericInput('threshold2', label='Threshold value(s) for 2d set of comparisons (e.g. %ile 1-100):', value=threshold.default[[2]][1]), 
          wellPanel(
            uiOutput('thresholdPICKS2', inline=FALSE) 
          )
          
          
        ),
        
        column(
          4,
          textInput('threshgroup3', label='Name for 3d set of comparisons', value=threshgroup.default[[3]]),
          numericInput('threshold3', label='Threshold value(s) for 3d set of comparisons (e.g. %ile 1-100):', value=threshold.default[[3]][1]), 
          wellPanel(
            uiOutput('thresholdPICKS3', inline=FALSE) 
          )
        )
        
      ),
      
      ####################################################################################
      tabPanel(
        
        "Upload/Settings",
        
        br(),
        #h4(textOutput("name1", container = span)),
        fluidRow(
          column(
            4,
            wellPanel(
              h4(fileInput('file1', 'Select file of batch results to upload and summarize',
                           accept=c('text/csv', 'text/txt', '.txt', 'text/comma-separated-values, text/plain', '.csv')))              
            ),
            textOutput('sitecount.text2'),
            tags$hr(),
            h5(fileInput('file2', 'Select file of fieldname mapping to rename fields to other than defaults',
                         accept=c('text/csv', 'text/txt', '.txt', 'text/comma-separated-values, text/plain', '.csv')))
          ),
          column(
            4,
            h4(textInput('batchname', "Name this analysis", "Example Dataset"))
          ),
          column(
            4,
            h4('Analysis settings'),
            
            checkboxGroupInput('probs','Percentiles of sites & residents to calculate:', choices=probs.default.choices, 
                               selected = probs.default)
            
            # can add settings here, e.g.
            #         checkboxInput('header', 'Header', TRUE),
            #         radioButtons('sep', 'Separator',
            #                      c(Comma=',', Semicolon=';', Tab='\t'), ','),
            #         radioButtons('quote', 'Quote',
            #                      c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'), 
            
          )
        ),
        tags$hr()
      ),
      
      ####################################################################################
      tabPanel(
        
        'Map of sites',
        
        h4('Map of uploaded sites and circular buffers'),
        tags$hr(),
        leafletOutput('map.sites'),
        tags$hr(),
        fluidRow(
          column(
            4,
            wellPanel(
              textOutput('sitecount.text')
            )
          ),
          column(
            4,
            h5('Hover over large marker for site name.'),
            h5('Click on point for site info.'),
            h5('Zoom in to see buffers around sites.')
          ),
          column(
            4,
            radioButtons('markertype', label='Marker style', choices= list('Large (easier to click but sites overlap)' = 'big', 'Small' = 'small'), 
                         selected = 'big' # default for example dataset which is small set so use big markers
            )
          )
        )
      ),
      
      ####################################################################################
      tabPanel(
        
        "Barplots", 
        
        plotOutput('barplots'),
        downloadButton('download.barplot', 'Download'),
        fluidRow(
          h4('Barplot settings'),
          column(4, radioButtons('bartype', h5('Indicator type'), list('Demographic'='Demographic', 'Environmental'='Environmental','EJ'='EJ'))),
          column(3, radioButtons('barvartype', 'Data Type', list('Percentile of population'='pctile', 'Raw data'='raw'))),
          column(3, radioButtons('barvarmean', 'Statistic', list('Median'='med', 'Average'='avg')))
        )
      ),
      
      ####################################################################################
      tabPanel(
        
        "Summary tables", 
        
        #h4(textOutput("name3", container = span)),
        br(),
        h4('Table 1. Does the population near these sites as a whole have demographics above the US average?'),
        tableOutput("table1"),
        downloadButton('download.table1', 'Download Table 1'),
        tags$hr(),
        h4('Table 2. Does the average site have demographics above the US average?'),
        tableOutput("table2"),
        downloadButton('download.table2', 'Download Table 2'),
        tags$hr(),
        h4('Table 3. Do most of these sites have demographics above the US average?'),
        tableOutput("table3"),
        downloadButton('download.table3', 'Download Table 3')
      ), 
      
     ####################################################################################
      tabPanel(
        
        "Detailed Stats", 
        
        downloadButton('download.rowsout', 'Download'),
        dataTableOutput("rowsout"),
        h5('Tip: Enter text (e.g., Demog, EJ, Env for Category column, and pctile, state, statepctile, etc. for the Type column) in the filter boxes at the bottoms of columns to limit view to certain rows.'),
        h5('Tip: Click a heading (e.g., Type) twice to sort descending, then Shift-click another column (e.g., Average person) twice for descending secondary sort (to sort on 2d col within each group in 1st col)')
        #h5('NOTE: SORTING DOES NOT WORK YET - NUMBERS ARE SORTED AS IF THEY WERE TEXT... TO BE FIXED SOON')
      ),
      
      ####################################################################################
      tabPanel(
        
        "Individual Sites", 
        
        downloadButton('download.colsout', 'Download'),
        dataTableOutput("colsout"),
        h5('Tip: Click a heading (e.g., State) to sort, then Shift-click another column for secondary sort (to sort on 2d within each group in 1st column)'),
        h5('Tip: Enter text in the filter box at the bottom of a column to focus on one State or search for one site by name.')
      ), 
      
      ####################################################################################
      tabPanel(
        
        "Histograms", 
        
        fluidRow(
          column(
            3,
            selectInput('myvar.friendly.base', h5('Indicator'), 
                        c(names.d.friendly, names.e.friendly, names.ej.friendly),
                        selected=1)
          ),
          column(9, plotOutput('histograms') )
        ),
        downloadButton('download.histogram', 'Download'),
        
        # this presumes new variable names are as in default file
        # myvar.base <- 'VSI.eo'  # *** BUT IF IT IS A SUMMARY STAT LIKE ??? this won't work in hist(fulltable[ , myvar]) since it is in outlist()$rows not in fulltable
        
        fluidRow(
          h4("Histogram settings"),
          column(
            2,
            radioButtons('sites.or.people', label=h5('Distribution across sites or people (pop.wtd.)'), list('Sites'='Sites','People'='People') )
          ),
          column(
            2,
            radioButtons('refzone', label=h5('Percentile Zone'), list('US'='us', 'Region'='region', 'State'='state'), select='us')
          ),
          column(
            2, 
            radioButtons('refstat', label=h5('Data type'), list('Percentile of population'='pctile', 'Raw data'='raw'))
          ),
          column(
            2,
            sliderInput('bincount', label=h5('Bins'), 5, 100, step=5, value=10)
          )
        )
      ),
      
      ####################################################################################
      tabPanel(
        
        "Map counties", 
        
        h3('US County Map, Census 2010'),
        
        selectInput("mapvar", 
                    label = "Choose a variable to display on map",
                    choices = c("Percent Non-White", "Percent White", "Percent Black", "Percent Hispanic", "Percent Asian"), # names.d.friendly,
                    selected = 1),
        sliderInput("range", 
                    label = "Range of interest:",
                    min = 0, max = 100, value = c(0, 100)),
        
        plotOutput("map")
      )  # , 
     
      ####################################################################################
      #      tabPanel(
#        textInput(  "plotly.search",  "Search what to plot",  '' ),
#        graphOutput("plotly.chart") 
#      ),
     
####################################################################################
#      tabPanel(
#        'debug',
#        verbatimTextOutput('debugginginfo')
#      )
     
    )
  )
)


