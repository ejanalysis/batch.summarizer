library(shiny) # http://shiny.rstudio.com

shinyUI(
  fluidPage(

    titlePanel(
      h2(textOutput('titletext')),
      windowTitle = 'Batch Summarizer'
      ),
    
    tabsetPanel(
      
      tabPanel(
        
        "Barplots", 
        
        plotOutput('barplots'),
        downloadButton('download.barplot', 'Download'),
        fluidRow(
          h4('Barplot settings'),
          column(4, radioButtons('bartype', h5('Indicator type'), list('Demographic'='Demographic', 'Environmental'='Environmental','EJ'='EJ'))),
          column(3, radioButtons('barvartype', 'Data Type', list('Percentile of population'='pctile', 'Raw data'='raw'))),
          column(3, radioButtons('barvarmean', 'Statistic', list('Average'='avg', 'Median'='med')))
        )
      ),
      
      tabPanel(
        
        "Upload a batch",
        
        br(),
        #h4(textOutput("name1", container = span)),
        fluidRow(
          column(
            4,
            h4(fileInput('file1', 'Select file of batch results to upload and summarize',
                         accept=c('text/csv', 'text/txt', '.txt', 'text/comma-separated-values, text/plain', '.csv'))),
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
            
            # ***  use dynamic ui to allow user to flexibly specify multiple thresholds in multiple groups,
            # via selectize to specify groups of columns (threshnames list), groups of thresholds (threshold list), etc.
            wellPanel(
              uiOutput('thresholdPICKS', inline=FALSE) 
              ),
            #h5('Selected plus default variables to compare to thresholds:'),
            #textOutput('mythreshnames.toprint'),
            numericInput('threshold1', label='Threshold value(s) for 1st set of comparisons (e.g. %ile 1-100):', value=threshold.default[[1]][1]), 
            numericInput('threshold2', label='Threshold value(s) for 2d set of comparisons (e.g. %ile 1-100):', value=threshold.default[[2]][1]), 
            numericInput('threshold3', label='Threshold value(s) for 3d set of comparisons (e.g. %ile 1-100):', value=threshold.default[[3]][1]), 
            
            checkboxGroupInput('probs','Percentiles of sites & residents to calculate:', choices=probs.default.choices, selected = probs.default.choices)
            #         checkboxInput('header', 'Header', TRUE),
            #         radioButtons('sep', 'Separator',
            #                      c(Comma=',', Semicolon=';', Tab='\t'), ','),
            #         radioButtons('quote', 'Quote',
            #                      c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'), 
          )
        ),
        tags$hr()
      ),
      
      # tabPanel(
      # 
      #         "Sites1",
      #         
      #         dataTableOutput("sitesout"),
      #         h5('Tip: Click a heading (e.g., State) to sort, then Shift-click another column for secondary sort (to sort on 2d within each group in 1st column)'),
      #         h5('Tip: Enter text in the filter box at the bottom of a column to focus on one State or search for one site by name.'),
      #         h5('NOTE: SORTING DOES NOT WORK YET - NUMBERS ARE SORTED AS IF THEY WERE TEXT... TO BE FIXED SOON'),
      #         downloadButton('download.sitesout', 'Download')
      #         #         radioButtons('transpose.rowsout', "Display transposed:", 
      #         #                      c("1 indicator/row, 1 stat or site/column (e.g. to view only cancer stats)" =  TRUE,
      #         #                        "1 indicator/column, 1 site/row (e.g. to sort sites by State, indicator, etc.)" = FALSE))
      #       ),
      
      tabPanel(
        
        "Summary Stats", 
        
        downloadButton('download.rowsout', 'Download'),
        dataTableOutput("rowsout"),
        h5('Tip: Enter text (e.g., EJ, Env, statepctile, etc. for the Type column) in the filter boxes at the bottoms of columns to limit view to certain rows.'),
        h5('Tip: Click a heading (e.g., Type) twice to sort descending, then Shift-click another column (e.g., Average person) twice for descending secondary sort (to sort on 2d col within each group in 1st col)'),
        h5('NOTE: SORTING DOES NOT WORK YET - NUMBERS ARE SORTED AS IF THEY WERE TEXT... TO BE FIXED SOON')
        #         radioButtons('transpose.rowsout', "Display transposed:", 
        #                      c("1 indicator/row, 1 stat or site/column (e.g. to view only cancer stats)" =  TRUE,
        #                        "1 indicator/column, 1 site/row (e.g. to sort sites by State, indicator, etc.)" = FALSE))
      ),
      
      tabPanel(
        
        "Individual Sites", 
        
        downloadButton('download.colsout', 'Download'),
        dataTableOutput("colsout"),
        h5('Tip: Click a heading (e.g., State) to sort, then Shift-click another column for secondary sort (to sort on 2d within each group in 1st column)'),
        h5('Tip: Enter text in the filter box at the bottom of a column to focus on one State or search for one site by name.'),
        h5('NOTE: SORTING DOES NOT WORK YET - NUMBERS ARE SORTED AS IF THEY WERE TEXT... TO BE FIXED SOON')
      ), 
      
      tabPanel(
        
        "Stat Tests", 
        
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
      
      tabPanel(
        
        "Maps", 
        
        h3('US County Map, Census 2010'),
        
        selectInput("mapvar", 
                    label = "Choose a variable to display on map",
                    choices = c("Percent Non-White", "Percent White", "Percent Black", "Percent Hispanic", "Percent Asian"), # names.d.friendly,
                    selected = 1),
        sliderInput("range", 
                    label = "Range of interest:",
                    min = 0, max = 100, value = c(0, 100)),
        
        plotOutput("map")
      )
    )
  )
)


