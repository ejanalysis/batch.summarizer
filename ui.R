library(shiny) # http://shiny.rstudio.com

shinyUI(
  fluidPage(
    
    # http://datatables.net/release-datatables/extensions/FixedColumns/js/dataTables.fixedColumns.js
    
    tagList(
      singleton(tags$head(tags$script(src='//cdn.datatables.net/fixedcolumns/3.0.0/js/dataTables.fixedColumns.js',type='text/javascript'))),
      singleton(tags$head(tags$link(href='//cdn.datatables.net/fixedcolumns/3.0.0/css/dataTables.fixedColumns.css',rel='stylesheet',type='text/css')))
    ),

#     tagList(
#       singleton(tags$head(tags$script(src='//cdn.datatables.net/fixedheader/2.1.2/js/dataTables.fixedHeader.min.js',type='text/javascript'))),
#       singleton(tags$head(tags$link(href='//cdn.datatables.net/fixedheader/2.1.2/css/dataTables.fixedHeader.css',rel='stylesheet',type='text/css')))
#     ),

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
        
        h4('*Still developing this feature - Specify which fields to compare to user-defined thresholds, in up to 3 groups of fields'),
        
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
                           accept=c('text/csv', 'text/txt', '.txt', 'text/comma-separated-values, text/plain', '.csv'))
                 )
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
        tags$hr(),
        h5('Required format for input file:'),
        tags$br(),
        h5('OBJECTID,FACID,NAME,LAT,LON,totpop,buff,stabbr,statename,region,S_E_TSDF_PER,R_P_TRAFFIC,S_E_PM25_PER,R_P_CANCER,S_P_DIESEL,N_D_INDEX,RAW_E_RMP,R_E_PM25,R_D_LESSHS,R_E_DIESEL,RAW_D_OVER64,N_E_TSDF,R_E_LEAD_PER,R_E_RMP_PER,S_E_DIESEL_PER,RAW_E_RESP,R_D_INDEX_PER,RAW_D_LESSHS,N_E_TRAFFIC,S_E_NEURO_PER,N_P_NPL,S_D_INDEX,S_D_MINOR,S_D_LESSHS,S_P_RESP,N_E_PM25_PER,RAW_D_INDEX,N_E_NEURO_PER,RAW_D_UNDER5,RAW_E_LEAD,R_E_NPL_PER,S_E_RESP_PER,S_E_O3_PER,N_P_PM25,S_D_LESSHS_PER,N_E_DIESEL_PER,S_D_INCOME_PER,RAW_E_NPL,R_D_MINOR_PER,S_E_TRAFFIC,R_P_TSDF,RAW_E_TSDF,N_P_CANCER,RAW_E_NEURO,S_E_DIESEL,RAW_D_INCOME,N_P_RMP,N_E_O3_PER,S_E_O3,R_E_RESP,S_E_RESP,N_E_DIESEL,N_D_INDEX_PER,N_E_RMP_PER,RAW_D_MINOR,N_E_CANCER_PER,R_E_O3_PER,S_D_INDEX_PER,N_E_RMP,R_P_LEAD,R_E_NEURO,N_E_LEAD,S_E_RMP_PER,R_E_RMP,RAW_E_DIESEL,R_D_LING_PER,R_E_TRAFFIC,R_E_LEAD,R_D_OVER64_PER,N_P_NEURO,R_E_CANCER_PER,R_E_NPDES_PER,N_E_CANCER,N_D_MINOR_PER,S_E_TSDF,S_E_NPL,R_D_OVER64,S_D_MINOR_PER,S_P_TSDF,S_P_RMP,N_E_PM25,R_E_TSDF,S_E_RMP,RAW_D_LING,S_E_TRAFFIC_PER,S_P_PM25,S_E_LEAD,R_P_NEURO,S_D_LING,N_E_NPL,R_E_DIESEL_PER,R_D_LESSHS_PER,R_P_O3,N_E_TRAFFIC_PER,RAW_E_NPDES,N_E_NPDES,N_E_NEURO,R_P_DIESEL,N_E_RESP_PER,R_E_TSDF_PER,RAW_E_TRAFFIC,R_D_INDEX,R_P_PM25,N_D_UNDER5_PER,N_D_LESSHS_PER,R_E_NPDES,N_D_LING,S_E_PM25,N_E_NPL_PER,R_E_NEURO_PER,R_D_MINOR,N_P_TSDF,S_D_LING_PER,R_P_NPL,S_P_NPDES,S_E_NPDES_PER,N_D_UNDER5,S_E_NPL_PER,S_E_CANCER_PER,N_E_RESP,N_D_LESSHS,S_D_UNDER5,N_P_LEAD,RAW_E_CANCER,S_P_TRAFFIC,N_E_NPDES_PER,R_E_TRAFFIC_PER,N_P_NPDES,RAW_E_O3,N_P_O3,R_E_O3,N_E_O3,N_E_TSDF_PER,R_E_RESP_PER,S_D_OVER64,N_D_INCOME,R_E_NPL,R_D_UNDER5_PER,R_P_RESP,R_P_NPDES,S_P_O3,N_P_DIESEL,N_D_OVER64_PER,R_P_RMP,N_P_TRAFFIC,N_E_LEAD_PER,S_E_NPDES,S_D_OVER64_PER,S_P_NPL,N_D_MINOR,RAW_E_PM25,N_D_LING_PER,S_D_INCOME,S_P_NEURO,N_P_RESP,N_D_OVER64,S_D_UNDER5_PER,R_D_LING,R_E_CANCER,S_E_CANCER,S_P_CANCER,N_D_INCOME_PER,R_D_INCOME_PER,S_E_NEURO,R_D_INCOME,R_E_PM25_PER,R_D_UNDER5,S_E_LEAD_PER,S_P_LEAD'),
        h5('1,1000,BRASW Facility,32.4,-94.7,"7,946",3 miles,TX, Texas,6,97,74,44,75,61,35%,1.2,9.44,19%,0.733,13%,0.054,74,92,37,1.5,72,23%,110,99,95,47%,55%,20%,66,27,61%,92,7%,0.27,97,54,51,77,64,49,70,0.29,69,91,96,0.33,80,0.12,0.913,52%,95,34,42.9,1.4,1.5,0.824,83,95,69%,72,47,68,0.31,84,0.043,0.3,91,0.42,0.478,66,81,0.18,63,90,87,73,49,79,0.073,0.067,11%,62,94,86,10.7,0.062,0.47,6%,60,61,0.17,92,9%,0.096,50,67,68,60,0.32,0.25,0.063,70,43,97,55,44%,68,62,78,0.35,5%,9.63,94,99,49%,97,56,95,74,71,7%,97,84,2.3,15%,8%,84,56,69,81,64,89,43.8,79,43.6,46.3,98,63,10%,34%,0.063,52,73,79,62,78,55,89,80,57,0.38,69,93,36%,9.44,74,39%,89,76,13%,49,7%,42,44,68,79,71,0.044,39%,47,7%,76,82')
        
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
        
        "Exec sum", 
        
        br(),
        h4('Executive Summary'),
        tags$hr(),
        h4(textOutput('popsitecounts.out2')),
        fluidRow(
          column(
            4,
            wellPanel(
              h4('Demographic:'),
              br(),
              h5(textOutput('execsum1')),
              # br(),
              h5(textOutput('execsum2')),
              br(),
              h5(textOutput('execsum3')),
              numericInput('execsum.threshold.d', 'Percentile threshold:', 95, min=0,max=100,step=5),
              br(),
              h5(textOutput('execsum4'))
              
            )
          ),
          column(
            4,
            wellPanel(
              h4('Environmental:'),
              br(),
              h5(textOutput('execsum5'))
            )
          ),
          column(
            4,
            wellPanel(
              h4('EJ:'),
              br(),
              h5(textOutput('execsum6')),
              numericInput('execsum.threshold', 'Percentile threshold:', 95, min=0,max=100,step=5)
            ),
            h6('** Note: More precisely, they reside in buffer zones near these sites where the average person in the buffer zone has a block group indicator value that is at/above the block group values of the specified % of the US population.')
          )
        )
      ), 
      
      ####################################################################################
      tabPanel(
        
        "States", 
        
        br(),
        h4(textOutput('popsitecounts.out')),
        tags$hr(),
        fluidRow(
          column(
            5,
            dataTableOutput('counts.by.state.out')
          ),
          column(
            5,
           dataTableOutput('counts.by.region.out')
          )
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
      )                       # , 
     
     ####################################################################################
     #      tabPanel(
     #        textInput(  "plotly.search",  "Search what to plot",  '' ),
     #        graphOutput("plotly.chart") 
     #      ),
     ####################################################################################
     
     ####################################################################################
     #      tabPanel(
     #        'debug',
     #        verbatimTextOutput('debugginginfo')
     #      )
     ####################################################################################
     
    )
  )
)


