library(shiny) # http://shiny.rstudio.com

#############################
# DEFAULT VALUES, possibly could recode to allow user to change them, and/or read fieldnames from the csv file: 
#############################
probs.default <-         c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1)  # defaults for quantiles summary stats
probs.default.choices <- c('0','0.25','0.50','0.75','0.80','0.90','0.95','0.99','1.00')  # defaults for quantiles summary stats
#mythreshold=80  # default for cutoff in at/above threshold stat summarizing the EJ Index US percentiles
na.rm=TRUE  # default for all functions so can get stats even if one site (or one indicator at one site) has no data
#
mydemofile <- 'Export_Output_Example2.txt' # example of export of batch results, for use in testing/demonstrating summarizer
mynamesfile <- 'map batch to friendly fieldnames v1.csv' # has default input and output and friendly fieldnames & var type & var category
# *** SPECIFY MORE PARAMETERS - USE DEFAULTS FOR NOW, POSSIBLY RECODE LATER TO LET USER CHANGE THESE
# default Demog vars for now:
mywtsname <- 'pop'  # used for weighted means to get stats on the average person in all these zones (e.g. avg person nearby any site)
names.d          <- c('VSI.eo', 'pctlowinc', 'pctmin', 'pctlths', 'pctlingiso', 'pctunder5', 'pctover64') 
names.d.friendly <- c('Demog.Ind.', '% Low-inc.', '% Minority', '% <High School', '% Linguistic Isol.', '% < age 5', '% > age 64')
# default Envt vars for now:
names.e          <- c("pm", "o3", "cancer", "neuro", "resp", "dpm", "pctpre1960", 
                      "traffic.score", "proximity.npl", "proximity.rmp", "proximity.tsdf", 
                      "proximity.npdes")
names.e.friendly <- c("PM2.5", "Ozone", "Cancer risk", "Neuro.", "Respiratory", "Diesel PM", "% built pre-1960", 
                      "Traffic", "NPL proximity", "RMP proximity", "TSDF proximity", 
                      "NPDES proximity")
# default EJ vars for now:
names.ej <- c("EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", "EJ.DISPARITY.cancer.eo", 
              "EJ.DISPARITY.neuro.eo", "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", 
              "EJ.DISPARITY.pctpre1960.eo", "EJ.DISPARITY.traffic.score.eo", 
              "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", 
              "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.proximity.npdes.eo")
names.ej.friendly <- paste('EJ:', names.e.friendly)
names.all <- c(names.d, names.e, names.ej)
names.all.friendly <- c(names.d.friendly, names.e.friendly, names.ej.friendly)
#######################################################################################

shinyUI(
  fluidPage(
    titlePanel(
      h2(textOutput('titletext'))
      ),
    
    tabsetPanel(
      
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
            h4(textInput('batchname', "Rename this analysis", "Example Dataset"))
          ),
          column(
            4,
            h4('Analysis settings'),
            numericInput('threshold', label='Threshold %ile:', value=80, min=1, max=100),
            checkboxGroupInput('probs','Percentiles of sites & residents to calculate:', choices=probs.default.choices, selected = probs.default.choices)
            #         checkboxInput('header', 'Header', TRUE),
            #         radioButtons('sep', 'Separator',
            #                      c(Comma=',', Semicolon=';', Tab='\t'), ','),
            #         radioButtons('quote', 'Quote',
            #                      c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'), 
            
          )
        ),
        tags$hr(),
        downloadButton('download.batchdata', 'Download the uploaded batch data with renamed fields but not summarized') #,
        
        #dataTableOutput("fulltableout")
      ),
      
      tabPanel(
        "Summary rows", 
        #h4(textOutput("name2", container = span)),
        downloadButton('download.rowsout', 'Download'),
        radioButtons('transpose.rowsout', "Display transposed:", 
                     c("One indicator per row, and one summary stat or site per column (useful for using search box to filter/view only cancer-related indicators for example)" =  TRUE,
                       "One indicator per column,  one summary stat or site per row (useful for sorting sites by State, demographics, or any other indicator)" = FALSE)),
        dataTableOutput("rowsout")
      ),
      
      tabPanel(
        "Summary cols", 
        #h4(textOutput("name3", container = span)),
        downloadButton('download.colsout', 'Download'),
        dataTableOutput("colsout")
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
        "Barplots", 
        plotOutput('barplots'),
        downloadButton('download.barplot', 'Download'),
        fluidRow(
          h4('Barplot settings'),
          column(4, radioButtons('bartype', h5('Indicator type'), list('Demographic'='Demographic', 'Environmental'='Environmental','EJ'='EJ'))),
          column(3, radioButtons('barvartype', 'Data Type', list('Percentile of population'='pctile', 'Raw data'='raw')))
        )
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
      )
    )
  )
)

#######################
# notes:
# Output function     reactive output created
# htmlOutput          raw HTML
# imageOutput         image
# plotOutput	        plot
# tableOutput	        table
# textOutput	        text
# uiOutput	          raw HTML
# verbatimTextOutput	text
#######################
