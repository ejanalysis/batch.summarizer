# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# http://shiny.rstudio.com
library(shiny)

require(Hmisc) 

# Require a package or just source the code needed:
# THESE FUNCTIONS MUST BE IN THE SHINY APP'S BASE DIRECTORY
#source('pop.ecdf.R')  # plot pop-wtd ecdf(s) for one demographic group vs others, etc.,  for comparing conditions between groups across many Census areal units (e.g. tracts)
#source('pct.above.R')         # returns percent of rows (or wtd people) that have value above specified cutoff (or mean as default), for each column of data.frame
#source('pct.below.R')         # returns percent of rows (or wtd people) that have value below specified cutoff (or mean as default), for each column of data.frame
#source('count.above.R')        # returns count of how many rows (or wtd people) have value above specified cutoff (or mean as default), for each column of data.frame
source('cols.above.count.R')  # returns count of how many cols (e.g. how many variables or indicators) have value above specified cutoff (or mean as default), for each row of data.frame
source('flagged.R')      # creates flag that is TRUE if at least one of 12 indicators is > cutoff (EJ index >80th %ile, for example), for each of many rows of a data.frame
source('rowMaxs.R')     # returns Max of each row
source('rowMins.R')     # returns Min of each row
source('colMaxs.R')     # returns Max of each col
source('colMins.R')     # returns Min of each col
source('wtd.colMeans.R')     # returns wtd.mean of each col
source('change.fieldnames.R')  # updated function that helps change or resort fieldnames using a map file mapping old to new names

source('batch.read.R')
source('batch.clean.R')
source('batch.summarize.R')

shinyServer(function(input, output) {
  
  # DEFAULT VALUES, possibly could recode to allow user to change them: 
  mynamesfile <- 'map batch to friendly fieldnames v1.csv'
  probs <- c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1)
  mythreshold=80
  na.rm=TRUE
  
  output$rowsout <- renderTable({
    
    # input$file1 will be NULL initially. 
    # After the user selects and uploads a file, it will be a data frame with 
    # 'name', 'size', 'type', and 'datapath' columns. 
    # The 'datapath' column will contain the local filenames where the data can be found.
    
    inFile <- input$file1
    
    if (is.null(inFile)) {
      #output$rowsout <- NULL
      return(NULL)
    }
    
    # Read the uploaded batch results. read.csv used to read text file (csv format) that was exported from ArcGIS batch tool
    fulltable <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, stringsAsFactors=FALSE)
    
    # Clean the uploaded batch results. Check & rename columns to friendly names specified in namesfile map, reorder columns, etc.
    fulltable <- batch.clean(fulltable, namesfile=mynamesfile)
    
    # SPECIFY MORE PARAMETERS - USE DEFAULTS FOR NOW, POSSIBLY RECODE LATER TO LET USER CHANGE THESE
    mywtsname <- 'pop'
    mythreshnames <- grep('^pctile.EJ.DISPARITY.', colnames(fulltable), value=TRUE)
    mycolnames <- colnames(fulltable)
    colfun.picked <- 'all' # later can be a logical vector but length must equal # of such funs defined as options in batch.summarize()
    rowfun.picked <- 'all' # later can be a logical vector but length must equal # of such funs defined as options in batch.summarize()
    
    # Create summary stats from uploaded batch results. outlist is a list of 2 elements: 
    #   rows (rows of summary stats), & cols (columns of summary stats)
    outlist <- batch.summarize(fulltable, 
      wts=fulltable[ , mywtsname], cols=mycolnames, 
      threshnames=mythreshnames, threshold=mythreshold, 
      colfun.picked=colfun.picked, rowfun.picked=rowfun.picked,
      probs=probs, na.rm=na.rm
      )
    
    # Transpose summary stats rows for dispaly so browser can fit it easily, by showing all the summary stats for a single input column as one row on the screen instead of as one column.
    # This does not currently display the columns of summary stats like # of EJ indicator uspctiles at/above threshold at each site.
    output$colsout <- renderTable(  outlist$cols)
    
#     output$histdemog <- renderPlot({
#       # e.g. draw histogram or barplot
#       hist(fulltable[ , 'VSI.eo'], breaks = (0:10)*10, col = 'darkgray', border = 'white')
#     })
#     
    output$barplotdemog <- renderPlot({
      plotdata <- rbind( outlist$rows[ c('Average person', 'Average site'), 
                                       c('VSI.eo', 'pctlowinc', 'pctmin', 'pctlths', 'pctlingiso', 'pctunder5', 'pctover64') ], 
                         
                         outlist$row[ 'Average person',
                                      c('us.avg.VSI.eo', 'us.avg.pctlowinc', 'us.avg.pctmin', 'us.avg.pctlths','us.avg.pctlingiso', 'us.avg.pctunder5', 'us.avg.pctover64' )  ] )
      barplot( plotdata, beside=TRUE,
               legend.text=c('Average person', 'Average site', 'US Overall'),
               names.arg=c('Demog.Ind.', '% Low-inc.', '% Minority', '% <High School', '% Linguistic Isol.', '% < age 5', '% > age 64'))
    })
    
    t( outlist$rows)
    
    })
})

