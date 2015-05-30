# for debugging:
# options(shiny.reactlog=TRUE)
# options(shiny.trace=TRUE)
# options(error=NULL)
 # options(shiny.error=browser)

library(shiny) # http://shiny.rstudio.com

# TO OBTAIN THE leaflet PACKAGE (NOT ON CRAN):
#  devtools::install_github("rstudio/leaflet")
library(leaflet) # for interactive maps
#library(leafletR) # for interactive maps (a different package than leaflet)

library(maps) # for static maps; choropleth of counties, etc.
library(mapproj)
source("maphelpers.R")  # if we want percent choropleths of county data
counties <- readRDS(file='data/counties.rds') # if we want county data
counties$nonwhite <- round( 100 - counties$white, 1)
# load gomap.js ??

# library(dplyr) # might not need this
require(Hmisc) # various useful functions for data analysis
library(plotrix) # for better weighted.hist than ggplot2 can provide.
require(ggplot2) # for geom_histogram() that allows weights to be used. plotrix package also does wtd hist, but better.

# CUSTOM FUNCTIONS USED HERE: REQUIRE AS A PACKAGE, OR SOURCE FROM THE SHINY APP'S BASE DIRECTORY:
# These will be made public as parts of packages called analyze.stuff and ejanalysis, 
# at http://ejanalysis.github.io
#source('pop.ecdf.R')  # plot pop-wtd ecdf(s) for one demographic group vs others, etc.,  for comparing conditions between groups across many Census areal units (e.g. tracts)
source('pct.above.R')         # returns percent of rows (or wtd people) that have value above specified cutoff (or mean as default), for each column of data.frame
#source('pct.below.R')         # returns percent of rows (or wtd people) that have value below specified cutoff (or mean as default), for each column of data.frame
source('count.above.R')        # returns count of how many rows (or wtd people) have value above specified cutoff (or mean as default), for each column of data.frame
source('cols.above.count.R')  # returns count of how many cols (e.g. how many variables or indicators) have value above specified cutoff (or mean as default), for each row of data.frame
source('flagged.R')      # creates flag that is TRUE if at least one of 12 indicators is > cutoff (EJ index >50th or 80th or 95th%ile, for example), for each of many rows of a data.frame
source('rowMaxs.R')     # returns Max of each row
source('rowMins.R')     # returns Min of each row
source('colMaxs.R')     # returns Max of each col
source('colMins.R')     # returns Min of each col
source('wtd.colMeans.R')     # returns wtd.mean of each col
source('lead.zeroes.R')
source('change.fieldnames.R')  # updated function that helps change or resort fieldnames using a map file mapping old to new names
source('wilcoxon.pvalues.r')  # for statistical significance testing

source('batch.read.R')
source('batch.clean.R')
source('batch.summarize.R')

##################################################################################################

##################################################################################################

shinyServer(function(input, output, session) {
  
  #   # seems to need this to calculate barplot at start
  updateTabsetPanel(session, "tabset1", selected = "Detailed settings")

  # User-defined name for this dataset / analysis
  
  mybatchname  <-  renderText(input$batchname)
  output$name1 <-  renderText(input$batchname)
  output$name2 <-  renderText(input$batchname)
  output$name3 <-  renderText(input$batchname)
  output$name4 <-  renderText(input$batchname)
  output$name5 <-  renderText(input$batchname)
  output$titletext <- renderText(paste("Batch Results Summarizer:", as.character(input$batchname)))
  
  # Code enabling Download of tables and charts

  output$download.rowsout <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'summary stats on each indicator -plus site data.csv')
    },
    contentType='text/csv',
    content = function(file) {
      write.csv( make.colnames.friendly.complete( rbind( outlist()$rows, fulltabler() )  ), file)
    }
  )
  
  output$download.colsout <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'summary stats on each site -plus site data.csv')
    },
    contentType='text/csv',
    content = function(file) {
      write.csv( cbind( outlist()$cols,  make.colnames.friendly.complete( fulltabler() ) ), file)
    }
  )
  
  output$download.table1 <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'Table 1 Demographic Summary.csv')
    },
    contentType='text/csv',
    content = function(file) {
      write.csv( table1(), file)
    }
  )

  output$download.table2 <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'Table 2 Demographic Summary.csv')
    },
    contentType='text/csv',
    content = function(file) {
      write.csv( table2(), file)
    }
  )
  
  output$download.table3 <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'Table 3 Demographic Summary.csv')
    },
    contentType='text/csv',
    content = function(file) {
      write.csv( table3(), file)
    }
  )
    
  output$download.barplot <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), barplotkind(), 'barplot comparing sites to US.png')
    },
    contentType='image/png',
    content = function(file) {
      #ggsave(file, plot=barplots(), device=png, width = 1200, height = 768, units = "px", pointsize = 12)
      png(filename=file, width = 1400, height = 768, units = "px", pointsize = 12)
      #print(barplots.react())
      # THIS WORKAROUND DOES WORK :
            barplots.NONreact()
      dev.off()
    }
  )
  
  output$download.histogram <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), histogramkind(), 'histogram across sites or people.png')
    },
    contentType='image/png',
    content = function(file) {
      #ggsave(file, plot=histograms.react(), device=png, width = 12.00, height = 7.68, units = "cm") # width = 1200, height = 768, units = "mm",
      png(filename=file, width = 1200, height = 768, units = "px", pointsize = 12)
#      print(histograms.react())
      dev.off()
    }
  )
  
  
  #############################
  # OLD VS NEW VS FRIENDLIER FIELDNAMES
  
  lookup.fieldnames <- reactive({
    # THIS LETS USER UPLOAD A CUSTOM LOOKUP TABLE/MAPPING OF OLD TO NEW FIELDNAMES TO RENAME THEM AND CATEGORIZE THEM, 
    # BUT I BELIEVE THE UPLOADED BATCH DATA WILL NOT BE RENAMED UNLESS/UNTIL IT IS UPLOADED AGAIN.
    # input$file2 will be NULL initially, or can use the example file and preload it
    # After the user selects and uploads a file, it will be a data frame with 
    # 'name', 'size', 'type', and 'datapath' columns. 
    # The 'datapath' column will contain the local filenames where the data can be found.
    # THIS IS USED ALSO BY vartype() and varcategory() to classify the indicators in useful ways for filtering tables.
    inFile2 <- input$file2
    if (is.null(inFile2)) {
      myfile <- mynamesfile.default
    } else {
      myfile <- inFile2$datapath
    }
    output$infilename2 <- renderText(inFile2$name)
    fieldnamesmap <- read.csv(myfile, stringsAsFactors = FALSE)
    fieldnamesmap
  })
  
  vartype <- reactive({
    if ('vartype' %in% colnames(lookup.fieldnames())) {
      # for each of the mycolnames(), find the corresponding vartype by looking in the lookup.fieldnames() table
      lookup.fieldnames()$vartype[match(mycolnames(), lookup.fieldnames()$newnames)]
    } else {
      NA
    }
  })
  
  varcategory <- reactive({
    if ('varcategory' %in% colnames(lookup.fieldnames())) {
      lookup.fieldnames()$varcategory[match(mycolnames(), lookup.fieldnames()$newnames)]
    } else {
      NA
    }
  })
  
 
  
  ##################################################################################################
  # UPLOADED DATASET AS A TABLE

    # This (the reactive expression called fulltabler) is the uploaded dataset that 
  # is the output of the batch processing and input to the batch summarizer:
  
  fulltabler <- reactive({
    
    # input$file1 will be NULL initially, or can use the example file and preload it?
    # After the user selects and uploads a file, it will be a data frame with 
    # 'name', 'size', 'type', and 'datapath' columns. 
    # The 'datapath' column will contain the local filenames where the data can be found.
    inFile <- input$file1
    if (is.null(inFile)) {
      myfile <- mydemofile
    } else {
      myfile <- inFile$datapath
    }
    
    output$infilename <- renderText(inFile$name)
    
    # Read the uploaded batch results. read.csv used to read text file (csv format) that was exported from ArcGIS batch tool
    # To allow user to specify parameters of upload file format:
    #fulltable <- read.csv(myfile, header=input$header, sep=input$sep, quote=input$quote, stringsAsFactors=FALSE)
    fulltable <- read.csv(myfile, stringsAsFactors = FALSE)
    # Clean the uploaded batch results. Check & rename columns to friendly names specified in namesfile map, reorder columns, etc.
    # fulltable <- batch.clean(fulltable, namesfile=mynamesfile)
    fulltable <- batch.clean(fulltable, oldcolnames=lookup.fieldnames()$oldnames, newcolnames=lookup.fieldnames()$newnames )
    if ('name' %in% colnames(fulltable) ) { rownames(fulltable) <- fulltable[ , 'name'] } # become colnames when transposed for viewing? no.
    fulltable
  })
  
  ####################################################################################
  # TALLIES
  
  popcount <- reactive({
    sum(fulltabler()$pop, na.rm=TRUE)
  })
  
  sitecount <- reactive({
    length(fulltabler()[,1])
  })
  
  output$sitecount.text <- renderText({
      paste( sitecount(), 'sites have been uploaded, analyzed, and mapped.')
    })
  
  output$sitecount.text2 <- renderText({
    paste( sitecount(), 'sites have been uploaded, analyzed, and mapped.')
  })
  
  output$popsitecounts.out <- renderText({
    paste("Total population is ", format(popcount(), big.mark=',', scientific=FALSE), " at ", sitecount(), " sites.", sep='')
  })
    
  output$popsitecounts.out2 <- renderText({
    paste("Total population is ", format(popcount(), big.mark=',', scientific=FALSE), " at ", sitecount(), " sites.", sep='')
  })
  
  counts.by.state <- reactive({
    id=isolate( fulltabler()$OBJECTID )
    state=isolate( fulltabler()$statename )
    x=aggregate.data.frame(id, by=list(state), FUN=length)
    # #x=bystats(id, state)[,'N']
    # #x=bystats[-length(x)] # if want to remove the "ALL" column
    names(x) <- c('State', 'Site count')
    popcounts <- aggregate.data.frame(fulltabler()$pop, by=list(fulltabler()$statename), FUN=function(z) sum(z, na.rm = TRUE))
    names(popcounts) <- c('State', 'Population count')
    x= merge(x, popcounts, all.x=TRUE, all.y=FALSE) # in case some have NA for pop, keep all states with a length even if sum is NA
    x[ , 'Population count'] <- format(x[ , 'Population count'], scientific=FALSE, big.mark = ',')
    x
    # x = rbind(x, colSums(x,na.rm=TRUE))
  })  
  
  output$counts.by.state.out <- renderDataTable({
    counts.by.state()
  }, 
  options=list(paging=FALSE, searching=FALSE)
  )
  
  ## same but by region:
  
  counts.by.region <- reactive({
    id=isolate( fulltabler()$OBJECTID )
    REGION=isolate( fulltabler()$REGION )
    x=aggregate.data.frame(id, by=list(REGION), FUN=length)
    # #x=bystats(id, state)[,'N']
    # #x=bystats[-length(x)] # if want to remove the "ALL" column
    names(x) <- c('Region', 'Site count')
    popcounts <- aggregate.data.frame(fulltabler()$pop, by=list(fulltabler()$REGION), FUN=function(z) sum(z, na.rm = TRUE))
    names(popcounts) <- c('Region', 'Population count')
    x= merge(x, popcounts, all.x=TRUE, all.y=FALSE) # in case some have NA for pop, keep all zones with a length even if sum is NA
    x[ , 'Population count'] <- format(x[ , 'Population count'], scientific=FALSE, big.mark = ',')
    x
    # x = rbind(x, colSums(x,na.rm=TRUE))
  })  
  
  output$counts.by.region.out <- renderDataTable({
    counts.by.region()
  }, 
  options=list(paging=FALSE, searching=FALSE)
  )
  
  #####################
    
    
  #####################
  # *** SPECIFY MORE PARAMETERS HERE THAT RELY ON fulltable 

  mycolnames <- reactive({ colnames(fulltabler()) })
  # *** could replace mycolnames with friendly names at some point, or at least do that just before rendering or downloading,
  # by using lookup.fieldnames()$longnames to replace corresponding $newnames
  
  ######## probably redundant:
  
  mycolnames.friendly <- reactive({
    # lookup the unfriendly colnames in the lookup table to get longname 
    # For default summary cols view, this is good- not unique names but other cols are there so OK.
    # For transposed view, need headers to be full unique names, so need vartype and longname...paste them together to create a unique friendlier name
    lookup.fieldnames()$longname[ match(mycolnames(), lookup.fieldnames()$newname) ] 
  })
  
  mycolnames.friendly.complete <- reactive({
    # lookup the unfriendly colnames in the lookup table to get longname 
    # For default summary cols view, this is good- not unique names but other cols are there so OK.
    # For transposed view, need headers to be full unique names, so need vartype and longname...paste them together to create a unique friendlier name
    paste( 
      lookup.fieldnames()$longname[ match(mycolnames(), lookup.fieldnames()$newname) ] , 
      lookup.fieldnames()$vartype[ match(mycolnames(), lookup.fieldnames()$newname) ] 
    )
  })
  
  make.colnames.friendly <- function(df) {
    # function of fulltabler()
    colnames(df) <- change.fieldnames(colnames(df), oldnames= lookup.fieldnames()$newname, newnames = lookup.fieldnames()$longname)
    df
  }
  
  make.colnames.friendly.complete <- function(df) {
    # function of fulltabler()
    colnames(df) <- change.fieldnames(colnames(df), oldnames= lookup.fieldnames()$newname, newnames = paste(lookup.fieldnames()$longname, lookup.fieldnames()$vartype) )
    df
  }
  ########
  
  #####################  #####################
  # comparisons to thresholds
  #####################  #####################
  
  #####################  #####################
  # Specify names of indicators (columns) that will be compared to the threshold when doing a threshold check, 
  # summarizing for each site how many of those indicators are at/above some specified threshold.
  # Let user specify these at some point via selectize multiple selections pull down on tab1, using renderUI or something to show list of current set of fields as options
  # make this the default but offer them ui that shows colnames(fulltabler()) and lets them hit multiple checkboxes or something to specify 3+ groups and thresholds for those.
  
  mythreshnames.default <- reactive({
    list( 
      group1=grep('^pctile.EJ.DISPARITY.', colnames(fulltabler()), value=TRUE) , 
      group2=grep('region.pctile.EJ.DISPARITY.', colnames(fulltabler()), value=TRUE) , 
      group3=grep('state.pctile.EJ.DISPARITY.', colnames(fulltabler()), value=TRUE) 
    )
  })
  
  # let user pick multiple fields to compare to user-defined thresholds
  # must convert default raw names into friendly complete names to show as defaults selected at start in selected=
  output$thresholdPICKS1 <- renderUI({
    mychoices <- mycolnames()
    names(mychoices) = mycolnames.friendly.complete()
    selectInput(
      "mythreshnames.in1", 
      "Choose fields to compare to threshold(s):", 
      choices = mychoices,
      multiple = TRUE,
      selected= mythreshnames.default()[[1]]
    )
  })
  
  output$thresholdPICKS2 <- renderUI({
    mychoices <- mycolnames()
    names(mychoices) = mycolnames.friendly.complete()
    selectInput(
      "mythreshnames.in2", 
      "Choose fields to compare to threshold(s):", 
      choices = mychoices,
      multiple = TRUE,
      selected = mythreshnames.default()[[2]]
      #       selected= change.fieldnames(  ( mythreshnames.default()[[2]] ), 
      #        oldnames= lookup.fieldnames()$newname, newnames = paste(lookup.fieldnames()$longname, lookup.fieldnames()$vartype) )
    )
  })

  output$thresholdPICKS3 <- renderUI({
    mychoices <- mycolnames()
    names(mychoices) = mycolnames.friendly.complete()
    selectInput(
      "mythreshnames.in3",
      "Choose fields to compare to threshold(s):", 
      choices = mychoices,
      multiple = TRUE,
      selected = mythreshnames.default()[[3]]
      #       selected= change.fieldnames(  ( mythreshnames.default()[[3]] ), 
      #        oldnames= lookup.fieldnames()$newname, newnames = paste(lookup.fieldnames()$longname, lookup.fieldnames()$vartype) )
    )
  })

  mythreshnames <-  reactive({ 
    #mythreshnames.default()
    # temporarily disable user defined names until code finished ***
    x=c(list(usergroup1=input$mythreshnames.in1,  usergroup2=input$mythreshnames.in2, usergroup3=input$mythreshnames.in3) )
    names(x)= c(input$threshgroup1, input$threshgroup2, input$threshgroup3)
    x
  })
  
  mythreshgroups <- reactive({
    list(input$threshgroup1, input$threshgroup2, input$threshgroup3)
  })
    
  mythresholds <- reactive({
    list(input$threshold1,  input$threshold2, input$threshold3 )
  })
  
  # not yet used?
  output$mythreshnames.toprint <- renderPrint( mythreshnames() )
  
  #####################  #####################
  #####################  #####################
  
  namecolpixels <- reactive({
    namecolchars <- min(max.allowed, max(nchar(fulltabler()$name)) )
    namecolpixels <- pixels.per.char * namecolchars
    namecolpixels
  })
  
  colfun.picked <- colfun.picked.default # 'all' # later can be a logical vector but length must equal # of such funs defined as options in batch.summarize()
  rowfun.picked <- rowfun.picked.default # 'all' # later can be a logical vector but length must equal # of such funs defined as options in batch.summarize()
  
  ##################################################################################################
  # CREATE TABLES OF SUMMARY STATS
  
  # Create the reactive expression providing summary rows and cols, the key output of the batch summarizer:
  # Create summary stats from uploaded batch results. 
  # outlist() is a list of 2 elements: 
  #   rows (rows of summary stats), & cols (columns of summary stats)
  
  outlist <- reactive({ 
    
    x= batch.summarize(
      fulltabler(),
      wts=fulltabler()[ , mywtsname], cols=mycolnames(), 
      threshnames= mythreshnames(), threshold=mythresholds(), threshgroup=mythreshgroups(),
      colfun.picked=colfun.picked, rowfun.picked=rowfun.picked,
      probs=as.numeric( input$probs ), na.rm=na.rm
    )
    
    # THIS FIXES BUG IN SORTING/ FORMATTING cols RESULTS AS NUMERIC VS CHARACTER
    x$cols <- as.data.frame(x$cols, stringsAsFactors=FALSE)
    # but can't do this here WHILE STRINGS ARE IN SOME CELLS: 
    # x$rows <- as.data.frame(x$rows, stringsAsFactors=FALSE)
    
    # For summary cols, put a duplicate column of user's site names field first if it exists, so can freeze it when seeing summary stat columns view
    if ('name' %in% colnames(fulltabler())) {x$cols <- cbind(Sitename=fulltabler()$name, x$cols, stringsAsFactors=FALSE) }
    
    # ******** don't set anything to NA for plotting or downloading or mapping! only for onscreen display/sort/filter!
    
    
    # FOR DOWNLOAD, ONLY FORMAT SOME KEY STATS BETTER - but want to round only for web display not download
    vars.NA <- c('OBJECTID',	'FACID',	'name',	'lat',	'lon',	'radius.miles',	'ST',	'statename',	'REGION')
    x$rows[ , vars.NA] <- NA
    vars.round0 <- 'pop'
    x$rows[ , vars.round0] <- round( x$rows[ , vars.round0], 0)
    
    x$rows <- as.data.frame(x$rows, stringsAsFactors=FALSE)

    #     stats.round2 <- c('Average site', 'Average person')
    #     numeric.cols <- apply(x$rows, 2, class)=='numeric' # all should be now
    #     x$rows[ stats.round2, numeric.cols] <- round( x$rows[ stats.round2, numeric.cols] , 1)
    
    #    vars.comma  <- 'pop'
    #    x$rows[ , vars.comma]  <- format( x$rows[ , vars.comma], big.mark=',')
    
    x

  })
  
  ###########################
  # Render comprehensive output/result rows & cols of the batch summarizer as an interactive datatable for the webpage:
  # this recreates the output cols AND rows each time any inputs/settings change, which might be slow for a huge dataset,
  # but it is unlikely you would ever want to recalculate ONLY the colsout, so not a big deal
  
  ###########################  ###########################  ###########################
  ###########################  ###########################  ###########################
  ###########################
  # one summary stat per site  # RENDER THE SUMMARY *COLS* AS AN INTERACTIVE DATA TABLE FOR WEB
  ###########################
  ###########################  ###########################  ###########################
  ###########################  ###########################  ###########################

  output$colsout <- renderDataTable( 
    {
      z <- cbind( outlist()$cols, make.colnames.friendly.complete( fulltabler()  ) , stringsAsFactors=FALSE)
      z
    }, 
    options=list(
      dom = 'rtip',
      lengthMenu = list(c(10, 100, -1), c('10', '100', 'All')),
      pageLength = 100,  # -1 loads all the rows into page 1, which might be too slow if huge # of sites is uploaded
      scrollX= TRUE,
      scrollY= "340px", # 440px is enough for 12 rows on my browser but headers wrap to use up lots of space
      scrollCollapse= TRUE,
      
      # FREEZE FIRST COLUMN AND HEADER:
      #       initComplete = I("function(settings, json){
      #         new $.fn.dataTable.FixedHeader(this, {
      #           left:   true
      #         } );
      #       }"),
      
      # THIS WORKS TO FIX COLUMNS FOR SCROLLING TO RIGHT, BUT IT BREAKS THE FILTER OPTION for the frozen column, AT THE BOTTOM OF THE TABLE ?!
      # so you can't filter on site name in this case *** but that isn't essential for now, espec since can filter on duplicate column that also has site name.
      initComplete = I("function(settings, json){
                new $.fn.dataTable.FixedColumns(this, {
                  leftColumns: 1
                } );
      }"),

      columnDefs = list(list(width="420px", 
                        # targets=list(0,1,2)
                        targets=list(0, length(outlist()$cols[1,]) + 
                                       which(mycolnames()=='name')
                                     -1))
      ) # makes the 1st column wide & the one called name
    )
  )
  
  ###########################  ###########################  ###########################
  ###########################  ###########################  ###########################
  ###########################
  # one summary stat per indicator  # RENDER THE SUMMARY *ROWS* AS AN INTERACTIVE DATA TABLE FOR WEB 
  ###########################
  ###########################  ###########################  ###########################
  ###########################  ###########################  ###########################
  
  output$rowsout <- renderDataTable({
    
    # prepare to display table of summary stats which is outlist()$rows, 
    # ideally along with the full table of facility-specific batch results, but it slows display if long list and it isn't useful without fixed cols/ freeze panes, which are hard to do while maintaining filtering.
    # but still will provide full site list for download with these summary stats even if not displayed in onscreen table.
    
    x <- outlist()$rows
    
    ##############################################
    # PUT SUMMARY STATS AND INDIVIDUAL SITES DATA TOGETHER
    # one row per indicator, one col per stat or site
    ##############################################
    
    charcols <- c("FACID", "name", "ST", "statename", 'lat', 'lon' )  #  "pop", "radius.miles", are ok. 'FACID' would be nice to sort on as # if it is that, but will need to assume it is character just in case.
    sites.data <- fulltabler()
    sites.data[ , charcols] <- NA  # MUST REMOVE CHARACTER FIELD INFO LIKE NAME/FACID/ST/STATENAME TO BE ABLE TO TRANSPOSE THIS INTO A DATA.FRAME AND SORT ONE FACILITY BY ALL ITS INDICATORS FOR EXAMPLE
    
    z = data.frame(
      n=lead.zeroes(1:length(mycolnames()), nchar(max(length(mycolnames())))),
      Category= varcategory(),
      Type= vartype(),
      Indicator=mycolnames.friendly(),
      # data.frame(  t(x), t(sites.data ), stringsAsFactors=FALSE, check.rows=FALSE, check.names=FALSE),
      # without sites data for onscreen display:
      data.frame(  t(x), stringsAsFactors=FALSE, check.rows=FALSE, check.names=FALSE),
      stringsAsFactors=FALSE, check.rows=FALSE, check.names=FALSE
    )
    # , check.rows=FALSE, check.names=FALSE   # is to avoid replacing spaces in colnames with a period . but there is some chance user will use invalid names for sites and that it might create a problem?
    
    ##############################################
    # QUICK FIXES TO FORMATTING AND SORTING *** NOW THAT SUMSTATS AND SITES ARE TOGETHER
    # REPLACED THE STRING CHARACTER CELLS WITH NA SO THAT SORTING BY NUMBER WILL WORK CORRECTLY
    ##############################################
    
    entirely.string.fields <- c('n' , 'Category', 'Type', 'Indicator') # can't just say sapply(mydf, class) I think
    # indicators to round to zero decimal places, but not for the string fields of those indicators:
    vars.round0 <- unique( c( 'pop', names.d, grep('VSI.eo', mycolnames(), value=TRUE), grep('pct', mycolnames(), value=TRUE) ) ) # intended to find pctile and pct and VSI.eo to get the ones that are integer 0-100 
    fields.to.round <- colnames(z)[!(colnames(z) %in% entirely.string.fields)]
    # round all to 2 decimals, then just some to zero decimals
    z[             , fields.to.round ] <- round( z[             , fields.to.round ] , 2)
    z[  vars.round0, fields.to.round ] <- round( z[  vars.round0, fields.to.round ] , 0)
    
    z
    
  }, 
  options=list(
    scrollX= TRUE,
    scrollY= "440px", # 440px is enough for 12 rows on my browser
    scrollCollapse= TRUE,
    lengthMenu = list(c(10, 200, -1), c('10', '200', 'All')),
    pageLength = 200,  # -1 would mean all of the rows of summary stats are in the window
    dom = 'rtip',
    # *** ??? this doesn't seem to get applied until after filter is used!? 
    columnDefs = list(list(width="280px", targets=list(3))) #,  
    #columns = ???
    
    ## Try FixedHeader approach to FREEZE HEADER AND LEFT COLUMN: - but this as written doesn't freeze 1st 4 cols which is needed and makes it harder to set colwidths and scroll down within a window
    #     initComplete = I("function(settings, json){
    #       new $.fn.dataTable.FixedHeader(this, {
    #         left:   true
    #       } );
    #     }"),
    
    ## Try FixedColumns approach -- THIS WORKS TO FIX 4 COLUMNS FOR SCROLLING TO RIGHT, BUT 
    ## IT BREAKS THE FILTER OPTION in frozen cols AT THE BOTTOM OF THE TABLE !?
    #     initComplete = I("function(settings, json){
    #         new $.fn.dataTable.FixedColumns(this, {
    #           leftColumns: 4 ,
    #           serverSide: true
    #         } );
    #     }"),
    
    # Try to get fixedcolumns and filtering at same time: 
    # *** It still won't filter on the fixed columns using shiny's renderDataTable() here, but does in their pure JS example...
    # see http://datatables.net/release-datatables/extensions/FixedColumns/examples/col_filter.html
    #
    #     initComplete = I("function(settings, json){
    # // Setup - add a text input to each footer cell
    # $('#example tfoot th').each( function () { 
    #   var title = $('#example thead th').eq( $(this).index() ).text();
    #   $(this).html( '<input type=\"text\" placeholder=\"Search '+title+'\" />' );
    # } );
    # 
    # // DataTable
    # var table = $('#example').DataTable( {
    #   scrollY:        \"440px\",
    #   scrollX:        true,
    #   scrollCollapse: true,
    #   paging:         false
    # } );
    # 
    # // Apply the filter
    # table.columns().indexes().each( function (idx) {
    # $( 'input', table.column( idx ).footer() ).on( 'keyup change', function () {
    #   table
    #   .column( idx )
    #   .search( this.value )
    #   .draw();
    #   } );
    # } );
    # 
    # new $.fn.dataTable.FixedColumns(this, {
    #   leftColumns: 4 ,
    #   serverSide: true
    # } );
    # 
    # table.fnUpdate();
    # }")
    #
    
  )
  )
  
  ###########################################  ###########################################
  ###########################################  ###########################################
  
  radius.miles <- reactive({
    fulltabler()$radius.miles[1]
  })
  
  ###########################################  ###########################################

  
  ###########################################
  # Create some summary tables of summary statistics & significance testing, comparing sites to US etc.
  
  table1 <- reactive({
    # table summarizing demog stats nearby and in US overall
    popnear = popcount()
    mytable <- cbind(
      Location=c('Total near these sites', 'US total', 'Overall near these sites (avg. person)',  'US overall', 'Avg person, ratio to US'), 
      Pop= c(format( c(popnear, popus, popnear, popus), big.mark=','), round(popnear/popus,4))
      )
    othercols <- rbind( format( popnear * 0.01 * outlist()$rows['Average person', names.d ] , big.mark = ',', digits=0, scientific=FALSE), 
                        format(us.counts[names.d] / 100, big.mark=',', digits=0, scientific=FALSE),
                        paste( round(row3<-outlist()$rows['Average person' , names.d], 0), '%',sep=''),
                        paste( round(row4<-us.percents[names.d], 0), '%',sep=''),
                        format( round(row3/row4,2)))
    colnames(othercols) <- names.d.friendly
    mytable <- cbind(mytable, othercols)
    rownames(mytable) <- NULL
    mytable
  })
  
  table2 <- reactive({
    # table of significance tests for avg site's D being above US avg
    mytable <- cbind(Statistic=c('At the average site', 'standard deviation', 't-statistic',  'p-value from Wilcoxon test', 'Avg site, ratio to US'))
    othercols <- rbind( paste( round(row1<- outlist()$rows['Average site', names.d ], 0), '%',sep = ''), 
                        round(sapply(fulltabler()[ , names.d], FUN=function(x) sd(x,na.rm=TRUE)), 2),
                        0, # t stat ***
                        0, #  wilcoxon.test(x= , n=length(fulltabler()[,1]) ) , # p value ***
                        round( row1 / us.percents[names.d],2))
    colnames(othercols) <- names.d.friendly
    mytable <- cbind(mytable, othercols)
    rownames(mytable) <- NULL
    mytable
  })

  table3 <- reactive({
    # table of significance tests for # of sites with D above US avg
    pct.above.usavg <- pct.above(fulltabler()[ , names.d ], benchmarks=us.percents[names.d], benchnames='cutoff', na.rm=TRUE, or.tied=FALSE, below=FALSE, wts=1, of.what='all')
    count.above.usavg <- count.above(fulltabler()[ , names.d ], benchmarks=us.percents[names.d], benchnames='cutoff', or.tied=FALSE, below=FALSE, wts=1)
    # sum( fulltabler()[ , names.d ] > us.percents[names.d]) / length(fulltablr()[,1]
    mytable <- cbind(Statistic=c(paste('% (#) of sites where demog. > US avg. (of ',length(outlist()$cols[,1] ),'sites)'), 'standard deviation', 't-statistic',  'p-value'))
    othercols <- rbind( paste( round( 100*  pct.above.usavg   , 0), '% (', count.above.usavg,')',sep = ''), 
                        #round(sapply( fulltabler()[ , names.d], FUN=function(x) sd(x,na.rm=TRUE)), 2),
                        0, # need standard deviation that is relevant to this statistic...
                        0, # t stat ***
                        0 ) # p value ***
    colnames(othercols) <- names.d.friendly
    mytable <- cbind(mytable, othercols)
    rownames(mytable) <- NULL
    mytable
  })
  
  output$table1 <- renderTable({table1()} )
  output$table2 <- renderTable({table2()} )
  output$table3 <- renderTable({table3()} )
  

  ##################################################################################################

  
  ##################################################################################################
  # EXECUTIVE SUMMARY TEXT
  
  ratio.to.us.d <- reactive({ outlist()$rows['Average person', names.d] /  fulltabler()[ 1, paste('us.avg.', names.d, sep='')] })
  ratio.to.us.e <- reactive({ outlist()$rows['Average person', names.e] /  fulltabler()[ 1, paste('us.avg.', names.e, sep='')] })
  
  max.ratio.to.us.d <- reactive({
    max( ratio.to.us.d(), na.rm=TRUE)
  })
  
  max.ratio.to.us.e <- reactive({
    max( ratio.to.us.e(), na.rm=TRUE)
  })
  
  max.ratio.to.us.d.name <- reactive({
    names.d[ which(ratio.to.us.d() == max.ratio.to.us.d() ) ]
  })

  max.ratio.to.us.e.name <- reactive({
    names.e[ which(ratio.to.us.e() == max.ratio.to.us.e() ) ]
  })
  
  execsum1.txt <- reactive({
    paste('People who live near (within ', radius.miles(), ' miles of any of) these ', sitecount(),' sites are ', 
          round(max.ratio.to.us.d(), 1), ' times as likely to be ', 
          tolower( mycolnames.friendly()[match( max.ratio.to.us.d.name(), mycolnames() )] ) ,' as the average person in the US (', 
          round(outlist()$rows['Average person', max.ratio.to.us.d.name()], 0)   ,'% vs. ', 
          round(fulltabler()[ 1, paste('us.avg.', max.ratio.to.us.d.name(), sep='')], 0), '%). The other demographic indicators have lower ratios.', sep='')
  })
  
  execsum2.txt <- reactive({
    paste('They are ', 
          round( outlist()$rows['Average person', max.ratio.to.us.d.name()] /  fulltabler()[ 1, paste('state.avg.', max.ratio.to.us.d.name(), sep='')], 1) ,
          ' times as likely to be ', 
          tolower( mycolnames.friendly()[match( max.ratio.to.us.d.name(), mycolnames() )] ), ' as the average person in the State they live in.', sep='')
  })
  
  execsum3.txt <- reactive({
    mypctiles <-  fulltabler()[ , paste('pctile.', max.ratio.to.us.d.name(), sep='')]
    mypctiles[is.na(mypctiles)] <- 0 # treat as zero if NA (missing) so stats come out right.
    paste( 
      format( sum( fulltabler()$pop[ mypctiles >= input$execsum.threshold.d ] , na.rm = TRUE), scientific=FALSE, big.mark = ','),
      ' people (',
      round( 100 * sum(fulltabler()$pop[ mypctiles >= input$execsum.threshold.d ] , na.rm = TRUE) / popcount(), 0) , 
      '% of all residents), who live near ',
      length(fulltabler()$pop[ mypctiles >= input$execsum.threshold.d ] ),
      ' (',
      round(100* length(fulltabler()$pop[ mypctiles >= input$execsum.threshold.d ] ) / sitecount(), 0),
      '%) of these sites, are in the top ', 100-input$execsum.threshold.d,'% of ',
      tolower(mycolnames.friendly()[match( max.ratio.to.us.d.name(), mycolnames() )]), ' values nationwide**.', sep='')
  })
  
  execsum4.txt <- reactive({
    paste('The median (50th percentile) site here is at the ',
          round(fulltabler()[ 1, paste('pctile.', max.ratio.to.us.d.name(), sep='')], 0), ' percentile of all US residents for ',
          tolower(mycolnames.friendly()[match( max.ratio.to.us.d.name(), mycolnames() )]), '.', sep='')
  })
  
  execsum5.txt <- reactive({
    paste('People who live near (within ', radius.miles(), ' miles of any of) these ', sitecount(),' sites have, on average, ', 
          round(max.ratio.to.us.e(), 1), ' times as high indicator values for ', 
          ( mycolnames.friendly()[match( max.ratio.to.us.e.name(), mycolnames() )] ) ,' as the average person in the US (', 
          round(outlist()$rows['Average person', max.ratio.to.us.e.name()], 2)   ,' vs. ', 
          round(fulltabler()[ 1, paste('us.avg.', max.ratio.to.us.e.name(), sep='')], 2), '). The other environmental indicators have lower ratios.', sep='')
    
  })
  
  execsum6.txt <- reactive({
    pctile = input$execsum.threshold
    mypctiles <- fulltabler()[ , paste('pctile.', names.ej, sep='')]
    mypctiles[is.na(mypctiles)] <- 0 # treat it as zero if it is missing, so tally of # at/above will count all if set cutoff to zero
    paste(
      format( sum( fulltabler()$pop[0 < cols.above.count(mypctiles, pctile, or.tied=TRUE )] , na.rm=TRUE), scientific=FALSE, big.mark = ','),
      ' people (',
      round(100* sum( fulltabler()$pop[0 < cols.above.count(mypctiles, pctile, or.tied=TRUE )] , na.rm=TRUE) / popcount() , 0),
      '% of all residents), who live near ',
      sum( 0 < cols.above.count(mypctiles, pctile, or.tied=TRUE ) , na.rm=TRUE),
      ' (',
      round(100* sum( 0 < cols.above.count(mypctiles, pctile, or.tied=TRUE ) , na.rm=TRUE) / sitecount() , 0),
      '%) of the sites, have one or more EJ Indexes in the top ', 100-pctile,'% of values nationwide**.', 
      sep='')
  })
  
  output$execsum1 <- renderText( execsum1.txt() )
  output$execsum2 <- renderText( execsum2.txt() )
  output$execsum3 <- renderText( execsum3.txt() )
  output$execsum4 <- renderText( execsum4.txt() )
  output$execsum5 <- renderText( execsum5.txt() )
  output$execsum6 <- renderText( execsum6.txt() )
  
  ##################################################################################################
  # BARPLOTS
  
  # for use in name of file when saving plot
  barplotkind <- reactive({
    paste(input$bartype, input$barvartype, input$barvarmean, sep='_' )
  })
  

  barplots.react <- reactive({
    
    # One set of bars per each of the myvars
    # *** possibly allow these to be set by user instead of hard-coded names:
    mybarvars <- switch(input$bartype,
                        'Demographic' = names.d,
                        'Environmental' = names.e,
                        'EJ' = names.ej
    )
    mybarvars.friendly <- switch(input$bartype,
                                 'Demographic' = names.d.friendly,
                                 'Environmental' = names.e.friendly,
                                 'EJ' = names.ej.friendly
    )
    
    mybarvars.refzone <- switch(input$bartype,
                                'Demographic' = paste('us.avg.', mybarvars,sep=''),
                                'Environmental' = paste('us.avg.', mybarvars,sep=''),
                                'EJ' = ''
    )
    
    if (input$barvartype=='pctile' | input$bartype=='EJ') {
      
      # PERCENTILE VALUES WILL BE SHOWN
      
      mybarvars <- paste('pctile.', mybarvars, sep='')
      
      if (input$barvarmean=='med') {
        # MEDIAN PCTILE IS SELECTED
        mybarvars.sumstat <- c( 'Median site','Median person')
        mybarvars.refzone.row <- 'Median person'  # 'Median person' 
        
        # MEDIAN PERSON'S PCTILE IS just 50
        mylegend <- c(  'Median site here', 'Median person here', 'Median person in US')
        
      } else {
        # AVERAGE PCTILE IS SELECTED
        mybarvars.sumstat <- c( 'Average site','Average person')
        mybarvars.refzone.row <- 'Average person' 
        
        # AVERAGE PERSON'S PCTILE IS NOT AVAILABLE IN THIS DATASET CURRENTLY ******  just shows median of 50 for now ***
        # 
        # IF IT WERE AVAILABLE HERE, THIS WOULD SAY mybarvars.refzone <- gsub('us.avg','us.avg.pctile', mybarvars.refzone) 
        mylegend <- c(  'Average site here', 'Average person here', 'Median person in US (not avg.)')
        
      }
    } else {
      
      # RAW VALUES FOR DEMOG OR ENVT WILL BE SHOWN
      
      if (input$barvarmean=='med') {
        # MEDIAN RAW IS SELECTED
        mybarvars.sumstat <- c( 'Median site','Median person')
        mybarvars.refzone.row <- 'Median person'  
        
        # MEDIAN PERSON'S RAW VALUE IS NOT IN THIS DATASET CURRENTLY ******** just shows avg person's raw for now ***
        mylegend <- c(  'Median site here', 'Median person here', 'Avg. person in US (not median)')
        
        # IF IT WERE AVAILABLE HERE, THIS WOULD SAY mybarvars.refzone <- gsub('us.avg', 'us.med', mybarvars.refzone)
      } else {
        # AVG RAW IS SELECTED      
        mybarvars.sumstat <- c( 'Average site','Average person')
        mybarvars.refzone.row <- 'Average person'  #
        mylegend <- c(  'Average site here', 'Average person here', 'Avg. person in US')
        
        # AVERAGE PERSON'S RAW SCORE IS us.avg...  already defined mybarvars.refzone as that to start with.
      }
    } 
    
    
    if (input$barvartype=='pctile' | input$bartype=='EJ') {
      # Percentile values for demog or envt indicators, or EJ picked which lacks raw values so must treat it as if plotting pctiles.
      # use 50th percentile person as US overall benchmark in this case
      plotdata <- rbind( outlist()$rows[ mybarvars.sumstat, mybarvars ], 
                         rep(50, length(mybarvars.refzone)) ) 
    } else {
      # Raw values for demog or envt indicators.
      # use actual US avg person's indicator score as US overall benchmark, even if medians are plotted
      #  we don't store US median raw score here so can't display it, but we could get/store that info in a lookup table.
      plotdata <- rbind( as.matrix( outlist()$rows[ mybarvars.sumstat, mybarvars ] ), 
                         as.matrix( outlist()$rows[ mybarvars.refzone.row, mybarvars.refzone] ) )
    }
    
    plotdata <- as.matrix(plotdata)
    
    if ( input$barvartype=='raw' & input$bartype=='Environmental') {myylims <- NULL} else {myylims <-  c(0, 100) }
    if ( input$bartype %in% c('Environmental', 'EJ')) {mycex=bar.cex * 0.7} else {mycex=bar.cex} # to see the long labels
    # as.character(input$barplot.title)  # was a way to just let user specify title
    
    barplot( plotdata, beside=TRUE, ylim=myylims, cex.axis = bar.cex, cex.names=mycex, 
             main= paste(mybatchname(), '-', input$bartype, input$barvartype, 'values for', paste(mylegend, collapse = ', ') , sep=' ' ) ,
             col=c('yellow', 'green', 'blue'),
             names.arg=mybarvars.friendly, 
             ylab=ifelse( (input$barvartype=='pctile' | input$bartype=='EJ'), 'US Percentile','Raw Indicator Value') )
    legend(x='topright', legend=mylegend, fill=c('yellow', 'green', 'blue'), 
           cex=bar.cex)
    
    #     myplot <- ggplot( plotdata, aes_string(  ) ) + 
    #       geom_barplot(fill='white', colour='darkgreen') +
    #       #geom_hline(aes_string(yintercept= )) +
    #       xlab( mybarvars.friendly ) + ylab(ifelse( (input$barvartype=='pctile' | input$bartype=='EJ'), 'US Percentile','Raw Indicator Value')) + 
    #       ggtitle( paste(mybatchname(), '-', input$bartype, input$barvartype, 'values for', paste(mylegend, collapse = ', ') , sep=' ' ))
    
    # barplot() has side effect of printing, but it just returns the barplot's midpoints of all data. not like ggplot that returns a plot object.
  })
  
  output$barplots <- renderPlot( barplots.react() )
  
  # TEMPORARY WORKAROUND TO BE ABLE TO DOWNLOAD BARPLOT:
  # ANOTHER OPTION IS TO RECREATE BARPLOT USING ggplot AND THEN CAN DOWNLOAD JUST LIKE HISTOGRAMS ARE DONE.
  
  barplots.NONreact <- function(){
    
    # One set of bars per each of the myvars
    # *** possibly allow these to be set by user instead of hard-coded names:
    mybarvars <- switch(input$bartype,
                        'Demographic' = names.d,
                        'Environmental' = names.e,
                        'EJ' = names.ej
    )
    mybarvars.friendly <- switch(input$bartype,
                                 'Demographic' = names.d.friendly,
                                 'Environmental' = names.e.friendly,
                                 'EJ' = names.ej.friendly
    )
    
    mybarvars.refzone <- switch(input$bartype,
                                'Demographic' = paste('us.avg.', mybarvars,sep=''),
                                'Environmental' = paste('us.avg.', mybarvars,sep=''),
                                'EJ' = ''
    )
    
    if (input$barvartype=='pctile' | input$bartype=='EJ') {
      
      # PERCENTILE VALUES WILL BE SHOWN
      
      mybarvars <- paste('pctile.', mybarvars, sep='')
      
      if (input$barvarmean=='med') {
        # MEDIAN PCTILE IS SELECTED
        mybarvars.sumstat <- c( 'Median site','Median person')
        mybarvars.refzone.row <- 'Median person'  # 'Median person' 
        
        # MEDIAN PERSON'S PCTILE IS just 50
        mylegend <- c(  'Median site here', 'Median person here', 'Median person in US')
        
      } else {
        # AVERAGE PCTILE IS SELECTED
        mybarvars.sumstat <- c( 'Average site','Average person')
        mybarvars.refzone.row <- 'Average person' 
        
        # AVERAGE PERSON'S PCTILE IS NOT AVAILABLE IN THIS DATASET CURRENTLY ******  just shows median of 50 for now ***
        # 
        # IF IT WERE AVAILABLE HERE, THIS WOULD SAY mybarvars.refzone <- gsub('us.avg','us.avg.pctile', mybarvars.refzone) 
        mylegend <- c(  'Average site here', 'Average person here', 'Median person in US (not avg.)')
        
      }
    } else {
      
      # RAW VALUES FOR DEMOG OR ENVT WILL BE SHOWN
      
      if (input$barvarmean=='med') {
        # MEDIAN RAW IS SELECTED
        mybarvars.sumstat <- c( 'Median site','Median person')
        mybarvars.refzone.row <- 'Median person'  
        
        # MEDIAN PERSON'S RAW VALUE IS NOT IN THIS DATASET CURRENTLY ******** just shows avg person's raw for now ***
        mylegend <- c(  'Median site here', 'Median person here', 'Avg. person in US (not median)')
        
        # IF IT WERE AVAILABLE HERE, THIS WOULD SAY mybarvars.refzone <- gsub('us.avg', 'us.med', mybarvars.refzone)
      } else {
        # AVG RAW IS SELECTED      
        mybarvars.sumstat <- c( 'Average site','Average person')
        mybarvars.refzone.row <- 'Average person'  #
        mylegend <- c(  'Average site here', 'Average person here', 'Avg. person in US')
        
        # AVERAGE PERSON'S RAW SCORE IS us.avg...  already defined mybarvars.refzone as that to start with.
      }
    } 
    
    
    if (input$barvartype=='pctile' | input$bartype=='EJ') {
      # Percentile values for demog or envt indicators, or EJ picked which lacks raw values so must treat it as if plotting pctiles.
      # use 50th percentile person as US overall benchmark in this case
      plotdata <- rbind( outlist()$rows[ mybarvars.sumstat, mybarvars ], 
                         rep(50, length(mybarvars.refzone)) ) 
    } else {
      # Raw values for demog or envt indicators.
      # use actual US avg person's indicator score as US overall benchmark, even if medians are plotted
      #  we don't store US median raw score here so can't display it, but we could get/store that info in a lookup table.
      plotdata <- rbind( as.matrix( outlist()$rows[ mybarvars.sumstat, mybarvars ] ), 
                         as.matrix( outlist()$rows[ mybarvars.refzone.row, mybarvars.refzone] ) )
    }
    
    plotdata <- as.matrix(plotdata)
    
    if ( input$barvartype=='raw' & input$bartype=='Environmental') {myylims <- NULL} else {myylims <-  c(0, 100) }
    if ( input$bartype %in% c('Environmental', 'EJ')) {mycex=bar.cex * 0.7} else {mycex=bar.cex} # to see the long labels
    # as.character(input$barplot.title)  # was a way to just let user specify title
    
    barplot( plotdata, beside=TRUE, ylim=myylims, cex.axis = bar.cex, cex.names=mycex, 
             main= paste(mybatchname(), '-', input$bartype, input$barvartype, 'values for', paste(mylegend, collapse = ', ') , sep=' ' ) ,
             col=c('yellow', 'green', 'blue'),
             names.arg=mybarvars.friendly, 
             ylab=ifelse( (input$barvartype=='pctile' | input$bartype=='EJ'), 'US Percentile','Raw Indicator Value') )
    legend(x='topright', legend=mylegend, fill=c('yellow', 'green', 'blue'), 
           cex=bar.cex)
    
    #     myplot <- ggplot( plotdata, aes_string(  ) ) + 
    #       geom_barplot(fill='white', colour='darkgreen') +
    #       #geom_hline(aes_string(yintercept= )) +
    #       xlab( mybarvars.friendly ) + ylab(ifelse( (input$barvartype=='pctile' | input$bartype=='EJ'), 'US Percentile','Raw Indicator Value')) + 
    #       ggtitle( paste(mybatchname(), '-', input$bartype, input$barvartype, 'values for', paste(mylegend, collapse = ', ') , sep=' ' ))
    
    # barplot() has side effect of printing, but it just returns the barplot's midpoints of all data. not like ggplot that returns a plot object.
  }
  

  ##################################################################################################
  # HISTOGRAMS
  
  # for use in name of file when saving plot
  histogramkind <- reactive({
    paste(input$myvar.friendly.base, input$refstat, input$refzone, input$sites.or.people, sep='_' )
  })
  
  histograms.react <- reactive({
    # e.g., draw histogram of selected variable's US percentiles, distribution over sites, vs expected distribution
    
    # *** User will be able to define these using checkboxes:
    # (this code presumes new variable names are as in default file)
    #       myvar.base <- 'VSI.eo'  # *** BUT IF IT IS A SUMMARY STAT LIKE ??? this won't work in hist(fulltable[ , myvar]) since it is in outlist()$rows not in fulltable
    #       myvar.full <- paste(refzone, refstat, myvar.base, sep='.')  # this presumes new variable names are as in default file
    #       myvar.full <- gsub('us.pctile', 'pctile', myvar.full)  # us.avg. is used but not us.pctile... it is just pctile for us! # this presumes new variable names are as in default file
    #       myvar.friendly.base <- 'Demographic Index'
    #       myvar.friendly.full <- paste(myvar.friendly.base, ', as ', refzone.friendly, ' ', refstat.friendly, ' across ', sites.or.people, sep='')
    
    refzone.friendly <- switch(input$refzone, 
                               'us'='US',
                               'region'='Region',
                               'state'='State')
    refstat.friendly <- switch(input$refstat,
                               'pctile'='Percentile',
                               'raw'='Indicator Value (not percentile)')
    
    # *** Should make this more generic/ flexible, not hard-coded names:
    # *** SHOULD USE FRIENDLY NAMES IN UI LIST AND PASS myvar.friendly.base 
    # and then HERE IN SERVER SHOULD fix to use that to get non friendly base for plot
    
    # get long name of field selected to plot, then convert to short name
    # *** could later change this to be more generic and use lookup.fieldnames() sort of like this:
    #   lookup.fieldnames()[match(input$myvar.friendly.base, lookup.fieldnames()[ , paste(c('longname', 'vartype'))]), 'newname' ] 
    myvar.friendly.base <- input$myvar.friendly.base
    myvar.base <- names.all[match(myvar.friendly.base, names.all.friendly)]
    
    if (substr(myvar.base, 1, 2)=='EJ') {
      myrefstat <- 'pctile'
      refstat.friendly <- 'Percentile'
    } else {
      myrefstat <- input$refstat
    }
    if (myrefstat=='raw' ) {
      myvar.full <- myvar.base
      myvar.friendly.full <- paste(myvar.friendly.base, ', as ', refstat.friendly, ' across ', input$sites.or.people, sep='')
    } else {
      myvar.full <- paste(input$refzone, myrefstat, myvar.base, sep='.')  # this presumes new variable names are as in default file
      myvar.full <- gsub('us.pctile', 'pctile', myvar.full)  # us.avg. is used but not us.pctile... it is just pctile for us! # this presumes new variable names are as in default file
      myvar.friendly.full <- paste(myvar.friendly.base, ', as ', refzone.friendly, ' ', refstat.friendly, sep='')
    }
    if (myrefstat=='raw' ) {
      sitecount <- 0 # suppress horizontal line benchmark when viewing raw data- it only applies to percentiles. Correct histo benchmark for raw would be the US overall histogram of that raw value in the selected # of bins, which is hard to provide here.
      popcount <- 0
    } else {
      sitecount <- length( fulltabler()[ , myvar.full] ) # but for popwtd hist, use popcount!
      #popcount < - sum( fulltable[ , mywtsname], na.rm=TRUE ) # assumes 'pop' is colname for weights, for now. fails.
      popcount <- outlist()$rows[ 'Sum','pop' ]
    }
    
    mybincount <- input$bincount # e.g. default (0:10)*10 # assumes you want to see sites in 10 bins, 0-10th percentile, 10-20, etc.
    expected.sites.per.bin= sitecount / mybincount # assumes you want to see sites in 10 bins  # but for popwtd hist, use popcount?!
    expected.pop.per.bin=   popcount  / mybincount  # but the horizontal line from this doesn't look right so don't graph it for now ****** 
    
    # HISTOGRAM plotted here
    
    if (input$sites.or.people=='Sites') {
      # see for formatting nicely:  http://docs.ggplot2.org/0.9.3.1/geom_bar.html
      
      myplot <- ggplot( fulltabler(), aes_string( myvar.full) ) + 
        geom_histogram(fill='white', colour='darkgreen', binwidth = diff(range( fulltabler()[ , myvar.full] ,na.rm=TRUE))/mybincount) +
        geom_hline(aes_string(yintercept=expected.sites.per.bin)) +
        xlab(myvar.friendly.full) + ylab(input$sites.or.people) + 
        ggtitle( paste(mybatchname(), ', ', myvar.friendly.full,': Distribution across ', input$sites.or.people, sep=''))
      
      py <- plotly()
      myplot2 <- py$ggplotly()
      
      return(myplot)
      
    } else {
      
      # *** hard coded to use mywtsname as weights for now:
      # BUT, ggplot approach to weighted hist is NOT the same as what we want, which plotrix package CAN do:
      # print(fulltabler()[ , myvar.full])    
      h = fulltabler()[ , myvar.full ]
      wts=fulltabler()[ , mywtsname  ]
      wts=wts[!is.na(h)] # but what if wts is na???
      h= h[!is.na(h)]
      if (myrefstat=='raw' ) {
        mybreaks <- 0
      }

      weighted.hist(
        x = h,
        w = wts,
        # breaks = seq(0, 100, 100 / mybincount),  # nice if demog raw, or any pctiles, are being plotted
        breaks = mybincount,  # needed if raw E being plotted
        main = paste(mybatchname(), ', ', myvar.friendly.full,': Distribution across ', input$sites.or.people, sep=''),
        # names.arg=myvar.friendly.full,
        ylab=input$sites.or.people
      )
      print('blah')
      abline(h=expected.pop.per.bin)
      curve(dnorm(x, mean=Hmisc::wtd.mean(h, wts), sd=sqrt(Hmisc::wtd.var(h,wts))), add=TRUE, col="darkblue", lwd=2)
      
#       myplot <- ggplot( fulltabler(), aes_string( myvar.full, weight=fulltabler()[ , mywtsname] ) ) + 
#         geom_histogram(fill='white', colour='darkgreen', binwidth = diff(range( fulltabler()[ , myvar.full] ,na.rm=TRUE))/mybincount) +
#         #geom_hline(aes_string(yintercept=expected.pop.per.bin)) +
#         xlab(myvar.friendly.full) + ylab(input$sites.or.people) + 
#         ggtitle( paste(mybatchname(), ', ', myvar.friendly.full,': Distribution across ', input$sites.or.people, sep=''))
#      return(myplot)
      
    }
    
  })
  
  output$histograms <- renderPlot( histograms.react() )

  ##################################################################################################
  # MAPS
  
  ############################################
  # MAP COUNTY CHOROPLETHS
  ############################################
  
  output$map <- renderPlot({
    args <- switch(input$mapvar,
                   "Percent Non-White" = list(counties$nonwhite, "darkblue", "% Non-White"),
                   "Percent White" = list(counties$white, "darkgreen", "% White"),
                   "Percent Black" = list(counties$black, "black", "% Black"),
                   "Percent Hispanic" = list(counties$hispanic, "darkorange", "% Hispanic"),
                   "Percent Asian" = list(counties$asian, "darkviolet", "% Asian"))
    # or hardcoded for now ***
    # data, color, legend params re passed using args above 
    args$min <- input$range[1]
    args$max <- input$range[2]
    
    do.call(percent_map, args)
  })
  
  ############################################
  # MAP SITES AS POINTS
  ############################################

  output$map.sites <- renderLeaflet({
    
    mypoints = data.frame(
      long=fulltabler()$lon, 
      lat= fulltabler()$lat,
      n=   fulltabler()$OBJECTID,
      name=fulltabler()$name,
      pop= fulltabler()$pop,
      pctmin=   fulltabler()$pctmin,
      pctlowinc=fulltabler()$pctlowinc
    )
    
    ## population near site as scaled from 1 to 20, where min site is 1, max is 20
    # popratio = mypoints$pop / min(mypoints$pop, na.rm=TRUE)
    # popratio = 1+( (popratio -1) * (( 20-1) / max(popratio-1)  ))

    m = leaflet(mypoints) %>% addTiles()
    # m = m %>% setView(mypoints$long[1], mypoints$lat[1], zoom = 4)
    
    mypopup = paste(
      'Site #', mypoints$n, '<br>', 
      'Name: ', mypoints$name, '<br>', 
      'Pop= ', mypoints$pop, '<br>', 
      mypoints$pctlowinc, '% low-income', '<br>', 
      mypoints$pctmin, '% minority', 
      sep='')
    
    if (input$markertype == 'big') {
      m = m %>% addMarkers(
        popup = mypopup, 
        options = markerOptions(title = mypoints$name)
      )
    } else {
      m = m %>% addCircleMarkers( 
        radius=circle.marker.radius,
        popup = mypopup,
        options = markerOptions(title = mypoints$name)
      )
    }
    
    m = m %>% addCircles(radius = radius.miles() * meters.per.mile, color = 'black', fill = FALSE)
    
    ## Zoom out to see all the points
    ## set view that shows all the points and a margin around their range
    ## m %>% fitBounds( L.latLngBounds() ) # doing this directly in leaflet might be easier
    lat.diff = abs(diff(range( mypoints$lat  )))
    lon.diff = abs(diff(range( mypoints$long )))
    lat.lowerleft = min(mypoints$lat)  #- (1.01 * lat.diff)
    lon.lowerleft = min(mypoints$long) #- (1.01 * lon.diff)
    lat.upright =   max(mypoints$lat)  #+ (1.01 * lat.diff)
    lon.upright =   max(mypoints$long) #+ (1.01 * lon.diff)
    
    #cat(lon.lowerleft, lat.lowerleft, lon.upright, lat.upright)
    m = m %>% fitBounds( lon.lowerleft, lat.lowerleft, lon.upright, lat.upright )
    #       m = m %>% fitBounds( -90, 33, -60, 46 )
    # clearBounds(m)
    # cat(lon.lowerleft, lat.lowerleft, 'and', lon.upright, lat.upright, ' are bounds \n' )
    
    # Possibly offer other layers
    #     myattribution='Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ, TomTom, Intermap, iPC, USGS, FAO, NPS, NRCAN, GeoBase, Kadaster NL, Ordnance Survey, Esri Japan, METI, Esri China (Hong Kong), and the GIS User Community'
    #     m %>% addTiles(
    #       paste(mapserver1, '/tile/{z}/{y}/{x}',sep='') #,
    #       # attribution = myattribution
    #     ) 
    
    m
    
  })
  
  ##################################################################################################
  # plotly interactive graphic
#   
# example:
#   output$plotly.chart <- renderGraph({
#     
#     #matched_complaints <- subset(df, grepl(input$search, df$Complaint.Type))
#     #most_common_complaints = sort(table(matched_complaints$Complaint.Type), decreasing=TRUE) 
#     df <- data.frame(a=1:100, b=501:600, c=c("blue",'green','orange','yellow'),d=c('high', 'low') )
#     col.to.search='c'
#     plotly.data <- subset(df, grepl(input$plotly.search, df[ , col.to.search]))
#       
#     list(
#       #message = c(most_common_complaints)
#       message = c(table(plotly.data))
#     )
#     
#   })
#   

  # ***** TO SHOW IN A DEBUGGING TAB:
  
  output$debugginginfo <- renderPrint( 
    #     # need one and only one line of output in this renderPrint()
    #     # print('DEBUGGING INFORMATION') 
    #     #str(cbind(  outlist()$cols,  make.colnames.friendly.complete( fulltabler()  ) , stringsAsFactors=FALSE) )
    #     #print(str(outlist()$cols))
    print(str(fulltabler()[,paste('pctile.',names.ej, sep='')]))
    #     #print(head(outlist()$cols))
    #print(mythreshgroups())
    #     #rownames(outlist()$rows)
    #     # head(outlist()$rows)
    #     (head(outlist()$rows,40))
    #     #     chr [1:179, 1:74] "001" "002" "003" "004" "005" "006" "007" "008" "009" "010" "011" "012" "013" ...
    #     #     - attr(*, "dimnames")=List of 2
    #     #     ..$ : chr [1:179] "OBJECTID" "FACID" "name" "lat" ...
    #     #     ..$ : chr [1:74] "n" "Category" "Type" "Indicator" ...
    #     #     
    #     #t( head(outlist()$rows, 30) )
  )
  
  # need to force a "recalculation" of barplots.react() at start of this app so it can be displayed before user changes any data.
  #  updateTabsetPanel(session, "tabset1", selected = "Barplots")
  updateTabsetPanel(session, "tabset1", selected = "Map of sites")
  
})

