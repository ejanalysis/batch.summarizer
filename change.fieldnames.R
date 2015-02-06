change.fieldnames <- function(allnames, oldnames, newnames, file=NA) {

	# FUNCTION TO CHANGE SOME OR ALL FIELDNAMES
	
	# Input parameters are:
	# allnames   a vector of all the original fieldnames, 
	# oldnames   a vector of just the fieldnames to be changed, and 
	# newnames   a vector of what those should be change to
	# file       a path\filename csv file with two columns: oldnames, newnames (instead of passing them to the function as parameters)

	# Value it returns:  the full set of fieldnames, with some or all updated.

	if (!is.na(file)) {
	  if (!missing(oldnames) | !missing(newnames)) {stop('Cannot specify file and also oldnames or newnames')}
	  changes <- read.csv(file, as.is=TRUE) 
	  if (!( ('oldnames' %in% names(changes) ) & ('newnames' %in% names(changes)) )) {stop('file must have columns named oldnames and newnames')}
	  oldnames <- changes$oldnames
	  newnames <- changes$newnames
	  if (missing(allnames)) {allnames <- oldnames}  
	  # ASSUME THAT IF ONLY file IS SPECIFIED, THE FIRST COLUMN HAS ALL THE NAMES, NOT JUST ONES TO CHANGE, BUT THEN FUNCTION SIMPLY RETURNS newnames FROM file
	}
	
	if (is.na(file) & missing(allnames)) {stop('Must specify allnames if file not specified')}  # specifying only file='blah.csv'  works
	if (!is.vector(allnames) | any(!is.character(allnames))) {stop('allnames must be a vector of character type fieldnames')}

	if (is.na(file) & sum(missing(oldnames), missing(newnames))==1) { 
	  stop('Must specify oldnames, newnames in 2-column csv file or as parameters, or specify none of the 3 for interactive editing of names')
	}
	
	if (is.na(file) & missing(oldnames) & missing(newnames)) {
	  changes <- edit(data.frame(oldnames=allnames, newnames=allnames, stringsAsFactors=FALSE))
	  write.csv(changes, file='saved fieldnames.csv', row.names=FALSE)
	  cat('old and new names saved in csv file called:   "saved fieldnames.csv"\n')
	  oldnames <- changes[ , 1]
	  newnames <- changes[ , 2]
	}

	if (!is.vector(oldnames) | any(!is.character(oldnames))) {stop('oldnames must be a vector of character type fieldnames\n')}
	if (!is.vector(newnames) | any(!is.character(newnames))) {stop('newnames must be a vector of character type fieldnames\n')}
	if (length(oldnames)!=length(newnames)) {stop('oldnames and newnames must be the same length\n')}
	if (length(allnames)==0 | length(oldnames)==0 | length(newnames)==0  ) {stop('no input can be length zero\n')}

	if (length(allnames) < length(oldnames)) {cat('Warning: length(allnames) generally should be >= length(oldnames)\n')}

	# Just replace the ones that match up, so 
	#   if allnames has something not in the oldnames, newnames entries, that is just left unchanged in allnames.
	#   if oldnames has something that is not in allnames, that is ignored.
	newnames <- newnames[oldnames %in% allnames]
	oldnames <- oldnames[oldnames %in% allnames]
	allnames[match(oldnames, allnames)] <- newnames
	return(allnames)

	################
	# Usage example
	oldnames <- c('PCTILE', 'REGION')
	newnames <- c('percentile', 'usregion')
	df <- data.frame(REGION=301:310, ID=1:10, PCTILE=101:110, OTHER=1:10)
	# examples:
	names(df) <- change.fieldnames(names(df), oldnames, newnames)
	names(df) <- change.fieldnames(names(df), "ID", "identification")
	names(df) <- change.fieldnames(names(df))
	names(df) <- change.fieldnames(names(df), 'saved fieldnames.csv')
}
