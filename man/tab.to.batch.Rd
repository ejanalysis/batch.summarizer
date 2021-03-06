% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab.to.batch.R
\name{tab.to.batch}
\alias{tab.to.batch}
\title{read tabular data files saved from a set of buffer reports - probably obsolete}
\usage{
tab.to.batch(files, folder = getwd(), ...)
}
\arguments{
\item{files}{character vector of filenames with full paths}

\item{folder}{default is working directory, searches for all files in this folder unless files param given}

\item{...}{pass more to tabs.compile}
}
\value{
the output is the output
}
\description{
This is a function to read a set of csv files saved from EJSCREEN tabular view
 and compile / format them and save them in format provided by output of batch processing tool
 so they can be uploaded by batch summarizer to be analyzed as if they had come from the batch processing tool.
 This allows users with access to EJSCREEN, but not the batch tool, to use the batch summarizer
 to get summary reports/ graphics on a group of buffer reports (i.e., standard reports on multiple points) (or block group reports if extended to that)
}
\author{
ejanalyst info@ejanalysis.com
}
