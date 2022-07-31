% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SplitNames.R
\name{SplitNames}
\alias{SplitNames}
\title{Split vector of names into two-column data frame}
\usage{
SplitNames(x, delim = " - ")
}
\arguments{
\item{x}{Vector of species names, in the format "Common Name - Scientific
name".}

\item{delim}{Character separator that delimits common from scientific names.}
}
\value{
A data.frame of two columns, \code{Common} and \code{Scientific}.
}
\description{
Split vector of names into two-column data frame
}
\details{
Names from eBird are returned in a single column as: 
"Snow Goose - Anser caerulescens". This function provides a means of 
separating the common name ("Snow Goose") from the scientific name ("Anser 
caerulescens") into two separate columns.
}
\examples{
# Read in data downloaded from eBird
user_file <- system.file("extdata", "example-list.csv", package = "lifeR")
user_list <- read.csv(file = user_file)
# Retrieve a two-column data frame with common names and scientific names
species_seen <- SplitNames(x = user_list$Species)
# If only common names are required, refer to \code{Common} column
species_seen <- SplitNames(x = user_list$Species)$Common

}