% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SplitNames.R
\name{SplitNames}
\alias{SplitNames}
\title{Split vector of names into two column data frame}
\usage{
SplitNames(x, delim = " - ")
}
\arguments{
\item{x}{vector of species names, in the format "Common Name - Scientific
name"}

\item{delim}{character separator}
}
\value{
data.frame of two columns, \code{Common} and \code{Scientific}
}
\description{
Split vector of names into two column data frame
}
\details{
Names from eBird come in a single column as: "Snow Goose - Anser
caerulescens". This function provides a means of separating the common name
(Snow Goose) from the scientific name (Anser caerulescens) into two separate
columns.
}
