## Resubmission
This is a resubmission. In this version I have:

* Updated reference to eBird API in DESCRIPTION to be in single quotes, 'eBird'

* Added return value to print.recent_obs and summary.recent_obs functions

* Replaced `:::` with `::` in MapSites function example

* Removed example from unexported function CoordInBounds

# R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: ‘knitr’
    All declared Imports should be used.

The knitr package is used to build vignettes.

# Downstream dependencies

There are no downstream dependencies.