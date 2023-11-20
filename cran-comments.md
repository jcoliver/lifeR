# Resubmission
This is an update. In this version I have:

* Replaced ggmap with leaflet for creating maps

# R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTES:

* checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: ‘knitr’
    All declared Imports should be used.

The knitr package is used to build vignettes.

* checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

The remote software 'tidy' is not installed on local machine (it is not an R
package, but an external HTML validator)

# Downstream dependencies

There are no downstream dependencies.