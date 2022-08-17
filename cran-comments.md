## Resubmission
This is a resubmission. In this version I have:

* Converted the DESCRIPTION title to title case.

* Revised DESCRIPTION description to not start with package name

* Included only CRAN BSD 2-clause LICENSE template

* Updated URLs that were redirects

* Consistently use https instead of http

* Consistently use trailing slashes in URLs

# R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: ‘knitr’
    All declared Imports should be used.

The knitr package is used to build vignettes.

# Downstream dependencies

There are no downstream dependencies.