# lifeR <img src="man/figures/logo.png" align="right" alt="lifeR logo" width="120">

An R package for identifying locations to visit in order to increase your 
species list count. The package relies on the 
[eBird API](https://documenter.getpostman.com/view/664302/S1ENwy59) to query 
for recent observations and compare them to a user's species list. The lists 
can be life lists, year lists, county lists, etc.

# Installation

Currently, only a development version is available (it should be available on 
CRAN soon). You can install this version from GitHub with the help of the 
remotes package

```r
install.packages("remotes")
remotes::install_github(repo = "jcoliver/lifeR")
```

If you want to have the introductory vignette included in the installation, 
then pass `build_vignettes = TRUE` in the call to `install_github()`:

```r
install.packages("remotes")
remotes::install_github(repo = "jcoliver/lifeR", build_vignettes = TRUE)
```

And load the package with `library`

```r
library("lifeR")
```

<!--
To open the vignette, run

```r
browseVignettes(package = "lifeR")
```
-->

## Miscellaneous debris

+ For early development of this project, also see [https://github.com/jcoliver/ebird-targets](https://github.com/jcoliver/ebird-targets).
+ For a more complete R wrapper for the eBird API, check out the 
[rebird](https://github.com/ropensci/rebird) package.