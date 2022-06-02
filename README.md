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

# A minimalist example

```r
# Location of the file with your year list
list_file <- "~/Desktop/ebird_world_year_2021_list.csv"

# Read the list of species into memory
user_list <- read.csv(file = list_file)

# Extract the common names of species from your list
my_species <- SplitNames(x = user_list$Species)$Common

# Read in eBird API key from a text file; replace the argument to file with 
# the actual location of your eBird key file
key <- scan(file = "ebird-api-key.txt", what = "character")

# A single center requires vector of coordinates
# Change these, unless you really want to go birding near McCall, Idaho
locs <- c(45, -116)
SitesReport(centers = locs, 
            ebird_key = key, 
            species_seen = my_species)
```

## Miscellaneous debris

+ For early development of this project, also see [https://github.com/jcoliver/ebird-targets](https://github.com/jcoliver/ebird-targets).
+ For a more complete R wrapper for the eBird API, check out the 
[rebird](https://github.com/ropensci/rebird) package.