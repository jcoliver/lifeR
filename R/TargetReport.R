#' Create report for sites to target
#'
TargetReport <- function(centers,
                         key_file,
                         list_file, # TODO: Maybe make this null and add functionality
                         center_names = NULL,
                         dist = 50,
                         back = 4,
                         hotspot = TRUE,
                         include_provisional = FALSE,
                         max_tries = 5,
                         timeout_sec = 30,
                         verbose = TRUE,
                         drop_patterns = c("sp.", "/", "Domestic type", "hybrid"),
                         max_sites = 5) {

  # Grab things we need (or stop) and qa/qc
  # centers vector or matrix make sure they're realistic

  # key_file

  # list_file (may be optional later on)

  # Read in user's list.

  # SplitNames

  # RecentNearby

  # DropPatterns

  # The below needs to run for EACH center

    # do set difference between user's list and result of RecentNearby post-drop

    # For each species remaining, run
    # RecentNearbySpecies
    # combine all this stuff, identify the targets, do some tidyverse here before
    # sending to Target report; will probably need to just create a list (or
    # fancier object) with name, lat/long, results of set diff

  # end iteration over each center

  # Using template, pass info to an RMarkdown template file

  # Will need to grab rmarkdown template that lives in inst/rmd like this:
  # report_template <- system.file("rmd", "Targets.Rmd", package = "lifeR")
  # See https://stackoverflow.com/questions/30377213/how-to-include-rmarkdown-file-in-r-package
  # and https://r-pkgs.org/inst.html
  # Parameters can be passed through params argument of rmarkdown::render
  # See https://rmarkdown.rstudio.com/lesson-6.html and
  # https://bookdown.org/yihui/rmarkdown/params-knit.html
  # rmarkdown::render(input = report_template, output_format = "html")
  # TODO: It remains an open question of whether parameters passed to params in
  # rmarkdown::render MUST be declared in the YAML header. Easy to test, though.




}
