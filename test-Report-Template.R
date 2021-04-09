# Testing creation of Report
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-04-02

rm(list = ls())
devtools::load_all()

results_list <- readRDS(file = "results-list.Rds")
report_details <- readRDS(file = "report-details.Rds")

report_template <- system.file("rmd", "Report-Template.Rmd", 
                               package = "lifeR")

output_format <- "html_document"  
rmarkdown::render(input = report_template, 
                  output_format = output_format,
                  output_file = "test-report-out", # knitr takes care of extension
                  output_dir = ".",
                  params = list(results_list = results_list,
                                report_details = report_details),
                  quiet = FALSE)

