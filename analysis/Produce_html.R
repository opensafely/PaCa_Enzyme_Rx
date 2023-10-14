
## library
library(tidyverse)
library(here)
library(MASS)
library(rmarkdown)

rmarkdown::render(here::here("analysis", "Report_html.Rmd"), 
                  output_file = "Report_file.html", 
                  output_dir = "output")
