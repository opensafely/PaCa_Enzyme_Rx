
## library
library(tidyverse)
library(here)
library(MASS)
library(rmarkdown)

rmarkdown::render(here::here("analysis", "Report_html.Rmd"), 
                  output_file = "Report_file_Oct2023.html", 
                  output_dir = "output")
