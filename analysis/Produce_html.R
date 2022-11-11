
## library
library(tidyverse)
library(here)
library(MASS)
library(rmarkdown)

rmarkdown::render(here::here("analysis", "Report_test1.Rmd"), 
                  output_file = "Test1.html", output_dir = "output")
