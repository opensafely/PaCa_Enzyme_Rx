
## library
library(tidyverse)
library(here)
library(MASS)
library(rmarkdown)

rmarkdown::render(here::here("analysis", "Report_test1.Rmd"), 
                  output_file = "Test1_R.html", output_dir = "output")
