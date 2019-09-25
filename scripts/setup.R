# setup.R
# load libraries and source functions used in the project.

options(scipen = 999999999)

missing_packages <- setdiff(
  c("dplyr", "tidyr", "glue", "here", "openxlsx", "readr", "stringr", "knitr", "kableExtra"), installed.packages())

if (length(missing_packages) != 0) {
  warning("The following packages used in this project are missing: ", paste0(missing_packages, collapse = ", "))
}

# load libraries
library("tidyverse")
library("readxl")

#reporting
#library("knitr")
#library("rmarkdown")
library("kableExtra")

# utils
library("here")
library("glue")
if (!require(openxlsx)) {
  warning("Package openxlsx not available, cannot write output to xlsx.")
}

#alternative ways to load functions:
#devtools::load_all(".")

source(here("R", "calc-funs-ong.R"))
source(here("R", "format-funs.R"))
source(here("R", "npv-math-funs.R"))
source(here("R", "table-funs-ong.R"))
source(here("R", "xlsx-output.R"))



