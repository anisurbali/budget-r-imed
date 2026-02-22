
library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(dplyr)
library(stringr)
library(ggplot2)
library(showtext)
library(sysfonts)

#----------------------------------------------
#project data location
# ----------------------------------------------

if (here() == "C:/Users/bmani/GitHub/budget-r-imed") {
  data <- "G:/My Drive/Finance Division/Tripartite Meeting/tripartite-r-project-imed/data"
} else {
  "Not available"
}