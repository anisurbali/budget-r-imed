
library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(dplyr)
library(stringr)
library(ggplot2)
library(showtext)
library(sysfonts)
library(stringr)
library(ragg)
library(writexl)


#----------------------------------------------
#project data location
# ----------------------------------------------

if (here() == "C:/Users/bmani/GitHub/budget-r-sid") {
  drive <- "G:/My Drive/Finance Division/Tripartite Meeting/tripartite-r-project-sid/"
  font_path <- "C:/Users/bmani/AppData/Local/Microsoft/Windows/Fonts/"
} else if ((here() == "C:/Users/Md. Mamunul Karim/Documents/GitHub/budget-r-sid")) {
  
  drive <- "G:/My Drive/Finance Division/Tripartite Meeting/tripartite-r-project-sid/"
  font_path <- "C:/Users/Md. Mamunul Karim/AppData/Local/Microsoft/Windows/Fonts"
  
} else {
  "Device Not available"
}

