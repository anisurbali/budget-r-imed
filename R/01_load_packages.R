
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

if (here() == "C:/Users/bmani/GitHub/budget-r-imed") {
  drive <- "C:/Users/bmani/OneDrive/Finance Division/tripartite_meeting26/115 IMED/"
  # font_path <- "C:/Users/bmani/AppData/Local/Microsoft/Windows/Fonts/"
  
} else if (Sys.getenv("USERNAME") == "Shihab") {
  
  drive <- "C:/Users/User/OneDrive/Finance Division/tripartite_meeting26/114 PD/"
  # font_path <- "C:/Users/Md. Mamunul Karim/AppData/Local/Microsoft/Windows/Fonts"
  
} else {
  "Device Not available"
}

