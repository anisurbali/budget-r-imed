## import form 8b1
rm(list = ls())

library(here)

source(here("R", "01_load_packages.R"))

raw_data <- read_excel(
  here(drive, "data/raw/bc1_form_8b2_development_detailsadp_e1y2dzkui.xls"),
  sheet = 1)


#keep required columns
raw_data <- raw_data %>% 
  select(c(1,2,8:16))


## rename the columns

colnames(raw_data) <- c("economic_code", "code_name",
                        "budget26_27gob", "budget26_27rpag", "budget26_27rpas", "budget26_27dpa",
                        "budget26_27", "budget27_28gob",
                        "budget27_28", "budget28_29gob",
                        "budget28_29")

## create new columns for project code and inst code

raw_data <- raw_data %>% 
  mutate(
    activity_code = ifelse(
      substr(raw_data$economic_code, 1, 1) == "2" &
        nchar(raw_data$economic_code)==9,
      raw_data$economic_code,
      NA
    ),
    activity_name = ifelse(
      substr(raw_data$economic_code, 1, 1) == "2" &
        nchar(raw_data$economic_code)==9,
      raw_data$code_name,
      NA
    ),
    type = 2
  )

## fill down the institution code and name for all rows
raw_data <- raw_data %>% 
  fill(c(activity_code, activity_name), .direction = "down")


## keep only economic code rows

raw_data <- raw_data %>% 
  filter(nchar(raw_data$economic_code)==7)



raw_data <- raw_data %>%
  mutate(across(3:11,
                ~as.numeric(str_replace_all(., ",", ""))))

## save data
saveRDS(raw_data, here("data/processed/form8b1.rds"))
