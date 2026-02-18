## import 5 years development data fro 2026-27

raw_data <- read_excel(here("data/raw/expenditure_detail_giluq4esa.xls"))


## rename the columns

colnames(raw_data) <- c("economic_code", "code_name",
                        "budget26_27", "corrected25_26", "actual26_27",
                        "budget25_26", "corrected24_25", "actual25_26",
                        "budget24_25", "corrected23_24", "actual24_25",
                        "budget23_24", "corrected22_23", "actual23_24",
                        "budget22_23", "corrected21_22", "actual22_23"
                        )



## make column for office names and project names

raw_data <- raw_data %>% 
  mutate(
    inst_code = ifelse(
      substr(raw_data$economic_code, 1, 3) == "115",
      raw_data$economic_code,
      NA
    ),
    
    inst_name = ifelse(
      substr(raw_data$economic_code, 1, 3) == "115",
      raw_data$code_name,
      NA
    ),
    
    project_code = ifelse(
      substr(raw_data$economic_code, 1, 1) == "2" &
        nchar(raw_data$economic_code)==9,
      raw_data$economic_code,
      NA
    ),
    
    project_name = ifelse(
      substr(raw_data$economic_code, 1, 1) == "2" &
        nchar(raw_data$economic_code)==9,
      raw_data$code_name,
      NA
    )
  )

## fill all rows with inst code and project code and name
raw_data <- raw_data %>% 
  fill(c(inst_code, inst_name, project_code, project_name), .direction = "down")


## clean data by removing unncessary rows

raw_data <- raw_data %>% 
  filter(nchar(raw_data$economic_code)==7 & substr(raw_data$economic_code,1,3)!="115")

## remove comma and convert to numeric
## remove commas from numbers
raw_data <- raw_data %>%
  mutate(across(3:17,
                ~as.numeric(str_replace_all(., ",", ""))))



## save the data
saveRDS(raw_data, here("data/processed/exp_5yr_dev.rds"))

