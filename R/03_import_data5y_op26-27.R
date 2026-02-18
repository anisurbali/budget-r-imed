## import 5 years operating data from 2026-27

raw_data <- read_excel(here("data/raw/expenditure_detail_bo4wbxxq7.xls"))


## rename the columns

colnames(raw_data) <- c("economic_code", "code_name",
                        "budget26_27", "corrected25_26", "actual26_27",
                        "budget25_26", "corrected24_25", "actual25_26",
                        "budget24_25", "corrected23_24", "actual24_25",
                        "budget23_24", "corrected22_23", "actual23_24",
                        "budget22_23", "corrected21_22", "actual22_23"
                        )


## make column for office names and activity names


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
    
    activity_code = ifelse(
      substr(raw_data$economic_code, 1, 1) == "1" &
        nchar(raw_data$economic_code)==9,
      raw_data$economic_code,
      NA
    ),
    
    activity_name = ifelse(
      substr(raw_data$economic_code, 1, 1) == "1" &
        nchar(raw_data$economic_code)==9,
      raw_data$code_name,
      NA
    )
  )


## fill down the institution code and name for all rows
raw_data <- raw_data %>% 
  fill(c(inst_code, inst_name), .direction = "down")


## fill the activity code by institution group

raw_data <- raw_data %>% 
  group_by(inst_code) %>% 
  fill(c(activity_code, activity_name), .direction = "down") %>% 
  ungroup()



## clean data by removing unncessary rows and commas

raw_data <- raw_data %>% 
  filter(nchar(raw_data$economic_code)==7 & !startsWith(raw_data$economic_code, "115"))


## check if any value *failed to decode utf16*
raw_data %>% 
  filter(if_any(everything(), ~ str_starts(., "\\*failed to decode utf16\\*")))


raw_data <- raw_data %>% 
  mutate( across(everything(), ~ str_remove_all(., "\\*failed to decode utf16\\*")))

## remove commas from numbers
raw_data <- raw_data %>%
  mutate(across(3:17,
                ~as.numeric(str_replace_all(., ",", ""))))


## save the data
saveRDS(raw_data, here("data/processed/exp_5yr_op_26.rds"))

