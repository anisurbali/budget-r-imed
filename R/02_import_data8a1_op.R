rm(list = ls())

raw_data <- read_excel(
  here("data/raw/bc1_form_8a1_operating_expenditure_detailsfield_office_9sih21rs8.xls"),
  sheet = 1)



## rename the columns

colnames(raw_data) <- c("economic_code", "code_name",
                        "budget25_26", "estimate26_27", "projection27_28",
                        "projection28_29", "additional")



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



##see special activity code

raw_data %>% 
  filter(nchar(raw_data$economic_code)==9) %>% unique()

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
  filter(nchar(raw_data$economic_code)==7 & !startsWith(raw_data$economic_code, "116"))


## check if any value *failed to decode utf16*
raw_data %>% 
  filter(if_any(everything(), ~ str_starts(., "\\*failed to decode utf16\\*")))


raw_data <- raw_data %>% 
  mutate( across(everything(), ~ str_remove_all(., "\\*failed to decode utf16\\*")))

## remove commas from numbers
raw_data <- raw_data %>%
  mutate(across(3:6,
                ~as.numeric(str_replace_all(., ",", ""))))


## keep first 07 digit of institution code if special activity


raw_data <- raw_data %>% 
  mutate(
    inst_code = if_else(is.na(raw_data$activity_code), raw_data$inst_code, 
                        substr(raw_data$inst_code,1,7))
  )


## save the data
saveRDS(raw_data, here("data/processed/form8a1.rds"))

