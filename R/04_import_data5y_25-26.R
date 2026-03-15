
## import form 8b1
rm(list = ls())

library(here)

source(here("R", "01_load_packages.R"))



raw_data <- read_excel(here(drive, "data/expenditure_detail_26_27.xls"))


## rename the columns

colnames(raw_data) <- c("economic_code", "code_name",
                        "budget26_27", "corrected25_26", "actual26_27",
                        "budget25_26", "corrected24_25", "actual25_26",
                        "budget24_25", "corrected23_24", "actual24_25",
                        "budget23_24", "corrected22_23", "actual23_24",
                        "budget22_23", "corrected21_22", "actual22_23"
                        )




raw_data <- raw_data %>% mutate(
  type = case_when(
    economic_code == "পরিচালন কার্যক্রম" ~ 1,
    economic_code == "উন্নয়ন কার্যক্রম" ~ 2,
    .default =  NA
  )
)





## make column for office names


raw_data <- raw_data %>% 
  mutate(
    inst_code = ifelse(
      substr(raw_data$economic_code, 1, 3) == "114",
      raw_data$economic_code,
      NA
    ),
    
    inst_name = ifelse(
      substr(raw_data$economic_code, 1, 3) == "114",
      raw_data$code_name,
      NA
    ),
    
    activity_code = ifelse(
      substr(raw_data$economic_code, 1, 1) %in% c("1", "2") &
        nchar(raw_data$economic_code)==9,
      raw_data$economic_code,
      NA
    ),
    
    activity_name = ifelse(
      substr(raw_data$economic_code, 1, 1) %in% c("1", "2") &
        nchar(raw_data$economic_code)==9,
      raw_data$code_name,
      NA
    )
  )

## fill down the institution code and name for all rows
raw_data <- raw_data %>% 
  fill(c(inst_code, inst_name, type), .direction = "down")

## fill the activity code by institution group

raw_data <- raw_data %>% 
  
  group_by(inst_code) %>% 
  fill(c(activity_code, activity_name), .direction = "down") %>% 
  ungroup()



## clean data by removing unncessary rows and commas

raw_data <- raw_data %>% 
  filter(nchar(raw_data$economic_code)==7 & !startsWith(raw_data$economic_code, "114"))


## check if any value *failed to decode utf16*
raw_data %>% 
  filter(if_any(everything(), ~ str_starts(., "\\*failed to decode utf16\\*")))


raw_data <- raw_data %>% 
  mutate( across(everything(), ~ str_remove_all(., "\\*failed to decode utf16\\*")))

## remove commas from numbers
raw_data <- raw_data %>%
  mutate(across(3:18,
                ~as.numeric(str_replace_all(., ",", ""))))


## save the data
saveRDS(raw_data, here("data/processed/exp_5yr.rds"))


#---------------------------------------------------------------------------
#             Import  detail expenditure data from 24-25
#----------------------------------------------------------------------------


raw_data <- read_excel(here(drive, "data/expenditure_detail_24_25.xls"))

# keep data of 20-21 and 21-22

raw_data <- raw_data %>% select(c(1,2, 12:15, 17))


## rename the columns

colnames(raw_data) <- c("economic_code", "code_name",
                        "budget21_22", "corrected20_21", "actual21_22",
                        "budget20_21",                  "actual20_21"
                        
)



raw_data <- raw_data %>% mutate(
  type = case_when(
    economic_code == "পরিচালন কার্যক্রম" ~ 1,
    economic_code == "উন্নয়ন কার্যক্রম" ~ 2,
    .default =  NA
  )
)



## make column for office names


raw_data <- raw_data %>% 
  mutate(
    inst_code = ifelse(
      substr(raw_data$economic_code, 1, 3) == "114",
      raw_data$economic_code,
      NA
    ),
    
    inst_name = ifelse(
      substr(raw_data$economic_code, 1, 3) == "114",
      raw_data$code_name,
      NA
    ),
    
    activity_code = ifelse(
      substr(raw_data$economic_code, 1, 1) %in% c("1", "2") &
        nchar(raw_data$economic_code)==9,
      raw_data$economic_code,
      NA
    ),
    
    activity_name = ifelse(
      substr(raw_data$economic_code, 1, 1) %in% c("1", "2") &
        nchar(raw_data$economic_code)==9,
      raw_data$code_name,
      NA
    )
  )

## fill down the institution code and name for all rows
raw_data <- raw_data %>% 
  fill(c(inst_code, inst_name, type), .direction = "down")

## fill the activity code by institution group

raw_data <- raw_data %>% 
  group_by(inst_code) %>% 
  fill(c(activity_code, activity_name), .direction = "down") %>% 
  ungroup()



## clean data by removing unncessary rows and commas

raw_data <- raw_data %>% 
  filter(nchar(raw_data$economic_code)==7 & !startsWith(raw_data$economic_code, "114"))


## check if any value *failed to decode utf16*
raw_data %>% 
  filter(if_any(everything(), ~ str_starts(., "\\*failed to decode utf16\\*")))


raw_data <- raw_data %>% 
  mutate( across(everything(), ~ str_remove_all(., "\\*failed to decode utf16\\*")))

## remove commas from numbers
raw_data <- raw_data %>%
  mutate(across(3:7,
                ~as.numeric(str_replace_all(., ",", ""))))


## save the data
saveRDS(raw_data, here("data/processed/exp_5yr24_25.rds"))
