rm(list = ls())

library(here)

source(here("R", "01_load_packages.R"))


#######################################################################
#               Form 7
###################################################################

raw_data <- read_excel(
  here(drive, "data/bc1_form_7_revenue_details.xls"),
  sheet = 1)

## keep necessary columns

raw_data <- raw_data %>% 
  select(-c(3:5, 9,10))


## rename the columns

colnames(raw_data) <- c("economic_code", "code_name",
                        "budget26_27",
                        "budget27_28",
                        "budget28_29")



## clean data by removing unncessary rows and commas

raw_data <- raw_data %>% 
  filter(nchar(raw_data$economic_code)==7 & !startsWith(raw_data$economic_code, "115"))


## check if any value *failed to decode utf16*
raw_data %>% 
  filter(if_any(everything(), ~ str_starts(., "\\*failed to decode utf16\\*")))



## remove commas from numbers
raw_data <- raw_data %>%
  mutate(across(3:5,
                ~as.numeric(str_replace_all(., ",", "")))) %>% 
  select(-code_name)


raw_data$economic_code = substr(raw_data$economic_code, 1, 5)



raw_data <- raw_data %>% group_by(economic_code) %>% 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)))


saveRDS(raw_data, here("data/processed/form7.rds"))

#######################################################################
#               Five years revenue 2026-27
###################################################################

raw_data <- read_excel(
  here(drive, "data/receipt_5_yrs_economicwise_26_27.xls"),
  sheet = 1)


## keep necessary columns

raw_data <- raw_data %>% 
  select(-c(2:4))



## rename the columns

colnames(raw_data) <- c("code",
                        "budget25_26", "corrected25_26", "actual25_26",
                        "budget24_25", "corrected24_25", "actual24_25",
                        "budget23_24", "corrected23_24", "actual23_24",
                        "budget22_23", "corrected22_23", "actual22_23")


## keep rows

raw_data <- raw_data %>% filter(str_detect(code, "^\\d{5} -")) %>% 
  mutate(
    economic_code = substr(code, 1, 5),
    code_name = substr(code, 9, nchar(code))
  ) %>% 
  select(-code) %>% 
  mutate(across(1:12,
                ~as.numeric(str_replace_all(., ",", ""))))




saveRDS(raw_data, here("data/processed/rev5yr26_27.rds"))

#######################################################################
#               Five years revenue 2024-25
###################################################################

raw_data <- read_excel(
  here(drive, "data/receipt_5_yrs_economicwise_24_25.xls"),
  sheet = 1)


## keep necessary columns

raw_data <- raw_data %>% 
  select(-c(2:10))



## rename the columns

colnames(raw_data) <- c("code",
                        "budget21_22", "corrected21_22", "actual21_22",
                        "budget20_21", "corrected20_21", "actual20_21"
                        )


## keep rows

raw_data <- raw_data %>% filter(str_detect(code, "^\\d{5} -")) %>% 
  mutate(
    economic_code = substr(code, 1, 5),
    code_name = substr(code, 9, nchar(code))
  ) %>% 
  select(-code)%>% 
  mutate(across(1:6,
                ~as.numeric(str_replace_all(., ",", ""))))




saveRDS(raw_data, here("data/processed/rev5yr24_25.rds"))



#######################################################################
#               Merge Datasets
###################################################################


df1 <- readRDS(here("data/processed/form7.rds"))
df2 <- readRDS(here("data/processed/rev5yr26_27.rds"))
df3 <- readRDS(here("data/processed/rev5yr24_25.rds"))



## merge df2 and df3

rev_df <- df2 %>% full_join(df3, by = c("economic_code", "code_name"))


## merge with df1

rev_df <- df1 %>% full_join(rev_df, by = "economic_code")


rev_df <- rev_df %>% select(
  economic_code, code_name, everything()
)

rev_df <- rev_df %>% mutate(
  across(c(3:23), ~.x/10000)
)

saveRDS(rev_df, here("data/final/rev_df.rds"))


