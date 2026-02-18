## import form 8b1


raw_data <- read_excel(
  here("data/raw/bc1_form_8b2_development_detailsadp_e1y2dzkui.xls"),
  sheet = 1)


#keep required columns
raw_data <- raw_data %>% 
  select(c(1,2,7:16))


## rename the columns

colnames(raw_data) <- c("economic_code", "code_name",
                        "budget25_26", "estimate26_27gob", "estimate26_27rpag", "estimate26_27rpas", "estimate26_27dpa",
                        "budget26_27", "projetion27_28gob",
                        "projection27_28", "projection28_29gob",
                        "projection28_29")

## create new columns for project code and inst code

raw_data <- raw_data %>% 
  mutate(
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

## fill down the institution code and name for all rows
raw_data <- raw_data %>% 
  fill(c(project_code, project_name), .direction = "down")


## keep only economic code rows

raw_data <- raw_data %>% 
  filter(nchar(raw_data$economic_code)==7)



raw_data <- raw_data %>%
  mutate(across(3:12,
                ~as.numeric(str_replace_all(., ",", ""))))

## save data
saveRDS(raw_data, here("data/processed/form8b1.rds"))
