rm(list = ls())

library(here)

source(here("R", "01_load_packages.R"))

## merge operating data

## load three dataset

df_1 <- readRDS(here("data/processed/form8a1.rds"))
df_2 <- readRDS(here("data/processed/form8b1.rds"))
df_3 <- readRDS(here("data/processed/exp_5yr.rds"))
df_4 <- readRDS(here("data/processed/exp_5yr24_25.rds"))


operating_df <- bind_rows(df_1, df_2)


df_3$fund <- NA
df_4$fund <- NA


# duplicates_df <- operating_df %>%
#   group_by(economic_code, inst_code, activity_code, type, fund) %>% # Group by the columns of interest
#   filter(n() > 1) %>%    # Keep only groups with more than one row
#   ungroup()              # Remove grouping
# 
# print(duplicates_df)

df_3$inst_code <- as.character(df_3$inst_code)


## join with 5 yr data
operating_df <- operating_df %>% 
  full_join(df_3, by = c("economic_code", "code_name", "inst_code", "inst_name",
                         "activity_code", "activity_name", "type", "fund"),
            suffix = c("", ".y"))



## join data from 24-25 5yr data
## join with 5 yr data
df_4$type <- as.numeric(df_4$type)



operating_df <- operating_df %>% 
  full_join(df_4, by = c("economic_code", "code_name", "inst_code", "inst_name",
                         "activity_code", "activity_name", "type", "fund"),
            suffix = c("", ".y"))


## convert the data into crore
## convert the data into crore

operating_df <- operating_df %>% 
  mutate(across(
    c(3:5, 12:17, 19:38), ~.x/10000
  ))



#save the data
saveRDS(operating_df, here("data/final/budget_df.rds"))





