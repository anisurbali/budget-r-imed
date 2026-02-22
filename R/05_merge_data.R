rm(list = ls())

## merge operating data

## load three dataset

df_1 <- readRDS(here("data/processed/form8a1.rds"))
df_2 <- readRDS(here("data/processed/form8b1.rds"))
df_3 <- readRDS(here("data/processed/exp_5yr.rds"))
df_4 <- readRDS(here("data/processed/exp_5yr24_25.rds"))


operating_df <- bind_rows(df_1, df_2)


## join with 5 yr data
operating_df <- operating_df %>% 
  full_join(df_3, by = c("economic_code", "code_name", "inst_code", "inst_name",
                         "activity_code", "activity_name", "type"),
            suffix = c("", ".y"))



## join data from 24-25 5yr data
## join with 5 yr data
df_4$type <- as.numeric(df_4$type)
df_4$sub_type <- as.numeric(df_4$sub_type)


operating_df <- operating_df %>% 
  full_join(df_4, by = c("economic_code", "code_name", "inst_code", "inst_name",
                         "activity_code", "activity_name", "type", "sub_type"),
            suffix = c("", ".y"))


## convert the data into crore
## convert the data into crore

operating_df <- operating_df %>% 
  mutate(across(
    c(3:5, 12:32, 34:38), ~.x/10000
  ))



#save the data
saveRDS(operating_df, here("data/final/budget_df.rds"))
