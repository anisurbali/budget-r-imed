rm(list = ls())

## merge operating data

## load three dataset

df_1 <- readRDS(here("data/processed/form8a1.rds"))
df_2 <- readRDS(here("data/processed/form8b1.rds"))
df_3 <- readRDS(here("data/processed/exp_5yr.rds"))


operating_df <- bind_rows(df_1, df_2)


## join with 5 yr data
operating_df <- operating_df %>% 
  full_join(df_3, by = c("economic_code", "code_name", "inst_code", "inst_name",
                         "activity_code", "activity_name"),
            suffix = c("", ".y"))

## coalesce the duplicate columns

operating_df <- operating_df %>% 
  mutate(
    type = coalesce(type, type.y))


#save the data
saveRDS(operating_df, here("data/final/budget_df.rds"))
