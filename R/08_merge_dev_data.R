## load the dataset

df1 <- readRDS(here("data/processed/form8b1.rds"))
df2 <- readRDS(here("data/processed/exp_5yr_dev.rds"))

## full join the dataset

dev_df <- df1 %>% 
  full_join(df2, by = c("economic_code", "code_name", "project_code", "project_name"),
                    suffix = c("", ".y"))


## save data
saveRDS(dev_df, here("data/final/development_df.rds"))


