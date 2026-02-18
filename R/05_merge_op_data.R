## merge operating data

## load three dataset

df_1 <- readRDS(here("data/processed/form8a1.rds"))
df_2 <- readRDS(here("data/processed/exp_5yr_op_26.rds"))
df_3 <- readRDS(here("data/processed/exp_5yr_op_24.rds"))

operating_df <- df_1 %>% 
  full_join(df_2, by = c("economic_code", "inst_code", "activity_code"),
            suffix = c("", ".y")) %>%
  full_join(df_3,by = c("economic_code", "inst_code", "activity_code"),
            suffix = c("", ".z"))

## coalesce the duplicate columns

operating_df <- operating_df %>% 
  mutate(
    code_name = coalesce(code_name, code_name.y, code_name.z),
    inst_name = coalesce(inst_name, inst_name.y, inst_name.z),
    activity_name = coalesce(activity_name, activity_name.y, activity_name.z)
  ) %>% 
  select(-c(code_name.y, code_name.z, inst_name.y, inst_name.z,
            activity_name.y, activity_name.z),
         -ends_with(".y"), -ends_with(".z")) %>% 
  select(economic_code, code_name, inst_code, inst_name, activity_code,
         activity_name, everything())


#save the data
saveRDS(operating_df, here("data/final/operating_df.rds"))
