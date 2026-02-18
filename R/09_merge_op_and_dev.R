## load data
df1 <- readRDS(here("data/final/operating_df.rds"))
df2 <- readRDS(here("data/final/development_df.rds"))

## add column marking operating or development


df1$budget_type = "operating"
df2$budget_type = "development"

budget_df <- bind_rows(df1, df2)

## save data
saveRDS(budget_df, here("data/final/budget_df.rds"))

