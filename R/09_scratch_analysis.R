## create table of expenditure analyis codewise
rm(list = ls())

df <- readRDS(here("data/final/budget_df.rds"))

## actual exp of salary and allowance

df1 <- df %>% filter(str_starts(economic_code, "31")) %>% 
  select(starts_with("actual")) %>% 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)))


df2 <- df %>% filter(str_starts(economic_code, "31")) %>% 
  select(c(economic_code, code_name), starts_with(c("actual", "corrected", "budget"))) %>% 
  filter(economic_code == "3111335")


df3 <- df %>% filter(str_starts(economic_code, "32"), is.na(activity_code))%>% 
  select(c(economic_code, code_name), starts_with(c("actual", "corrected", "budget")))


df3 <- df3 %>% mutate(
  avg_actual = rowMeans(select(., starts_with("actual"), -actual26_27), na.rm = TRUE)
)

df3 <- df3 %>% select(economic_code, code_name, budget26_27, starts_with("actual"), avg_actual)



