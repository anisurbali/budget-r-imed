
rm(list = ls())

## open the clean datasets

op_def <- readRDS(here("data/final/operating_df.rds"))
dev_df <- readRDS(here("data/final/development_df.rds"))

budget_df <- readRDS(here("data/final/budget_df.rds"))

budget_df <- budget_df %>% select(-additional)

# sums of columns in crore taka
sum_df <- budget_df %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)/10000))

# sum operating budget

sum_op <- budget_df %>% 
  filter(budget_type == "operating") %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)/10000))

## sum development budget

sum_dev <- budget_df %>% 
  filter(budget_type == "development") %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)/10000))


## make a dataset for the budget at a glance table


budget_glance <- data.frame(
  description = character(),
  actual23_24 = numeric(),
  actual24_25 = numeric(),
  budget25_26 = numeric(),
  corrected25_26 = numeric(),
  budget26_27 = numeric(),
  projection27_28 = numeric(),
  projection28_29 = numeric(),
  stringsAsFactors = FALSE
)



## functions for percentage chang

rate_change <- function(current, previous) {
  
  ((current / previous) - 1) * 100
}

####################################################################
##          পরিসংখ্যান ও তথ্য ব্যবস্থাপনা বিভাগের বাজেট সংক্রান্ত তথ্য
######################################################################


## add total budget row

budget_glance <- budget_glance %>% 
  add_row(description = "মন্ত্রণালয়/বিভাগের বাজেট ",
          actual23_24 = sum_df$actual23_24,
          actual24_25 = sum_df$actual24_25,
          budget25_26 = sum_df$budget25_26,
          corrected25_26 = sum_df$corrected25_26,
          budget26_27 = sum_df$budget26_27,
          projection27_28 = sum_df$projection27_28,
          projection28_29 = sum_df$projection28_29
          
          ) %>% 
  add_row(description = "বাজেটের বছরভিত্তিক প্র্রবৃদ্ধি (শতাংশে)",
          actual23_24 = rate_change(sum_df$actual23_24, sum_df$actual22_23),
          actual24_25 = rate_change(sum_df$actual24_25, sum_df$actual23_24),
          budget25_26 = rate_change(sum_df$budget25_26, sum_df$budget24_25),
          corrected25_26 = rate_change(sum_df$corrected25_26, sum_df$corrected24_25),
          budget26_27 = rate_change(sum_df$budget26_27, sum_df$budget25_26),
          projection27_28 = rate_change(sum_df$projection27_28, sum_df$budget26_27),
          projection28_29 = rate_change(sum_df$projection28_29, sum_df$projection27_28)
          
        ) %>% 
  add_row(
          description = "মন্ত্রণালয়ের পরিচালন বাজেট (কোটি টাকায়)",
          actual23_24 = sum_op$actual23_24,
          actual24_25 = sum_op$actual24_25,
          budget25_26 = sum_op$budget25_26,
          corrected25_26 = sum_op$corrected25_26,
          budget26_27 = sum_op$budget26_27,
          projection27_28 = sum_op$projection27_28,
          projection28_29 = sum_op$projection28_29
        ) 
    
## create a custom row for adding
custom_row <- tibble(
  description = "পরিচালন বাজেটের বছরভিত্তিক শতকরা প্র্রবৃদ্ধি",
  actual23_24 = rate_change(sum_op$actual23_24, sum_op$actual22_23),
  actual24_25 = rate_change(sum_op$actual24_25, sum_op$actual23_24),
  budget25_26 = rate_change(sum_op$budget25_26, sum_op$budget24_25),
  corrected25_26 = rate_change(sum_op$corrected25_26, sum_op$corrected24_25),
  budget26_27 = rate_change(sum_op$budget26_27, sum_op$budget25_26),
  projection27_28 = rate_change(sum_op$projection27_28, sum_op$budget26_27),
  projection28_29 = rate_change(sum_op$projection28_29, sum_op$projection27_28)
)

## add the custom row to the budget_glance dataset
budget_glance <- bind_rows(budget_glance, custom_row)



## create a custom row for adding

custom_row <- tibble(
  description = "মন্ত্রণালয়ের উন্নয়ন বাজেট (কোটি টাকায়)",
  actual23_24 = sum_dev$actual23_24,
  actual24_25 = sum_dev$actual24_25,
  budget25_26 = sum_dev$budget25_26,
  corrected25_26 = sum_dev$corrected25_26,
  budget26_27 = sum_dev$budget26_27,
  projection27_28 = sum_dev$projection27_28,
  projection28_29 = sum_op$projection28_29
)

## add the custom row to the budget_glance dataset
budget_glance <- bind_rows(budget_glance, custom_row)



## create a custom row for adding

custom_row <- tibble(
  description = "মোট বাজেটে পরিচালন বাজেটের অংশ",
  actual23_24 = sum_op$actual23_24 / sum_df$actual23_24 * 100,
  actual24_25 = sum_op$actual24_25 / sum_df$actual24_25 * 100,
  budget25_26 = sum_op$budget25_26 / sum_df$budget24_25 * 100,
  corrected25_26 = sum_op$corrected25_26 / sum_df$corrected24_25 * 100,
  budget26_27 = sum_op$budget26_27 / sum_df$budget25_26 * 100,
  projection27_28 = sum_op$projection27_28 / sum_df$budget26_27 * 100,
  projection28_29 = sum_op$projection28_29 / sum_df$projection27_28 * 100
)

## add the custom row to the budget_glance dataset
budget_glance <- bind_rows(budget_glance, custom_row)



## create a custom row for adding

custom_row <- tibble(
  description = "মোট বাজেটে উন্নয়ন বাজেটের অংশ",
  actual23_24 = sum_dev$actual23_24 / sum_df$actual23_24 * 100,
  actual24_25 = sum_dev$actual24_25 / sum_df$actual24_25 * 100,
  budget25_26 = sum_dev$budget25_26 / sum_df$budget24_25 * 100,
  corrected25_26 = sum_dev$corrected25_26 / sum_df$corrected24_25 * 100,
  budget26_27 = sum_dev$budget26_27 / sum_df$budget25_26 * 100,
  projection27_28 = sum_dev$projection27_28 / sum_df$budget26_27 * 100,
  projection28_29 = sum_dev$projection28_29 / sum_df$projection27_28 * 100
)

## add the custom row to the budget_glance dataset
budget_glance <- bind_rows(budget_glance, custom_row)


##########################################################################
## পরিসংখ্যান ও তথ্য ব্যবস্থাপনা বিভাগের বাজেট ও জিডিপি সংক্রান্ত তথ্য
#########################################################################


## create a custom row for adding

custom_row <- tibble(
  description = "চলতি মূল্যে জিডিপি (কোটি টাকায়)",
  actual23_24 = NA,
  actual24_25 = sum_dev$actual24_25 / sum_df$actual24_25 * 100,
  budget25_26 = sum_dev$budget25_26 / sum_df$budget24_25 * 100,
  corrected25_26 = sum_dev$corrected25_26 / sum_df$corrected24_25 * 100,
  budget26_27 = sum_dev$budget26_27 / sum_df$budget25_26 * 100,
  projection27_28 = sum_dev$projection27_28 / sum_df$budget26_27 * 100,
  projection28_29 = sum_dev$projection28_29 / sum_df$projection27_28 * 100
)

## add the custom row to the budget_glance dataset
budget_glance <- bind_rows(budget_glance, custom_row)


##

custom_row <- tibble(
  description = "জিডিপি’র প্র্রবৃদ্ধি (নামিক)",
  actual23_24 = 5,
  actual24_25 = 5,
  budget25_26 = 10,
  corrected25_26 = 10,
  budget26_27 = 10,
  projection27_28 = 10,
  projection28_29 = 10
)

## add the custom row to the budget_glance dataset
budget_glance <- bind_rows(budget_glance, custom_row)



custom_row <- tibble(
  description = "জিডিপি’র তুলনায় মন্ত্রণালয়ের মোট বাজেটের শতকরা হার",
  actual23_24 = budget_glance$actual23_24[1]/budget_glance$actual23_24[8]*100,
  actual24_25 = budget_glance$actual24_25[1]/budget_glance$actual24_25[8]*100,
  budget25_26 = budget_glance$budget25_26[1]/budget_glance$budget25_26[8]*100,
  corrected25_26 = budget_glance$corrected25_26[1]/budget_glance$corrected25_26[8]*100,
  budget26_27 = budget_glance$budget26_27[1]/budget_glance$budget26_27[8]*100,
  projection27_28 = budget_glance$projection27_28[1]/budget_glance$budget26_27[8],
  projection28_29 = budget_glance$projection28_29[1]/budget_glance$budget28_29[8]*100
)

## add the custom row to the budget_glance dataset
budget_glance <- bind_rows(budget_glance, custom_row)




## create a custom row for adding
custom_row <- tibble(
  description = "জাতীয় বাজেটের তুলনায় মন্ত্রণালয়ের বাজেট (শতকরা হারে)",
  actual23_24 = budget_glance$actual23_24[1] / budget_glance$actual23_24[5] * 100,
  actual24_25 = budget_glance$actual24_25[1] / budget_glance$actual24_25[5] * 100,
  budget25_26 = budget_glance$budget25_26[1] / budget_glance$budget25_26[5] * 100,
  corrected25_26 = budget_glance$corrected25_26[1] / budget_glance$corrected25_26[5] * 100,
  budget26_27 = budget_glance$budget26_27[1] / budget_glance$budget26_27[5] * 100,
  projection27_28 = budget_glance$projection27_28[1] / budget_glance$projection27_28[5] * 100,
  projection28_29 = budget_glance$projection28_29[1] / budget_glance$projection28_29[5] * 100
)

## add the custom row to the budget_glance dataset
budget_glance <- bind_rows(budget_glance, custom_row)



###############################################################################
####          পরিসংখ্যান ও তথ্য ব্যবস্থাপনা বিভাগের বাজেট ও মোট বাজেটের তুলনা
##############################################################################


## create a custom row for adding
custom_row <- tibble(
  description = "জাতীয় বাজেট (কোটি টাকায়)",
  actual23_24 = 611020,
  actual24_25 = budget_glance$actual24_25[1] / budget_glance$actual24_25[5] * 100,
  budget25_26 = budget_glance$budget25_26[1] / budget_glance$budget25_26[5] * 100,
  corrected25_26 = budget_glance$corrected25_26[1] / budget_glance$corrected25_26[5] * 100,
  budget26_27 = budget_glance$budget26_27[1] / budget_glance$budget26_27[5] * 100,
  projection27_28 = budget_glance$projection27_28[1] / budget_glance$projection27_28[5] * 100,
  projection28_29 = budget_glance$projection28_29[1] / budget_glance$projection28_29[5] * 100
)

## add the custom row to the budget_glance dataset
budget_glance <- bind_rows(budget_glance, custom_row)



## create a custom row for adding
custom_row <- tibble(
  description = "জাতীয় বাজেটের তুলনায় মন্ত্রণালয়ের বাজেট (শতকরা হারে)",
  actual23_24 = budget_glance$actual23_24[1],
  actual24_25 = budget_glance$a,
  budget25_26 = budget_glance$budget25_26[1] / budget_glance$budget25_26[5] * 100,
  corrected25_26 = budget_glance$corrected25_26[1] / budget_glance$corrected25_26[5] * 100,
  budget26_27 = budget_glance$budget26_27[1] / budget_glance$budget26_27[5] * 100,
  projection27_28 = budget_glance$projection27_28[1] / budget_glance$projection27_28[5] * 100,
  projection28_29 = budget_glance$projection28_29[1] / budget_glance$projection28_29[5] * 100
)

## add the custom row to the budget_glance dataset
budget_glance <- bind_rows(budget_glance, custom_row)


## create a custom row for adding
custom_row <- tibble(
  description = "জাতীয় বাজেটে পরিচালন বাজেটের অংশ",
  actual23_24 = 1000,
  actual24_25 = 1000,
  budget25_26 = 1000,
  corrected25_26 = 100,
  budget26_27 = 100,
  projection27_28 = 100,
  projection28_29 = 100
)

## add the custom row to the budget_glance dataset
budget_glance <- bind_rows(budget_glance, custom_row)



## create a custom row for adding
custom_row <- tibble(
  description = "জাতীয় বাজেটের পরিচালন বাজেটের তুলনায় মন্ত্রণালয়ের পরিচালন বাজেটের হার (শতাংশে) ",
  actual23_24 = budget_glance$actual23_24[3]/budget_glance$actual23_24[13]*100,
  actual24_25 = budget_glance$actual24_25[3]/budget_glance$actual24_25[13]*100,
  budget25_26 = budget_glance$budget25_26[3]/budget_glance$budget25_26[13]*100,
  corrected25_26 = budget_glance$corrected25_26[3]/budget_glance$corrected25_26[13]*100,
  budget26_27 = budget_glance$budget26_27[3]/budget_glance$budget26_27[13]*100,
  projection27_28 = budget_glance$projection27_28[3]/budget_glance$projection27_28[13]*100,
  projection28_29 = budget_glance$projection28_29[3]/budget_glance$projection28_29[13]*100
)

## add the custom row to the budget_glance dataset
budget_glance <- bind_rows(budget_glance, custom_row)




custom_row <- tibble(
  description = "জাতীয় বাজেটে উন্নয়ন বাজেটের অংশ",
  actual23_24 = 1000,
  actual24_25 = 1000,
  budget25_26 = 1000,
  corrected25_26 = 100,
  budget26_27 = 100,
  projection27_28 = 100,
  projection28_29 = 100
)

## add the custom row to the budget_glance dataset
budget_glance <- bind_rows(budget_glance, custom_row)


## create a custom row for adding
custom_row <- tibble(
  description = "জাতীয় বাজেটের তুলনায় মন্ত্রণালয়ের উন্নয়ন বাজেটের হার (শতাংশে) (শতাংশে) ",
  actual23_24 = budget_glance$actual23_24[5]/budget_glance$actual23_24[15]*100,
  actual24_25 = budget_glance$actual24_25[5]/budget_glance$actual24_25[15]*100,
  budget25_26 = budget_glance$budget25_26[5]/budget_glance$budget25_26[15]*100,
  corrected25_26 = budget_glance$corrected25_26[5]/budget_glance$corrected25_26[15]*100,
  budget26_27 = budget_glance$budget26_27[5]/budget_glance$budget26_27[15]*100,
  projection27_28 = budget_glance$projection27_28[5]/budget_glance$projection27_28[15]*100,
  projection28_29 = budget_glance$projection28_29[5]/budget_glance$projection28_29[15]*100
)

## add the custom row to the budget_glance dataset
budget_glance <- bind_rows(budget_glance, custom_row)


saveRDS(budget_glance, here("data/final/budget_glance.rds"))



#############################################################################
## graph five years budget
##########################################################################









