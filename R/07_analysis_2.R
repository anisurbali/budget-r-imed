## create table of expenditure analyis codewise
rm(list = ls())

df <- readRDS(here("data/final/budget_df.rds"))

# keep operating data only

df <- df %>% filter(type == 1)


df_table <- tibble(
  desc = character(),
  budget25_26 = numeric(),
  corrected25_26 = numeric(),
  budget26_27 = numeric(),
  change = numeric(),
  chnage_percent = numeric(),
  fd_advice = numeric(),
  advised_rate = character()
)


#------------------------------------------------------------------

## declare a function to add row

row_add_function <- function(df, df_table, code_range, row_name) {
  df_row <- df %>% filter(
    economic_code %in% code_range & is.na(activity_code)) %>% 
    select(c(3:5, 19:38)) %>% 
    summarise(across(everything(), ~sum(.x, na.rm = TRUE)))
  
  
  new_row = tibble(
    desc = row_name,
    budget25_26 = df_row$budget25_26,
    corrected25_26 = df_row$corrected25_26,
    budget26_27 = df_row$budget26_27,
    change = df_row$budget26_27 - df_row$corrected25_26,
    chnage_percent = ((df_row$budget26_27 - df_row$corrected25_26)/df_row$corrected25_26)*100,
    fd_advice = NA,
    advised_rate = NA
  )
  df_table <- bind_rows(df_table, new_row)
}

#--------------৩১. কর্মচারীদের প্রতিদান-------------

df_table <- add_row(df_table, desc = "৩১. কর্মচারীদের প্রতিদান")

#------------------------------------------------------------------
## add rows officer salary
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3111101),
  row_name = "কর্মকর্তাদের বেতন"
)



#------------------------------------------------------------------

## add rows officer salary
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3111101),
  row_name = "কর্মকর্তাদের বেতন"
)

#------------------------------------------------------------------
#------------------------------------------------------------------
## add rows chuti nogodayan

df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3111110),
  row_name = "ছুটি নগদায়ন বেতন (অফিসার)"
)
#------------------------------------------------------------------
## add rows employee salary

df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3111201),
  row_name = "কর্মচারীদের বেতন"
)
#------------------------------------------------------------------

## add rows employee chuti nogodayon

df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3111209),
  row_name = "ছুটি নগদায়ন বেতন (কর্মচারী)"
)

#------------------------------------------------------------------

## add rows bhatadi

df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3111301:3111352),
  row_name = "ভাতাদি"
)
#------------------------------------------------------------------

## add sub total row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3111101:3111352),
  row_name = "উপমোট :"
)

#---------------------------------------------------------------------

#--------------৩২. পণ্য ও সেবার ব্যবহারিদান-------------

df_table <- add_row(df_table, desc = "৩২. পণ্য ও সেবার ব্যবহার")

#--------------------------------------------


####################################################################
## see the budget 2027 with last 5 yrs actual data trend

df_new <- df %>% filter(
  economic_code %in% c(4111309:4112316) & is.na(activity_code)) %>% 
  select(c(economic_code, code_name, budget26_27), starts_with("actual")) 
###########################################################################


#---------------------------------------------------------------------

## add soroboraho seba row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3211102:3257306),
  row_name = "সরবরাহ সেবা"
)
#-----------------------------------------------------------------------


## add seminar conference row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3211111),
  row_name = "৩২১১১১১- সেমিনার/কনফারেন্স ব্যয়"
)
#-----------------------------------------------------------------------




## add nirikkha  row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3221101),
  row_name = "৩২২১১০১-নিরীক্ষা/সমীক্ষা ফি"
)

#---------------------------------------------------------------------

## add training row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3231301),
  row_name = "৩২৩১৩০১-প্রশিক্ষণ "
)
#-----------------------------------------------------------------------


#---------------------------------------------------------------------

## add training row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3244101),
  row_name = "৩২৪৪১০১-ভ্রমণ ব্যয়"
)
#-----------------------------------------------------------------------

## add soroboraho seba row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3258101:3258143),
  row_name = "মেরামত-সংরক্ষণ"
)
#-----------------------------------------------------------------------

#--------------------Motor jan Seba-------------------------------------------

## add row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3258140),
  row_name = "৩২৫৮১৪০-মোটরযান রক্ষণাবেক্ষণ ব্যয়"
)
#-----------------------------------------------------------------------

#--------------------অভ্যন্তরীণ শোভাবর্ধন-------------------------------------------

## add row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3258141),
  row_name = "৩২৫৮১৪১ - অভ্যন্তরীণ শোভাবর্ধন"
)
#-----------------------------------------------------------------------



#--------------------উপমোট :------------------------------------------

## add row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3211102:3258141),
  row_name = "উপমোট :"
)
#-----------------------------------------------------------------------



#------------------৩৯. সংরক্ষিত সাধারণ থোক বরাদ্দ----------------------------------

## add row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3911111),
  row_name = "৩৯. সংরক্ষিত সাধারণ থোক বরাদ্দ"
)
#-----------------------------------------------------------------------


#------------------৪১. অ-আর্থিক সম্পদদ্দ----------------------------------

## add row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(4111309:4112316),
  row_name = "৪১. অ-আর্থিক সম্পদ"
)
#-----------------------------------------------------------------------


#-----------------অভ্যন্তরীণ শোভাবর্ধন---------------------------------

## add row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(4111325),
  row_name = "৪১১১৩২৫ - অভ্যন্তরীণ শোভাবর্ধন"
)
#-----------------------------------------------------------------------



#-----------------মোটরযান---------------------------------

## add row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(4112101),
  row_name = "৪১১২১০১ - মোটরযান"
)
#-----------------------------------------------------------------------


#---------------মোট সচিবালয় পরিচালন ব্যয়-------------------------

## add row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3111101:4113302),
  row_name = "মোট সচিবালয় পরিচালন ব্যয়"
)
#-----------------------------------------------------------------------


## customize function for support activities

## declare a function to add row

row_add_function <- function(df, df_table, code_range, row_name) {
  df_row <- df %>% filter(
    economic_code %in% code_range & activity_code == "131021401") %>% 
    select(c(3:5, 19:32)) %>% 
    summarise(across(everything(), ~sum(.x, na.rm = TRUE)))
  
  
  new_row = tibble(
    desc = row_name,
    budget25_26 = df_row$budget25_26,
    corrected25_26 = df_row$corrected25_26,
    budget26_27 = df_row$budget26_27,
    change = df_row$budget26_27 - df_row$corrected25_26,
    chnage_percent = ((df_row$budget26_27 - df_row$corrected25_26)/df_row$corrected25_26)*100,
    fd_advice = NA,
    advised_rate = NA
  )
  df_table <- bind_rows(df_table, new_row)
}


#--------------সহায়তা কার্যক্রমিদান-------------

df_table <- add_row(df_table, desc = "সহায়তা কার্যক্রম")

#--------------------------------------------


#---------------সাহায্য মঞ্জুরি (বিপিপিএ)-----------------------------------

## add row
df_table <- row_add_function(
  df = df,
  df_table = df_table,
  code_range = c(3631101:3632106),
  row_name = "সাহায্য মঞ্জুরি (বিপিপিএ)"
)
#-----------------------------------------------------------------------

#---------------মোট পরিচালন ব্যয়পিএ)-----------------------------------

## add row
df_row <- df %>% 
  select(c(3:5, 19:32)) %>% 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)))


new_row = tibble(
  desc = "মোট পরিচালন ব্যয়",
  budget25_26 = df_row$budget25_26,
  corrected25_26 = df_row$corrected25_26,
  budget26_27 = df_row$budget26_27,
  change = df_row$budget26_27 - df_row$corrected25_26,
  chnage_percent = ((df_row$budget26_27 - df_row$corrected25_26)/df_row$corrected25_26)*100,
  fd_advice = NA,
  advised_rate = NA
)
df_table <- bind_rows(df_table, new_row)

#-----------------------------------------------------------------------

saveRDS(df_table, here("data/final/op_analysis_table4.rds"))


