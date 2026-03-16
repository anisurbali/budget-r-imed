## create table of expenditure analyis codewise
rm(list = ls())
source(here("R/01_load_packages.R"), echo = TRUE)

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

row_add_function <- function(df, code_range, row_name = NULL) {
  

  
  df_row <- df %>% filter(
    economic_code %in% code_range) %>% 
    select(c(3:5, 19:38)) %>% 
    summarise(across(everything(), ~sum(.x, na.rm = TRUE)))
  
  
  if (is.null(row_name)) {
    row_name <- paste(code_range, "-", df$code_name[df$economic_code==code_range[1]][1] )   # your internal calculation
  } 
  
  tibble(
    desc = row_name,
    budget25_26 = df_row$budget25_26,
    corrected25_26 = df_row$corrected25_26,
    budget26_27 = df_row$budget26_27,
    change = df_row$budget26_27 - df_row$corrected25_26,
    chnage_percent = ((df_row$budget26_27 - df_row$corrected25_26)/df_row$corrected25_26)*100,
    fd_advice = NA,
    advised_rate = NA
  )
  
}


#------------------- Add rows ---------------------------------------------
#select the institution
df1 <- df %>% filter( inst_code == "1150101103364" &
  is.na(activity_code)
)

#

### add all rows

codes <- df %>% filter(inst_code == "1150101103364" & is.na(activity_code)) %>% 
  select(economic_code) 

# 
# 
# 
# ##      বিশেষ/সহায়তা কার্যক্রম 
# #----------------------------------------------------------------------------
# #select the institution
# df1 <- df %>% filter( inst_code == "1150101" &
#                         !is.na(activity_code)
# )


#-----------------------------------------------------------------------


# g <- "3211"
# 
# rows_to_add <- row_add_function(df1, c(paste0(g, "000"):paste0(g, "999")),
#                                 paste0(g, ": চাঁদা"
#                                 ))
# df_table <- bind_rows(df_table, rows_to_add)



#----------------------------------------------------------------------------
##############################################################################
#############  Secretariat ##################################################
##############################################################################
##############################################################################

#select the institution
df1 <- df %>% filter( inst_code == "1150101103364" &
                        is.na(activity_code)
)




rows_to_add <- row_add_function(df1, 0000, "সচিবালয়")

df_table <- bind_rows(df_table, rows_to_add)


#---------------------------------------------------------------------------

## group of codes

codes <- df %>% filter(inst_code == "1150101103364" & is.na(activity_code)) %>% 
  select(economic_code) 

group_df <- read_excel(here(drive, "data/raw/econ_group.xlsx"))
group_df <- distinct(group_df)






results <- list()

for (i in seq_len(nrow(group_df))) {
  results[[i]] <- list(
    code  = group_df$`Eco Group`[i],
    range = codes %>%
      filter(startsWith(economic_code, as.character(group_df$`Eco Group`[i]))) %>%
      pull(economic_code),
    name  = group_df$`Eco Group Name`[i]
  )
}


results[[6]]$name

for (row in results) {
  
  if (length(row$range) > 0) {
    
    print(row)
    # g <- "3258"
    
    rows_to_add <- row_add_function(df1, c(paste0(row$code, "00"):paste0(row$code, "99")),
                                    paste0(row$code," : ", row$name
                                    ))
    
    if (rows_to_add$budget26_27 != 0){
      df_table <- bind_rows(df_table, rows_to_add)
    }
    
    
    rows_to_add <- bind_rows(
      lapply(row$range, function(code) {
        row_add_function(df1, code)
      })
    )
    
    # rows_to_add <- rows_to_add %>% filter(budget26_27 != 0)
    
    df_table <- bind_rows(df_table, rows_to_add)
    
  }
  
}

#---------------------------------------------------------------------------


















#######################################################################
#-----------------------------------------------------------------------
##################################################################
############ BBS          BBS     BBS             #####################

######################################################################
########################################################################

#select the institution
df1 <- df %>% filter( inst_code == "1150101103364")




rows_to_add <- row_add_function(df1, 0000, "বিবিএস")

df_table <- bind_rows(df_table, rows_to_add)


#---------------------------------------------------------------------------

## group of codes

codes <- df %>% filter(inst_code == "1150101103364" ) %>% 
  select(economic_code) 

group_df <- read_excel(here(drive, "data/raw/econ_group.xlsx"))
group_df <- distinct(group_df)






results <- list()

for (i in seq_len(nrow(group_df))) {
  results[[i]] <- list(
    code  = group_df$`Eco Group`[i],
    range = codes %>%
      filter(startsWith(economic_code, as.character(group_df$`Eco Group`[i]))) %>%
      pull(economic_code),
    name  = group_df$`Eco Group Name`[i]
  )
}


results[[6]]$name

for (row in results) {
  
  if (length(row$range) > 0) {
    
    print(row)
    # g <- "3258"
    
    rows_to_add <- row_add_function(df1, c(paste0(row$code, "00"):paste0(row$code, "99")),
                                    paste0(row$code," : ", row$name
                                    ))
    
    if (rows_to_add$budget26_27 != 0){
      df_table <- bind_rows(df_table, rows_to_add)
    }
    
    
    rows_to_add <- bind_rows(
      lapply(row$range, function(code) {
        row_add_function(df1, code)
      })
    )
    
    # rows_to_add <- rows_to_add %>% filter(budget26_27 != 0)
    
    df_table <- bind_rows(df_table, rows_to_add)
    
  }
  
}



#######################################################################
#-----------------------------------------------------------------------
##################################################################
############ Divisional Offices             #####################

######################################################################
########################################################################

div_office_list <- list("1160202103367",
                        "1160202103368",
                        "1160202103369",
                        "1160202103370",
                        "1160202103371",
                        "1160202103372",
                        "1160202103373",
                        "1160202103374",
                        "1160203000000",
                        "1160204000000",
                        "1160205000000"
                        )





for (office in div_office_list){
  #select the institution
  df1 <- df %>% filter( inst_code == office)
  
  
  
  rows_to_add <- row_add_function(df1, 0000, df %>% 
  filter(inst_code == office) %>% select(inst_name) %>%
  slice(1) %>% pull())
  
  df_table <- bind_rows(df_table, rows_to_add)
  
  
  ## group of codes
  
  codes <- df %>% filter(inst_code == office ) %>% 
    select(economic_code) 
  
  group_df <- read_excel(here(drive, "data/raw/econ_group.xlsx"))
  group_df <- distinct(group_df)
  
  
  
  results <- list()
  
  for (i in seq_len(nrow(group_df))) {
    results[[i]] <- list(
      code  = group_df$`Eco Group`[i],
      range = codes %>%
        filter(startsWith(economic_code, as.character(group_df$`Eco Group`[i]))) %>%
        pull(economic_code),
      name  = group_df$`Eco Group Name`[i]
    )
  }
  
  
  results[[6]]$name
  
  for (row in results) {
    
    if (length(row$range) > 0) {
      
      print(row)
      # g <- "3258"
      
      rows_to_add <- row_add_function(df1, c(paste0(row$code, "00"):paste0(row$code, "99")),
                                      paste0(row$code," : ", row$name
                                      ))
      
      if (rows_to_add$budget26_27 != 0){
        df_table <- bind_rows(df_table, rows_to_add)
      }
      
      
      rows_to_add <- bind_rows(
        lapply(row$range, function(code) {
          row_add_function(df1, code)
        })
      )
      
      # rows_to_add <- rows_to_add %>% filter(budget26_27 != 0)
      
      df_table <- bind_rows(df_table, rows_to_add)
  
    }
  }
}







saveRDS(df_table, here("data/final/op_analysis_table4.rds"))




write_xlsx(df_table, here(drive, "output/tables/table4.xlsx"))


