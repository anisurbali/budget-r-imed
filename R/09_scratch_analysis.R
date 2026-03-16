## create table of expenditure analyis codewise
rm(list = ls())

df <- readRDS(here("data/final/budget_df.rds"))

## actual exp of salary and allowance



#----------------------------------------------------------------------------
#                   Create a function that will generate graphs for each code
#-------------------------------------------------------------------------------

showtext_auto(FALSE)

row_graph <- function(df, econcode, inst_value, path) {
  agg_png(paste0(path, econcode, ".png"), width = 8, height = 6, units = "in", res = 300)
  
  codename <- df  %>% filter(economic_code == econcode) %>% 
    select(code_name) %>% 
    slice(1) %>% pull()

  df1 <- df %>% filter(
    type == 1 & inst_code == inst_value & is.na(activity_code)
    & economic_code == econcode 
  ) %>% select(
    starts_with(c("actual", "budget", "corrected")) &
      !ends_with(c("gob", "rpag", "rpas", ".y", "dpa"))
  ) 
  
  budget26_27 <- ifelse(is.na(df1$budget26_27), 0, df1$budget26_27)   # for horizontal line
  
  df1 <- pivot_longer(df1, cols = everything(), names_to = "year", values_to = "value")
  
  
  df1 <- df1 %>% 
    extract(
      year,
      into = c("type", "year"),
      regex = "(actual|budget|corrected)(\\d+_\\d+)",
      remove = FALSE
    )
  
  df1 <- df1 %>% 
    mutate(across(where(is.numeric), ~replace_na(., 0)))
  
  
  p <- ggplot() +
    geom_line(data = df1 %>% filter(type %in% c("actual", "corrected") &
                                      !(year %in% c("26_27"))),
              aes(x = year, y = value, group = type, colour = type)) +
    
    geom_text(
      data = df1 %>%
        filter(type %in% c("actual", "corrected") & !(year %in% c("26_27"))) %>%
        mutate(label = sprintf("%.2f", value)),
      aes(x = year, y = value, label = label, colour = type),
      size = 5,
      vjust = -.3,
      show.legend = FALSE
    )+
    
    geom_hline(aes(yintercept = budget26_27, colour = "Budget 26-27"), 
               linetype = "solid") +
    
    geom_text(
      aes(x = "25_26", y = budget26_27,
          label = sprintf("%.2f", budget26_27)),
      colour = "red",
      vjust = -0.7,
      size = 5,
      inherit.aes = FALSE
    )+
    
    labs(
      title = paste(econcode, codename),
      color = "Legend",
      y = "টাকা (কোটি)"
  
      ) +
    
    scale_x_discrete(labels = c(
      "20_21" = "2020-21",
      "21_22" = "2021-22",
      "22_23" = "2022-23",
      "23_24" = "2023-24",
      "24_25" = "2024-25",
      "25_26" = "2025-26"
    ))+
    
    scale_colour_manual(
      values = c(
        "actual" = "blue",
        "corrected" = "green",
        "Budget 26-27" = "red"   # color for hline
      ),
      labels = c(
        "actual" = "প্রকৃত ব্যয়",
        "corrected" = "সংশোধিত বাজেট",
        "Budget 26-27" = "২০২৬-২৭ প্রস্তাবিত"
        )
      ) +
    
    scale_y_continuous(limits = c(0, NA)) +
      
    
    theme_minimal(base_family = "NikoshBAN")+
    theme(
      title = element_text(family = "NikoshBAN", size = 30),
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(family = "NikoshBAN", size = 15, colour = "black")
    )
  
  print(p)
  
  dev.off()
  
}

inst_value <- "1150101103364"
store_path <- "output/code_graphs/secretariat/"

all_codes <- df %>% filter(inst_code == inst_value & is.na(activity_code) &
                             budget26_27 != 0 &
                             !is.na(budget26_27)) %>% 
  select(economic_code) %>% pull()

for (code in all_codes) {
  row_graph(df, code, inst_value, store_path)
  print(code)
  
}



#------------------------------------------------------------------------------
#           BBS Analysis
#-----------------------------------------------------------------------------



inst_value <- "1160201103366"

# inst_value <- "1160101103365"
store_path <- "output/code_graphs/BBS/"

all_codes <- df %>% filter(inst_code == inst_value & is.na(activity_code) &
                             budget26_27 != 0 &
                             !is.na(budget26_27)) %>% 
  select(economic_code) %>% pull()

for (code in all_codes) {
  row_graph(df, code, inst_value, store_path)
  print(code)
  
}





#------------------------------------------------------------------------------
#           Divisional Offices and Subordinates
#-----------------------------------------------------------------------------


## list of divisional and subordinate offices

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


for (inst_value in div_office_list) {
  
  
  store_path <- paste0("output/code_graphs/", inst_value, "/")
  
  if (!dir.exists(store_path)) {
    dir.create(store_path)
    cat(sprintf("Directory '%s' created.\n", store_path))
  } 
  
  
  
  
  
  all_codes <- df %>% filter(inst_code == inst_value &
                               is.na(activity_code) &
                               budget26_27 != 0 &
                               !is.na(budget26_27)) %>%
    select(economic_code) %>% pull()
  
  # 
  
  for (code in all_codes) {
    row_graph(df, code, inst_value, store_path)
    print(code)
    
  }
  
}




