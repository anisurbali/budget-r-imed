## create table of expenditure analyis codewise
rm(list = ls())
library(here)

source(here("R", "01_load_packages.R"))

df <- readRDS(here("data/final/budget_df.rds"))

## actual exp of salary and allowance


## import the fd advice column
# write_xlsx(budget, paste0(drive, "data/budget_inputxxxx.xlsx"))
fd <- readxl::read_xlsx(paste0(drive, "data/budget_input.xlsx"))
df$fd_advice = fd$fd_advice




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
    starts_with(c("actual", "budget", "corrected", "fd_advice")) &
      !ends_with(c("gob", "rpag", "rpas", ".y", "dpa"))
  ) 
  
  budget26_27 <- ifelse(is.na(df1$budget26_27), 0, df1$budget26_27)   # for horizontal line
  
  fd_rec <- ifelse(is.na(df1$fd_advice), 0, df1$fd_advice)   # for horizontal line
  
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
        mutate(label = sprintf("%.3f", value)),
      aes(x = year, y = value, label = label, colour = type),
      size = 5,
      vjust = -.3,
      show.legend = FALSE
    )+
    
    geom_hline(aes(yintercept = budget26_27, colour = "Budget 26-27"),  ## ministry proposal
               linetype = "solid") +
    
    geom_text(
      aes(x = "25_26", y = budget26_27,
          label = sprintf("%.3f", budget26_27)),
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
        "Budget 26-27" = "red",
        "FD Rec" = "#5478FF" # color for hline
      ),
      labels = c(
        "actual" = "প্রকৃত ব্যয়",
        "corrected" = "সংশোধিত বাজেট",
        "Budget 26-27" = "২০২৬-২৭ প্রস্তাবিত",
        "FD Rec" = "অর্থ বিভাগের সুপারিশ"
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


  if (fd_rec != budget26_27) {

    p <- p +
      geom_hline(aes(yintercept = fd_rec, colour = "FD Rec"),
                 linetype = "solid") +
      geom_text(
        aes(x = "25_26", y = fd_rec,
            label = sprintf("%.3f", fd_rec)),
        colour = "5478FF",
        vjust = -0.7,
        size = 5,
        inherit.aes = FALSE
      )
  }

  
  
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








###########################################################################
##           combined graphs
#############################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)


row_graph <- function(df, econcode, inst_value) {
  
  codename <- df %>% 
    filter(economic_code == econcode) %>% 
    select(code_name) %>% 
    slice(1) %>% 
    pull()
  
  
  df1 <- df %>% filter(
    type == 1 & inst_code == inst_value & is.na(activity_code)
    & economic_code == econcode 
  ) %>% select(
    starts_with(c("actual", "budget", "corrected", "fd_advice")) &
      !ends_with(c("gob", "rpag", "rpas", ".y", "dpa"))
  ) 
  
  budget26_27 <- ifelse(is.na(df1$budget26_27), 0, df1$budget26_27)   # for horizontal line
  
  fd_rec <- ifelse(is.na(df1$fd_advice), 0, df1$fd_advice)   # for horizontal line
  
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
        mutate(label = sprintf("%.3f", value)),
      aes(x = year, y = value, label = label, colour = type),
      size = 5,
      vjust = -.3,
      show.legend = FALSE
    )+
    
    geom_hline(aes(yintercept = budget26_27, colour = "Budget 26-27"),  ## ministry proposal
               linetype = "solid") +
    
    geom_text(
      aes(x = "25_26", y = budget26_27,
          label = sprintf("%.3f", budget26_27)),
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
        "Budget 26-27" = "red",
        "FD Rec" = "#5478FF" # color for hline
      ),
      labels = c(
        "actual" = "প্রকৃত ব্যয়",
        "corrected" = "সংশোধিত বাজেট",
        "Budget 26-27" = "২০২৬-২৭ প্রস্তাবিত",
        "FD Rec" = "অর্থ বিভাগের সুপারিশ"
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
  
  
  if (fd_rec != budget26_27) {
    
    p <- p +
      geom_hline(aes(yintercept = fd_rec, colour = "FD Rec"),
                 linetype = "solid") +
      geom_text(
        aes(x = "25_26", y = fd_rec,
            label = sprintf("%.3f", fd_rec)),
        colour = "5478FF",
        vjust = -0.7,
        size = 5,
        inherit.aes = FALSE
      )
  }
  
  
  return(p)
}



#--------------------------------------------------------------------------



inst_value <- "1150101103364"
store_path <- "output/code_graphs/secretariat/combined/"

dir.create(store_path, recursive = TRUE, showWarnings = FALSE)

all_codes <- df %>%
  filter(
    inst_code == inst_value,
    is.na(activity_code),
    budget26_27 != 0,
    !is.na(budget26_27),
    budget26_27 != fd_advice,
    economic_code != 3911111
  ) %>%
  pull(economic_code)

groups <- split(all_codes, ceiling(seq_along(all_codes) / 6))

for (i in seq_along(groups)) {
  plot_list <- lapply(groups[[i]], function(code) {
    row_graph(df, code, inst_value)
  })
  
  combined_plot <- wrap_plots(plotlist = plot_list, ncol = 3, nrow = 2) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          legend.text = element_text(size = 20))
  
  ggsave(
    filename = paste0(store_path, "combined_page_", i, ".png"),
    plot = combined_plot,
    width = 24,
    height = 12,
    dpi = 300,
    bg = "white"
  )
  
  print(paste("Saved page", i))
}







