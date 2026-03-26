############################################################################################################
##            five years budget expenditure capacity
##################################################################################

rm(list = ls())

library(here)

source(here("R", "01_load_packages.R"))

df <- readRDS(here("data/final/rev_df.rds"))


sum_total <- df %>%
  summarise(across(c(3:23),
                   ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")








#############################################################################

## transform to long format
plot_df <- sum_total %>%
  pivot_longer(cols = everything(),
               names_to = "year_type",
               values_to = "Amount")




plot_df <- plot_df %>%
  extract(col = year_type,
          into = c("type", "year"),
          regex = "(budget|actual|corrected)?(\\d{2}_\\d{2})")


plot_df <- plot_df %>%
  complete(year, type = c("actual","budget","corrected"))


plot_df$type <- factor(plot_df$type,
                       levels = c("budget", "corrected", "actual"))


#########################################################################
##          5 year and projecton data
##########################################################################


ceiling <- data.frame(
  year = c("26_27", "27_28", "28_29"),
  Amount = c(548.22, 603.04, 663.34),   # your values here
  type = "target",   # new category
  label = c("548.22", "603.04", "663.34")
)

agg_png("output/figures/03_rev_graph.png", width = 12,
        height = 8, units = "in", res = 300)

## color code


#font_add("bangla_font", "C:/Users/Md. Mamunul Karim/AppData/Local/Microsoft/Windows/Fonts/NikoshBAN.ttf")

# font_add("bangla_font", here(font_path, "NikoshBAN.ttf"))
# 
showtext_auto(FALSE)

p <- ggplot(plot_df,
            aes(x = year, y = Amount, fill = type)) +
  
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7,
           color = "white") +
  
  # special single bar on 26_27
  geom_col(
    data = ceiling,
    aes(x = year, y = Amount, fill = type),
    inherit.aes = FALSE,
    width = 0.18        # make it narrower so it sits on top
  ) +
  
  
  # label for special bar
  geom_text(
    data = ceiling,
    aes(x = year, y = Amount, label = label),
    inherit.aes = FALSE,
    family = "NikoshBAN",
    size = 5,
    color = "#E63946",
    vjust = -0.5,
    show.legend = FALSE
  ) +
  
  scale_fill_manual(values = c(
    "budget" = "#1A3263",       # green
    "corrected" = "#FFA6A6",
    "actual" = "#3A9AFF",
    "target" = "#E63946"   # new color
  ),
  
  labels = c(
    "budget" = "বাজেট",       # green
    "corrected" = "সংশোধিত বাজেট",
    "actual" = "প্রকৃত",
    "target" = "প্রাথ: লক্ষ্যমাত্রা (বিসি-১)"   # 👈 your legend text
  ),
  drop = FALSE) +
  
  labs(
    title = "",
    subtitle = "",
    x = NULL,
    y = "টাকা (কোটি)",
    fill = NULL
  ) +
  scale_x_discrete(labels = c(
    "20_21" = "2020-21",
    "21_22" = "2021-22",
    "22_23" = "2022-23",
    "23_24" = "2023-24",
    "24_25" = "2024-25",
    "25_26" = "2025-26",
    "26_27" = "2026-27",
    "27_28" = "2027-28",
    "28_29" = "2028-29"
  ))+
  
  theme_minimal(base_family = "NikoshBAN", base_size = 16) +
  
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    legend.key.size = unit(1.1, "cm"),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#E6E6E6", linewidth = 0.8),
    axis.text.x = element_text(family = "NikoshBAN", size = 15, colour = "black")
  ) +
  
  geom_text(aes(label = sprintf("%.2f", Amount)),
            position = position_dodge(width = 0.75),
            family = "NikoshBAN", size = 5, color = "black",
            hjust = -.1,
            angle = 45)


print(p)
dev.off()



############################################################################################################
##            Create Revenue Table
##################################################################################

rev_table <- tibble(
  year = character(),
  target = numeric(),
  corrected = numeric(),
  actual = numeric()
)


for (row in c("28_29", "27_28", "26_27", "25_26", "24_25", "23_24", "22_23", "21_22", "20_21")) {
  print(row)
  
  add_row <- tibble(
    year = row,
    target = plot_df$Amount[plot_df$type=="budget" & plot_df$year==row],
    corrected = plot_df$Amount[plot_df$type=="corrected" & plot_df$year==row],
    actual = plot_df$Amount[plot_df$type=="actual" & plot_df$year==row]
  )
  
  rev_table <- bind_rows(rev_table, add_row)
}


###################################################################################
##            Create Revenue Table  for Economic Code
##################################################################################


econ_table <- tibble(
  code = character(),
  year20_21 = numeric(),
  year21_22 = numeric(),
  year22_23 = numeric(),
  year23_24 = numeric(),
  year24_25 = numeric(),
  year25_26 = numeric()
)




for (row in df$economic_code){
  add_row <- tibble(
    code = df$code_name[df$economic_code == row],
    year20_21 = df$corrected20_21[df$economic_code == row],
    year21_22 = df$corrected21_22[df$economic_code == row],
    year22_23 = df$corrected22_23[df$economic_code == row],
    year23_24 = df$corrected23_24[df$economic_code == row],
    year24_25 = df$corrected24_25[df$economic_code == row],
    year25_26 = df$corrected25_26[df$economic_code == row]
  )
  
  econ_table <- bind_rows(econ_table, add_row)
  
}

## clean table
econ_table <- econ_table %>% 
  filter(!if_all(c(2:7), ~ is.na(.) | . == 0))

