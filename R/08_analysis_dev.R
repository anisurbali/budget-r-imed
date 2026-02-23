
############################################################################################################
##            five years budget expenditure capacity
##################################################################################

rm(list = ls())

library(here)

source(here("R", "01_load_packages.R"))

df <- readRDS(here("data/final/budget_df.rds"))



#____________________ Table 6_______________________________________________
#--------------------Development Expenditure 5yr Table---------------------------
exp_df <- df %>% 
  filter(type == 2) %>% 
  select(c(19:38)) %>% 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)))

## make a table

dev_table <- tibble(
  year = c("2020-21", "2021-22", "2022-23", "2023-24", "2024-25", "2025-26"),
  budget = c(exp_df$budget20_21, exp_df$`budget21-22`, exp_df$budget22_23,
             exp_df$budget23_24, exp_df$budget24_25, exp_df$budget25_26),
  corrected = c(exp_df$`corrected20-21`, exp_df$corrected21_22, 
                exp_df$corrected22_23, exp_df$corrected23_24,
                exp_df$corrected24_25, exp_df$corrected25_26),
  actual = c(exp_df$actual20_21, exp_df$`actual21-22`, exp_df$actual22_23, 
             exp_df$actual23_24, exp_df$actual24_25, exp_df$actual25_26)
)




dev_table$ratio_exp <- dev_table$actual/dev_table$corrected

saveRDS(dev_table, here("data/final/table6.rds"))

#--------------------------------------------------------------------------
#-----------------graph for five year development budget exp cabacity-----------
#-----------------------------------------------------------------------------


## transform to long format
df_long <- dev_table %>%
  pivot_longer(cols = c(2:5),
               names_to = "type",
               values_to = "value")




# Scaling factor for secondary axis
scale_factor <- max(df_long$value[df_long$type != "ratio_exp"], na.rm = TRUE) /
  max(df_long$value[df_long$type == "ratio_exp"], na.rm = TRUE)



# Split data
bar_df  <- df_long %>% filter(type %in% c("budget","actual","corrected"))
line_df <- df_long %>% filter(type == "ratio_exp")


font_add("bangla_font", here(font_path, "NikoshBAN.ttf"))
showtext_auto()


bar_df <- bar_df %>% 
  mutate(
    type = factor(type, levels = c("budget","corrected", "actual"))
  )

p <- ggplot() +
  
  # ---------------- Bars ----------------
geom_col(
  data = bar_df,
  aes(x = year, y = value, fill = type),
  position = position_dodge(width = 0.7),
  width = 0.6
) +
  
  # Bar labels
  geom_text(
    data = bar_df,
    aes(x = year, y = value, label = round(value,2), group = type),
    position = position_dodge(width = 0.7),
    hjust = 1,
    size = 20,
    angle = 90
  ) +
  
  # ---------------- Line ----------------
geom_line(
  data = line_df,
  aes(x = year, 
      y = value * scale_factor, 
      color = type,
      group = 1),
  linewidth = 1.2
) +
  
  geom_point(
    data = line_df,
    aes(x = year, 
        y = value * scale_factor, 
        color = type),
    size = 2.5
  ) +
  
  # Line labels
  geom_text(
    data = line_df,
    aes(x = year, 
        y = value * scale_factor, 
        label = sprintf("%.2f%%", value*100)),
    vjust = -0.1,
    color = "black",
    size = 20
  ) +
  
  # ---------------- Dual Axis ----------------
scale_y_continuous(
  name = "Amount",
  sec.axis = sec_axis(~ . / scale_factor,
                      name = "Ratio (%)")
) +
  
  # ---------------- Colors ----------------
scale_fill_manual(values = c(
  "budget"    = "#3A9AFF",
  "actual"    = "#BCD9A2",
  "corrected" = "#FB9B8F"
),
labels = c(
  "budget"    = "Budget",
  "actual"    = "Actual",
  "corrected" = "Revised"
)) +
  
  scale_color_manual(values = c(
    "ratio_exp" = "#3D45AA"),
    labels = c("ratio_exp" = "Ratio of Actual to Revised"
  )) +
  
  # Merge legends
  labs(fill = "", color = "") +
  
  theme_minimal(base_size = 20, base_family = "bangla_font") +
  
  # ---------------- Legend at Bottom ----------------
theme(
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.box = "horizontal",
  legend.text = element_text(size = 30),
  axis.text = element_text(size = 40,
                           color = "black"),
  axis.title.x = element_blank()
) +
  
  guides(
    fill  = guide_legend(nrow = 1),
    color = guide_legend(nrow = 1)
  )
p


ggsave("output/figures/05_devexp.png", plot = p, width = 12, height = 6, units = "in", dpi = "print")

