## import data

rm(list = ls())

library(here)

source(here("R", "01_load_packages.R"))

df <- readRDS(here("data/final/budget_df.rds"))


## make a df for 5 years graph

df_5yr <- df %>% 
  select(c(economic_code, activity_code, type, actual23_24, actual24_25, budget25_26, corrected25_26,
           budget26_27, budget27_28, budget28_29))


sum_total <- df_5yr %>%
  summarise(across(c(4:10),
                   ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")

sum_total$type <- 3

df_5yr <- bind_rows(df_5yr, sum_total)


## transform to long format
plot_df <- df_5yr %>%
  pivot_longer(cols = c(4:10),
               names_to = "year",
               values_to = "Amount")

plot_df <- plot_df %>% 
  group_by(year, type) %>% 
  summarise(Amount = sum(Amount, na.rm = TRUE),
            .groups = "drop")



plot_df$type <- recode(plot_df$type,
                      `1` = "Operating",
                      `2` = "Development",
                      `3` = "Total")



plot_df <- plot_df %>% 
  mutate(
    type = factor(type, levels = c("Operating", "Development", "Total")),
    year = factor(year, levels = c("actual23_24", "actual24_25", "budget25_26",
                                   "corrected25_26", "budget26_27", "budget27_28", "budget28_29"))
  )


############################################

## color code

op_color <- "#9CD5FF"
dev_color <-  "#7AB2B2"
total_color <- "#80A1BA"

#font_add("bangla_font", "C:/Users/Md. Mamunul Karim/AppData/Local/Microsoft/Windows/Fonts/NikoshBAN.ttf")

font_add("bangla_font", here(font_path, "NikoshBAN.ttf"))


showtext_auto()

p <- ggplot(plot_df,
       aes(x = year, y = Amount, fill = type)) +
  
  geom_col(position = position_dodge(width = 0.75),
           width = 0.7,
           color = "white") +
  
  scale_fill_manual(values = c(
    "Operating" = op_color,       # green
    "Development" = dev_color,      # orange
    "Total" = total_color     # dark blue
  )) +
  
  labs(
    title = "",
    subtitle = "",
    x = NULL,
    y = "Taka (Crore)",
    fill = NULL
  ) +
  
  theme_minimal(base_family = "bangla_font", base_size = 24) +
  
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 35),
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(face = "bold", size = 50, colour = "black")
    ) +
  scale_x_discrete(labels = c(
    "actual23_24" = "2023-24",
    "actual24_25" = "2024-25",
    "budget25_26" = "2025-26",
    "budget26_27" = "2026-27",
    "budget27_28" = "2027-28",
    "budget28_29" = "2028-29",
    "corrected25_26" = "2025-26"
  ))+
  geom_text(aes(label = sprintf("%.2f", Amount)),
            position = position_dodge(width = 0.75),
            family = "bangla_font", size = 14, color = "black",
            vjust = -0.2)
            

p
  ggsave(here("output/figures/1_5yrs.png"), plot = p, width = 12, height = 6, units = "in", dpi = "print")


############################################

# stacked column chart for operating and development ratio
##################################################################

plot_ratio <- plot_df %>% 
  filter(type != "Total") %>% 
  group_by(year) %>% 
  mutate(
    ratio = Amount/sum(Amount),
    type = factor(type, levels = c("Development", "Operating"))
  )

p <- ggplot(plot_ratio,
       aes(x = year, y = ratio, fill = type))+
  geom_col(position = "fill")+
  
  
  scale_fill_manual(values = c(
    "Operating" = op_color,       # green
    "Development" = dev_color))+

  
  scale_x_discrete(labels = c(
    "actual23_24" = "2023-24",
    "actual24_25" = "2024-25",
    "budget25_26" = "2025-26",
    "budget26_27" = "2026-27",
    "budget27_28" = "2027-28",
    "budget28_29" = "2028-29",
    "corrected25_26" = "2025-26"
  ))+
  
  theme_minimal(base_family = "bangla_font", base_size = 24) +
  
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 50),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 50,
                               family = "bangla_font", face = "bold",
                               color = "black"))+
  
  labs(
    x = NULL,
    y = "Ratio")+
 
  geom_text(aes(label = sprintf("%.2f%%", ratio*100)),
            family = "bangla_font", size = 20,
            position = position_stack(vjust = 0.5))


p

ggsave(here("output/figures/2_ratio.png"), plot = p, width = 12, height = 6, units = "in", dpi = "print")


############################################################################################################
##            five years budget expenditure capacity
##################################################################################



exp_df <- df %>% 
  filter(type == 1) %>% 
  select(c(19:32)) %>% 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)))

## make a table

op_table <- tibble(
  year = c("2022-23", "2023-24", "2024-25", "2025-26"),
  budget = c(exp_df$budget22_23, exp_df$budget23_24, exp_df$budget24_25, exp_df$budget25_26),
  corrected = c(exp_df$corrected22_23, exp_df$corrected23_24, exp_df$corrected24_25, exp_df$corrected25_26),
  actual = c(exp_df$actual22_23, exp_df$actual23_24, exp_df$actual24_25, exp_df$actual25_26)
)

new_row <- data.frame(
  year = "2021-22",
  budget = 51.17,
  corrected = 51.97,
  actual = 46.00
)

op_table <- bind_rows(new_row, op_table)

op_table$ratio_exp <- op_table$actual/op_table$corrected


## ------------------------------------------------------------------------------

  
##############################################################
## transform to long format
df_long <- op_table %>%
  pivot_longer(cols = c(2:5),
               names_to = "type",
               values_to = "value")


# Scaling factor for secondary axis
scale_factor <- max(df_long$value[df_long$type != "ratio_exp"], na.rm = TRUE) /
  max(df_long$value[df_long$type == "ratio_exp"], na.rm = TRUE)

# Split data
bar_df  <- df_long %>% filter(type %in% c("budget","actual","corrected"))
line_df <- df_long %>% filter(type == "ratio_exp")

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
  "budget"    = "#1b9e77",
  "actual"    = "#d95f02",
  "corrected" = "#7570b3"
)) +
  
  scale_color_manual(values = c(
    "ratio_exp" = "red"
  )) +
  
  # Merge legends
  labs(fill = "", color = "") +
  
  theme_minimal(base_size = 20, base_family = "bangla_font") +
  
  # ---------------- Legend at Bottom ----------------
theme(
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.box = "horizontal",
  axis.text = element_text(size = 30,
                             color = "black")
) +
  
  guides(
    fill  = guide_legend(nrow = 1),
    color = guide_legend(nrow = 1)
  )

p

ggsave("output/figures/03_opexp.png", plot = p, width = 12, height = 6, units = "in", dpi = "print")
