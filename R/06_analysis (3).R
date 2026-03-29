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
#------------------------------------------------------------------------------
#                 1-5 years budget graph Graph 1
#-------------------------------------------------------------------------------

agg_png("output/figures/1_5yrs.png", width = 12,
        height = 8, units = "in", res = 300)

## color code

op_color <- "#F7B980"
dev_color <-  "#66D0BC"
total_color <- "#7AAACE"

#font_add("bangla_font", "C:/Users/Md. Mamunul Karim/AppData/Local/Microsoft/Windows/Fonts/NikoshBAN.ttf")

# font_add("bangla_font", here(font_path, "NikoshBAN.ttf"))
# 
showtext_auto(FALSE)

p <- ggplot(plot_df,
       aes(x = year, y = Amount, fill = type)) +
  
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7,
           color = "white") +
  
  scale_fill_manual(values = c(
    "Operating" = op_color,       # green
    "Development" = dev_color,      # orange
    "Total" = total_color     # dark blue
  ),
  
  labels = c(
    "Operating" = "পরিচালন",
    "Development" = "উন্নয়ন",
    "Total" = "মোট"
  )) +
  
  labs(
    title = "",
    subtitle = "",
    x = NULL,
    y = "টাকা (কোটি)",
    fill = NULL
  ) +
  scale_x_discrete(labels = c(
    "actual23_24" = "2023-24 \n প্রকৃত",
    "actual24_25" = "2024-25 \n প্রকৃত",
    "budget25_26" = "2025-26 \n বাজেট",
    "corrected25_26" = "2025-26 \n সংশোধিত বাজেট ",
    "budget26_27" = "2026-27  \n প্রাক্কলন",
    "budget27_28" = "2027-28 \n প্রক্ষেপন",
    "budget28_29" = "2028-29 \n প্রক্ষেপন"
  ))+
  
  theme_minimal(base_family = "NikoshBAN", base_size = 24) +
  
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    legend.key.size = unit(1.4, "cm"),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#E6E6E6", linewidth = 0.8),
    axis.text.x = element_text(family = "NikoshBAN", size = 18, colour = "black")
    ) +
 
  geom_text(aes(label = sprintf("%.2f", Amount)),
            position = position_dodge(width = 0.75),
            family = "NikoshBAN", size = 5, color = "black",
            vjust = -.1,
            angle = 0)
            

print(p)
dev.off()


  # ggsave(here("output/figures/1_5yrs.png"), plot = p, width = 12, height = 6, units = "in", dpi = "print")


#-------------------------------------------------------------------------------  
  
############################################

# stacked column chart for operating and development ratio graph 2
##################################################################
#-------------------------------------------------------------------------------
  
plot_ratio <- plot_df %>% 
  filter(type != "Total") %>% 
  group_by(year) %>% 
  mutate(
    ratio = Amount/sum(Amount),
    type = factor(type, levels = c("Development", "Operating"))
  )


agg_png("output/figures/2_ratio.png", width = 12,
        height = 6, units = "in", res = 300)

p <- ggplot(plot_ratio,
       aes(x = year, y = ratio, fill = type))+
  geom_col(position = "fill")+
  
  
  scale_fill_manual(values = c(
    "Operating" = op_color,       # green
    "Development" = dev_color),
    labels = c(
      "Operating" = "পরিচালন",
      "Development" = "উন্নয়ন"
    ))+

  
  scale_x_discrete(labels = c(
    "actual23_24" = "2023-24 \n প্রকৃত",
    "actual24_25" = "2024-25 \n প্রকৃত",
    "budget25_26" = "2025-26 \n বাজেট",
    "corrected25_26" = "2025-26 \n সংশোধিত বাজেট ",
    "budget26_27" = "2026-27  \n প্রক্ষেপন",
    "budget27_28" = "2027-28 \n প্রাক্কলন",
    "budget28_29" = "2028-29 \n প্রক্ষেপন"
  ))+
  
  theme_minimal(base_family = "NikoshBAN", base_size = 20) +
  
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 18),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 18,
                               family = "NikoshBAN",
                               color = "black"))+
  
  labs(
    x = NULL,
    y = "শতকরা হার")+
 
  geom_text(aes(label = sprintf("%.2f%%", ratio*100)),
            family = "NikoshBAN", size = 8,
            position = position_stack(vjust = 0.5))


print(p)
dev.off()



#-------------------------------------------------------------------------------

################################################################################
##            five years total budget expenditure capacity
#################################################################################

#-------------------------------------------------------------------------------




exp_df <- df %>% 
  select(c(19:38)) %>% 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)))

## make a table

budget_table <- tibble(
  year = c("2020-21", "2021-22", "2022-23", "2023-24", "2024-25"),
  budget = c(exp_df$budget20_21, exp_df$budget21_22, exp_df$budget22_23,
             exp_df$budget23_24, exp_df$budget24_25),
  corrected = c(exp_df$corrected20_21, exp_df$corrected21_22, 
                exp_df$corrected22_23, exp_df$corrected23_24,
                exp_df$corrected24_25),
  actual = c(exp_df$actual20_21, exp_df$actual21_22, exp_df$actual22_23, 
             exp_df$actual23_24, exp_df$actual24_25)
)



budget_table$ratio_budget <- budget_table$actual/budget_table$budget


budget_table$ratio_corrected <- budget_table$actual/budget_table$corrected


saveRDS(budget_table, here("data/final/table4.rds"))


## ------------------------------------------------------------------------------


##############################################################
## transform to long format
df_long <- budget_table %>%
  pivot_longer(cols = c(2:6),
               names_to = "type",
               values_to = "value")


# Scaling factor for secondary axis
scale_factor <- max(df_long$value[df_long$type != "ratio_corrected"], na.rm = TRUE) /
  max(df_long$value[df_long$type == "ratio_corrected"], na.rm = TRUE)

# Split data
bar_df  <- df_long %>% filter(type %in% c("budget","actual","corrected"))
line_df <- df_long %>% filter(type == "ratio_corrected")


bar_df <- bar_df %>% 
  mutate(
    type = factor(type, levels = c("budget", "corrected", "actual"))
  )




showtext_auto(FALSE)

agg_png("output/figures/03_totalbudget5yr.png", width = 12,
        height = 6.5, units = "in", res = 300)


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
    hjust = 1.1,
    size = 5.5,
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
    vjust = -.01,
    color = "black",
    size = 5.5
  ) +
  
  # ---------------- Dual Axis ----------------
scale_y_continuous(
  name = "টাকা (কোটি)",
  sec.axis = sec_axis(~ . / scale_factor *100,
                      name = "শতকরা হার (%)")
) +
  
  # ---------------- Colors ----------------
scale_fill_manual(values = c(
  "budget"    = "#3A9AFF",
  "actual"    = "#BCD9A2",
  "corrected" = "#FB9B8F"
),
labels = c(
  "budget"    = "বাজেট",
  "actual"    = "প্রকৃত",
  "corrected" = "সংশোধিত বাজেট"
)
) +
  
  scale_color_manual(values = c(
    "ratio_corrected" = "#3D45AA"),
    labels = c("ratio_corrected" = "প্রকৃত ব্যয় এর হার \n (সংশোধিত বাজেটের তুলনায়)")
  ) +
  
  # Merge legends
  labs(fill = "", color = "") +
  
  theme_minimal(base_size = 12, base_family = "NikoshBAN") +
  
  # ---------------- Legend at Bottom ----------------
theme(
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.box = "horizontal",
  legend.text = element_text(size = 18),
  axis.text = element_text(size = 18,
                           color = "black"),
  axis.title.x = element_blank(),
  axis.title.y = element_text(size = 18)
) +
  
  guides(
    fill  = guide_legend(nrow = 1),
    color = guide_legend(nrow = 1)
  )

print(p)

dev.off()













#-------------------------------------------------------------------------------

################################################################################
##            five years operating budget expenditure capacity
#################################################################################

#-------------------------------------------------------------------------------


exp_df <- df %>% 
  filter(type == 1) %>% 
  select(c(19:38)) %>% 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)))

## make a table

op_table <- tibble(
  year = c("2020-21", "2021-22", "2022-23", "2023-24", "2024-25"),
  budget = c(exp_df$budget20_21, exp_df$budget21_22, exp_df$budget22_23,
             exp_df$budget23_24, exp_df$budget24_25),
  corrected = c(exp_df$corrected20_21, exp_df$corrected21_22, 
                exp_df$corrected22_23, exp_df$corrected23_24,
                exp_df$corrected24_25),
  actual = c(exp_df$actual20_21, exp_df$actual21_22, exp_df$actual22_23, 
             exp_df$actual23_24, exp_df$actual24_25)
)



op_table$ratio_exp <- op_table$actual/op_table$corrected

saveRDS(op_table, here("data/final/table3.rds"))


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


bar_df <- bar_df %>% 
  mutate(
    type = factor(type, levels = c("budget", "corrected", "actual"))
  )




showtext_auto(FALSE)

agg_png("output/figures/03_opexp.png", width = 12,
        height = 6.5, units = "in", res = 300)


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
    hjust = 1.5,
    size = 7,
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
    size = 7
  ) +
  
  # ---------------- Dual Axis ----------------
scale_y_continuous(
  name = "টাকা (কোটি)",
  sec.axis = sec_axis(~ . / scale_factor *100,
                      name = "শতকরা হার (%)")
) +
  
  # ---------------- Colors ----------------
scale_fill_manual(values = c(
  "budget"    = "#3A9AFF",
  "actual"    = "#BCD9A2",
  "corrected" = "#FB9B8F"
  ),
labels = c(
  "budget"    = "বাজেট",
  "actual"    = "প্রকৃত",
  "corrected" = "সংশোধিত বাজেট"
  )
) +
  
  scale_color_manual(values = c(
    "ratio_exp" = "#3D45AA"),
    labels = c("ratio_exp" = "প্রকৃত এর হার (সংশোধিত বাজেটের তুলনায়)")
  ) +
  
  # Merge legends
  labs(fill = "", color = "") +
  
  theme_minimal(base_size = 12, base_family = "NikoshBAN") +
  
  # ---------------- Legend at Bottom ----------------
theme(
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.box = "horizontal",
  legend.text = element_text(size = 18),
  axis.text = element_text(size = 18,
                             color = "black"),
  axis.title.x = element_blank(),
  axis.title.y = element_text(size = 18)
) +
  
  guides(
    fill  = guide_legend(nrow = 1),
    color = guide_legend(nrow = 1)
  )

print(p)

dev.off()










#-------------------------------------------------------------------------------

################################################################################
##            five years Development budget expenditure capacity
#################################################################################

#-------------------------------------------------------------------------------


exp_df <- df %>% 
  filter(type == 2) %>% 
  select(c(19:38)) %>% 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)))

## make a table

dev_table <- tibble(
  year = c("2020-21", "2021-22", "2022-23", "2023-24", "2024-25"),
  budget = c(exp_df$budget20_21, exp_df$budget21_22, exp_df$budget22_23,
             exp_df$budget23_24, exp_df$budget24_25),
  corrected = c(exp_df$corrected20_21, exp_df$corrected21_22, 
                exp_df$corrected22_23, exp_df$corrected23_24,
                exp_df$corrected24_25),
  actual = c(exp_df$actual20_21, exp_df$actual21_22, exp_df$actual22_23, 
             exp_df$actual23_24, exp_df$actual24_25)
)



dev_table$ratio_exp <- dev_table$actual/dev_table$corrected

saveRDS(dev_table, here("data/final/table6.rds"))


## ------------------------------------------------------------------------------


##############################################################
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


bar_df <- bar_df %>% 
  mutate(
    type = factor(type, levels = c("budget", "corrected", "actual"))
  )




showtext_auto(FALSE)

agg_png("output/figures/03_devexp.png", width = 12,
        height = 6.5, units = "in", res = 300)


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
    vjust = -.7,
    size = 5.5,
    angle = 0
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
    hjust = -.7,
    color = "#6367FF",
    size = 5.5
  ) +
  
  # ---------------- Dual Axis ----------------
scale_y_continuous(
  name = "টাকা (কোটি)",
  expand = expansion(mult = c(0, 0.15)),
  sec.axis = sec_axis(~ . / scale_factor *100,
                      name = "শতকরা হার (%)")
) +
  
  # ---------------- Colors ----------------
scale_fill_manual(values = c(
  "budget"    = "#3A9AFF",
  "actual"    = "#BCD9A2",
  "corrected" = "#FB9B8F"
),
labels = c(
  "budget"    = "বাজেট",
  "actual"    = "প্রকৃত",
  "corrected" = "সংশোধিত বাজেট"
)
) +
  
  scale_color_manual(values = c(
    "ratio_exp" = "#3D45AA"),
    labels = c("ratio_exp" = "প্রকৃত এর হার (সংশোধিত বাজেটের তুলনায়)")
  ) +
  
  # Merge legends
  labs(fill = "", color = "") +
  
  theme_minimal(base_size = 12, base_family = "NikoshBAN") +
  
  # ---------------- Legend at Bottom ----------------
theme(
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.box = "horizontal",
  legend.text = element_text(size = 18),
  axis.text = element_text(size = 18,
                           color = "black"),
  axis.title.x = element_blank(),
  axis.title.y = element_text(size = 18)
) +
  
  guides(
    fill  = guide_legend(nrow = 1),
    color = guide_legend(nrow = 1)
  ) 

print(p)

dev.off()







