# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(DT)
library(here)
library(dplyr)
library(writexl)
library(readxl)
library(ggplot2)
# library(ggbreak)
library(patchwork)

source(here("R", "01_load_packages.R"))

#------------------------------------------------------------------------------
#         Functions
#___________________________________________________________________________

# 1. Create a function to rename columns
rename_budget_cols <- function(df) {
  
  df <- df %>% 
    select(c(
      economic_code, code_name, budget26_27, fd_adv,
      budget27_28, budget28_29,
      budget25_26, corrected25_26, actual25_26,
      budget24_25, corrected24_25, actual24_25,
      budget23_24, corrected23_24, actual23_24,
      budget22_23, corrected22_23, actual22_23,
      budget21_22, corrected21_22, actual21_22,
      budget20_21, corrected20_21, actual20_21
    ))
  
  colnames(df) <- c(
    "অর্থনৈতিক কোড",
    "বিবরণ",
    "প্রাক্কলণ ২০২৬-২৭",
    "অর্থ বিভাগের সুপারিশ",
    "প্রক্ষেপণ ২০২৭-২৮",
    "প্রক্ষেপণ ২০২৮-২৯",
    "বাজেট ২০২৫-২৬",
    "সংশোধিত বাজেট ২০২৫-২৬",
    "প্রকৃত ২০২৫-২৬ (জানুয়ারি, ২৬)",
    "বাজেট ২০২৪-২৫",
    "সংশোধিত বাজেট ২০২৪-২৫",
    "প্রকৃত ২০২৪-২৫",
    "বাজেট ২০২৩-২৪",
    "সংশোধিত বাজেট ২০২৩-২৪",
    "প্রকৃত ২০২৩-২৪",
    "বাজেট ২০২২-২৩",
    "সংশোধিত বাজেট ২০২২-২৩",
    "প্রকৃত ২০২২-২৩",
    "বাজেট ২০২১-২২",
    "সংশোধিত বাজেট ২০২১-২২",
    "প্রকৃত ২০২১-২২",
    "বাজেট ২০২০-২১",
    "সংশোধিত বাজেট ২০২০-২১",
    "প্রকৃত ২০২০-২১"
  )
  
  df
}

# 2. Create a reusable server function for DT table + modal image
make_budget_server <- function(id, df_reactive, graph_prefix, input, output, session) {
  
  output[[paste0("data_table_", id)]] <- renderDT({
    df <- df_reactive()
    
    if (isTRUE(input$show_mismatch)) {
      df <- df[
        !is.na(df$"প্রাক্কলণ ২০২৬-২৭") &
          !is.na(df$"অর্থ বিভাগের সুপারিশ") &
          df$"প্রাক্কলণ ২০২৬-২৭" != df$"অর্থ বিভাগের সুপারিশ",
      ]
    }
    
    datatable(
      df,
      selection = "single",
      extensions = "FixedHeader",
      options = list(
        pageLength = 100,
        scrollX = TRUE,
        scrollY = "600px",
        fixedHeader = TRUE
      )
    ) %>%
      formatRound(columns = which(sapply(df, is.numeric)), digits = 2)
  })
  
  observeEvent(input[[paste0("data_table_", id, "_rows_selected")]], {
    selected <- input[[paste0("data_table_", id, "_rows_selected")]]
    req(length(selected) == 1)
    
    df <- df_reactive()
    
    if (isTRUE(input$show_mismatch)) {
      df <- df[
        !is.na(df$"প্রাক্কলণ ২০২৬-২৭") &
          !is.na(df$"অর্থ বিভাগের সুপারিশ") &
          df$"প্রাক্কলণ ২০২৬-২৭" != df$"অর্থ বিভাগের সুপারিশ",
      ]
    }
    
    row_data <- df[selected, , drop = FALSE]
    code_val <- as.character(row_data$"অর্থনৈতিক কোড"[1])
    img_src <- paste0(graph_prefix, "/", code_val, ".png")
    
    showModal(
      modalDialog(
        title = paste0(code_val, " - ", as.character(row_data$"বিবরণ"[1])),
        tags$img(src = img_src, style = "width:85%; height:auto;"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
}




make_plot <- function(df, title_text) {
  ggplot(df, aes(y = `বিবরণ`, x = `প্রাক্কলণ ২০২৬-২৭`)) +
    geom_col(fill = "#7AAACE", width = 0.7) +
    geom_segment(
      aes(
        x = `অর্থ বিভাগের সুপারিশ`,
        xend = `অর্থ বিভাগের সুপারিশ`,
        y = as.numeric(`বিবরণ`) - 0.32,
        yend = as.numeric(`বিবরণ`) + 0.32
      ),
      color = "#D64541",
      linewidth = 1.2
    ) +
    labs(
      title = title_text,
      x = "টাকা (কোটি)",
      y = NULL
    ) +
    theme_minimal(base_family = "NikoshBAN", base_size = 18) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 14, colour = "black"),
      axis.text.x = element_text(size = 12, colour = "black"),
      plot.title = element_text(face = "bold", size = 20)
    )
}





# 3. Create horizontal bar chart server
make_budget_bar_server <- function(id, df_reactive, input, output, session) {
  
  output[[paste0("bar_plot_", id)]] <- renderPlot({
    df <- df_reactive()
    
    if (isTRUE(input$show_mismatch)) {
      df <- df[
        !is.na(df$"প্রাক্কলণ ২০২৬-২৭") &
          !is.na(df$"অর্থ বিভাগের সুপারিশ") &
          df$"প্রাক্কলণ ২০২৬-২৭" != df$"অর্থ বিভাগের সুপারিশ",
      ]
    }
    
    req(nrow(df) > 0)
    

    plot_df <- df %>%
      # arrange(desc(`প্রাক্কলণ ২০২৬-২৭`)) %>%
      mutate(
        `বিবরণ` = factor(`বিবরণ`, levels = rev(unique(`বিবরণ`)))
      )

    
    
    
    plot_df_below20 <- plot_df %>%
      filter(`প্রাক্কলণ ২০২৬-২৭` < 2)
    
    plot_df_above20 <- plot_df %>%
      filter(`প্রাক্কলণ ২০২৬-২৭` >= 2)
    
    
    
    
    p1 <- make_plot(plot_df_below20, "২-এর নিচে আইটেমসমূহ")
    p2 <- make_plot(plot_df_above20, "২ বা তার বেশি আইটেমসমূহ")
    
    
    
    
    p1 / p2

    
    
    
    
    # ggplot(plot_df, aes(y = `বিবরণ`, x = `প্রাক্কলণ ২০২৬-২৭`)) +
    #   geom_col(fill = "#7AAACE", width = 0.7) +
    #   geom_segment(
    #     aes(
    #       x = `অর্থ বিভাগের সুপারিশ`,
    #       xend = `অর্থ বিভাগের সুপারিশ`,
    #       y = as.numeric(`বিবরণ`) - 0.32,
    #       yend = as.numeric(`বিবরণ`) + 0.32
    #     ),
    #     color = "#D64541",
    #     linewidth = 1.2
    #   ) +
    # 
    #   
    #   labs(
    #     title = "প্রাক্কলণ ২০২৬-২৭ ও অর্থ বিভাগের সুপারিশ",
    #     x = "টাকা (কোটি)",
    #     y = NULL
    #   ) +
    #   theme_minimal(base_family = "NikoshBAN", base_size = 18) +
    #   theme(
    #     panel.grid.major.y = element_blank(),
    #     panel.grid.minor = element_blank(),
    #     panel.grid.major.x = element_line(color = "#E6E6E6"),
    #     axis.text.y = element_text(size = 14, colour = "black"),
    #     axis.text.x = element_text(size = 12, colour = "black"),
    #     plot.title = element_text(face = "bold", size = 20)
    #   )
  }, height = function() {
    df <- df_reactive()
    
    if (isTRUE(input$show_mismatch)) {
      df <- df[
        !is.na(df$"প্রাক্কলণ ২০২৬-২৭") &
          !is.na(df$"অর্থ বিভাগের সুপারিশ") &
          df$"প্রাক্কলণ ২০২৬-২৭" != df$"অর্থ বিভাগের সুপারিশ",
      ]
    }
    
    max(600, nrow(df) * 35)
  })
}

#----------------------------------------------------------------------------

# 4. Prepare each dataframe once

data <- readRDS(here("data/final/budget_df.rds"))

fd_data <- read_xlsx(paste0(drive, "data/budget_input.xlsx"))
data$fd_adv <- fd_data$fd_advice

data_imed <- data %>%
  filter(inst_code %in% c("1150101103364") & type == 1) %>%
  filter(budget26_27 != 0 & !is.na(budget26_27))

# write_xlsx(data_imed, here("output/table4_input.xlsx"))

data_imed <- rename_budget_cols(data_imed)

#--------------------------------------------------------------------------------

dashboard_config <- list(
  list(
    id = "imed",
    tabName = "secretariat",
    label = "সচিবালয়",
    title = "সচিবালয় এর পরিচালন বাজেট",
    graph_prefix = "secretariat_graphs",
    data = data_imed,
    type = "table"
  ),
  list(
    id = "imed_bar",
    tabName = "secretariat_bar",
    label = "সচিবালয় বার চার্ট",
    title = "সচিবালয় এর প্রাক্কলণ বনাম অর্থ বিভাগের সুপারিশ",
    graph_prefix = NULL,
    data = data_imed,
    type = "bar"
  )
)

#-------------------------------------------------------------------------------
# UI

ui <- dashboardPage(
  
  dashboardHeader(title = "Budget Dashboard"),
  
  dashboardSidebar(
    do.call(
      sidebarMenu,
      lapply(dashboard_config, function(x) {
        menuItem(x$label, tabName = x$tabName, icon = icon("table"))
      })
    ),
    checkboxInput("show_mismatch", "হ্রাসকৃত সুপারিশসমূহ", value = FALSE)
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        table.dataTable {
          font-family: 'NikoshBAN';
          font-size: 20px;
        }
      "))
    ),
    
    do.call(
      tabItems,
      lapply(dashboard_config, function(x) {
        if (x$type == "table") {
          tabItem(
            tabName = x$tabName,
            fluidRow(
              box(
                title = x$title,
                width = 12,
                DTOutput(paste0("data_table_", x$id))
              )
            )
          )
        } else if (x$type == "bar") {
          tabItem(
            tabName = x$tabName,
            fluidRow(
              box(
                title = x$title,
                width = 12,
                plotOutput(paste0("bar_plot_", x$id))
              )
            )
          )
        }
      })
    )
  )
)

addResourcePath(
  "secretariat_graphs",
  here::here("output/code_graphs/secretariat")
)

#-------------------------------------------------------------------------------
# server

server <- function(input, output, session) {
  
  for (cfg in dashboard_config) {
    local({
      x <- cfg
      
      if (x$type == "table") {
        make_budget_server(
          id = x$id,
          df_reactive = reactive(x$data),
          graph_prefix = x$graph_prefix,
          input = input,
          output = output,
          session = session
        )
      }
      
      if (x$type == "bar") {
        make_budget_bar_server(
          id = x$id,
          df_reactive = reactive(x$data),
          input = input,
          output = output,
          session = session
        )
      }
    })
  }
}

# Run the application
shinyApp(ui = ui, server = server)