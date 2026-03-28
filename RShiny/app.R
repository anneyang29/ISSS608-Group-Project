#====================================================
# ISSS608 Group Project - Main Dashboard Application
#====================================================

# load R packages
#====================================================
pacman::p_load(shiny, shinydashboard, shinycssloaders, 
               tidyverse, dplyr, readr, ggcorrplot, parallelPlot, 
               cluster, mclust, fresh, bslib, bsicons,  tsibble, fable, 
               feasts, tidymodels, timetk, 
               modeltime, rlang, plotly, data.table, lubridate)

find_existing_path <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) > 0) {
    normalizePath(existing[1], winslash = "/", mustWork = TRUE)
  } else {
    NA_character_
  }
}

base_rds_candidates <- c(
  file.path("RShiny", "data", "shiny_base_data_light.rds"),
  file.path("data", "shiny_base_data_light.rds"),
  file.path("..", "RShiny", "data", "shiny_base_data_light.rds"),
  file.path("..", "data", "shiny_base_data_light.rds")
)

time_rds_candidates <- c(
  file.path("RShiny", "data", "shiny_time_data.rds"),
  file.path("data", "shiny_time_data.rds"),
  file.path("..", "RShiny", "data", "shiny_time_data.rds"),
  file.path("..", "data", "shiny_time_data.rds")
)

base_data_path <- find_existing_path(base_rds_candidates)
time_data_path <- find_existing_path(time_rds_candidates)

if (!is.na(base_data_path)) {
  message("Loading light base RDS from: ", base_data_path)
  
  data_bundle <- readRDS(base_data_path)
  base_data <- data_bundle$base_data
  
  # do NOT load time_data at app startup
  time_data <- NULL
  
} else {
  stop(
    "shiny_base_data_light.rds not found. Please deploy the prebuilt light RDS file with the app.",
    call. = FALSE
  )
}

build_base_data_from_csv <- function(customer_csv_path, tx_csv_path) {
  customer_raw <- readr::read_csv(customer_csv_path, show_col_types = FALSE) %>%
    dplyr::mutate(customer_id = as.numeric(customer_id))
  
  tx_clean <- readr::read_csv(tx_csv_path, show_col_types = FALSE) %>%
    dplyr::mutate(
      customer_id = as.numeric(customer_id),
      date = as.Date(date),
      amount = as.numeric(amount),
      type = as.character(type)
    ) %>%
    dplyr::filter(!is.na(customer_id), !is.na(date), !is.na(amount), amount > 0)
  
  if (nrow(tx_clean) == 0) {
    stop("transactions_data.csv has no valid rows after cleaning.", call. = FALSE)
  }
  
  snapshot_date <- max(tx_clean$date, na.rm = TRUE)
  
  tx_features <- tx_clean %>%
    dplyr::group_by(customer_id) %>%
    dplyr::summarise(
      monthly_transaction_count = dplyr::n() / pmax(dplyr::n_distinct(format(date, "%Y-%m")), 1),
      average_transaction_value = mean(amount, na.rm = TRUE),
      total_transaction_volume = sum(amount, na.rm = TRUE),
      transaction_frequency = dplyr::n() / pmax(as.numeric(max(date) - min(date)) + 1, 1),
      avg_daily_transactions = dplyr::n() / pmax(dplyr::n_distinct(date), 1),
      weekend_transaction_ratio = mean(weekdays(date) %in% c("Saturday", "Sunday"), na.rm = TRUE),
      feature_usage_diversity = dplyr::n_distinct(type),
      customer_tenure = as.numeric(snapshot_date - min(date)) + 1,
      .groups = "drop"
    )
  
  overlap <- intersect(setdiff(names(tx_features), "customer_id"), names(customer_raw))
  
  base_data <- customer_raw %>%
    dplyr::select(-dplyr::any_of(overlap)) %>%
    dplyr::left_join(tx_features, by = "customer_id")
  
  product_flag_cols <- intersect(
    c(
      "savings_account", "credit_card", "personal_loan", "investment_account",
      "insurance_product", "bill_payment_user", "auto_savings_enabled"
    ),
    names(base_data)
  )
  
  if (length(product_flag_cols) > 0) {
    base_data <- base_data %>%
      dplyr::mutate(
        active_products = rowSums(
          dplyr::across(
            dplyr::all_of(product_flag_cols),
            ~ as.numeric(tolower(as.character(.x)) %in% c("1", "true", "yes", "y", "active"))
          ),
          na.rm = TRUE
        )
      )
  }
  
  if ("app_logins_frequency" %in% names(base_data)) {
    base_data <- base_data %>%
      dplyr::mutate(app_logins_frequency = as.numeric(app_logins_frequency))
  }
  
  cus_clean <- customer_raw %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(intersect("customer_segment", names(.))),
        ~ factor(.x, levels = c("inactive", "occasional", "regular", "power"))
      ),
      dplyr::across(
        dplyr::all_of(intersect("clv_segment", names(.))),
        ~ factor(.x, levels = c("Bronze", "Silver", "Gold", "Platinum"))
      ),
      dplyr::across(
        dplyr::all_of(intersect("income_bracket", names(.))),
        ~ factor(.x, levels = c("Low", "Medium", "High", "Very High"))
      )
    )
  
  time_data <- tx_clean %>%
    dplyr::inner_join(cus_clean, by = "customer_id")
  
  list(
    base_data = base_data,
    time_data = time_data
  )
}

customer_csv_candidates <- c(
  file.path("RShiny", "data", "customer_data.csv"),
  file.path("data", "customer_data.csv"),
  file.path("..", "RShiny", "data", "customer_data.csv"),
  file.path("..", "data", "customer_data.csv")
)

tx_csv_candidates <- c(
  file.path("RShiny", "data", "transactions_data.csv"),
  file.path("data", "transactions_data.csv"),
  file.path("..", "RShiny", "data", "transactions_data.csv"),
  file.path("..", "data", "transactions_data.csv")
)


# Global Variables
cluster_vars_master <- c("monthly_transaction_count", "average_transaction_value", 
                         "total_transaction_volume", "transaction_frequency", 
                         "avg_daily_transactions", "app_logins_frequency", 
                         "feature_usage_diversity", "active_products", 
                         "weekend_transaction_ratio", "customer_tenure")

demo_vars_master <- c("gender", "income_bracket", "education_level", 
                      "marital_status", "investment_account", "insurance_product",
                      "acquisition_channel", "customer_segment", "savings_account",
                      "credit_card", "personal_loan", "bill_payment_user", "auto_savings_enabled")

cluster_vars <- intersect(cluster_vars_master, names(base_data))
demo_vars <- intersect(demo_vars_master, names(base_data))

if (length(cluster_vars) < 2) {
  fallback_numeric <- setdiff(names(base_data)[vapply(base_data, is.numeric, logical(1))], "customer_id")
  cluster_vars <- fallback_numeric[seq_len(min(length(fallback_numeric), 10))]
}

if (length(demo_vars) == 0) {
  fallback_categorical <- names(base_data)[
    vapply(base_data, function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))
  ]
  demo_vars <- fallback_categorical[seq_len(min(length(fallback_categorical), 10))]
}

confirm_numeric_vars <- names(base_data)[vapply(base_data, is.numeric, logical(1))]
confirm_numeric_vars <- setdiff(confirm_numeric_vars, c("customer_id"))

confirm_categorical_vars <- names(base_data)[
  vapply(base_data, function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))
]

pick_first <- function(preferred, pool) {
  hit <- preferred[preferred %in% pool]
  if (length(hit) > 0) hit[1] else if (length(pool) > 0) pool[1] else NA_character_
}

humanize_var <- function(x) {
  if (is.na(x) || is.null(x)) return("")
  x %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_squish()
}

h1_outcome_default <- pick_first(c("customer_lifetime_value", "churn_probability", "total_transaction_volume"), confirm_numeric_vars)
h1_predictor_default <- pick_first(c("total_transaction_volume", "monthly_transaction_count", "avg_daily_transactions"), setdiff(confirm_numeric_vars, h1_outcome_default))
h2_group_default <- pick_first(c("customer_segment", "income_bracket", "gender"), confirm_categorical_vars)
h2_value_default <- pick_first(c("total_transaction_volume", "average_transaction_value", "monthly_transaction_count"), confirm_numeric_vars)

morandi_colors <- c(
  "#9297A0", "#D4C5B9", "#A8B7AB", "#C3A6A0","#B18E88","#7E8B92","#E5D9D0",
  "#8E9E94", "#C6B48F", "#988C95"  )

#====================================================
#MAIN BODY - START
#====================================================

header <- dashboardHeader(title = " Colombian Fintech Financial Analytics")

# sidebar <- dashboardSidebar(
#   sidebarMenu(
#     width = 100,
#     selected = "Clustering",
#     menuItem(
#       "Home",
#       icon = icon("home"),
#       href = "https://anneyang29.github.io/ISSS608-Group-Project/",
#       newtab = TRUE
#     ),
#     menuItem("Clustering Analysis", tabName = "Clustering", icon = icon("users")),
#     menuItem("Confirmatory Analysis", tabName = "Confirmatory", icon = icon("chart-bar")),
#     menuItem("Time-Oriented Data Analysis", tabName = "Time", icon = icon("chart-line"),
#              menuSubItem("Dashboard", tabName = "Time_Dashboard"),
#              menuSubItem("Cashflow Analysis", tabName = "Time_Cashflow"))
#   )
# )

# mytheme <- create_theme(
#  adminlte_color(light_blue = "#D4C5B9"),
#  adminlte_sidebar(width = "250px", dark_bg = "#2E3440", dark_hover_bg = "#D4C5B9", dark_color = "#FFFFFF"),
#  adminlte_global(content_bg = "#F8F9FA", box_bg = "#FFFFFF", info_box_bg = "#FFFFFF")
# )

#====================================================
# CLUSTERING UI - START
#====================================================

# STEP 1: Correlation 
ClusterRow1 <- fluidRow(
  column(3,
         card(title = "Filter Variables", status = "info", solidHeader = FALSE, width = NULL,
             selectizeInput("corr_vars", "Select Variables:", 
                            choices = cluster_vars, multiple = TRUE, 
                            selected = cluster_vars[1:4],
                            options = list(plugins = list("remove_button"))), 
             uiOutput("corr_warning")
         )
  ),
  column(9,
         card(title = "Correlation Matrix", status = "primary", solidHeader = TRUE, width = NULL, align = "center",
             withSpinner(plotOutput("corrPlot", height = "600px")))
  )
)

# STEP 2: Method Selection & Evaluation
ClusterRow2 <- fluidRow(
  column(3,
         card(
           title = "Clustering Method",
           status = "info",
           solidHeader = FALSE,
           width = NULL,
           
           radioButtons(
             "clust_method",
             "Select Algorithm:",
             choices = c("K-means", "PAM (CLARA)", "GMM"),
             selected = "K-means"
           ),
           
           br(),
           
           actionButton(
             "confirm_method",
             "Confirm Algorithm",
             class = "btn-primary"
           ),
           
           br(), br(),
           
           tags$small(
             "Please confirm the selected algorithm before generating evaluation plots."
           )
         )
  ),
  column(9,
         card(
           title = "Model Evaluation (Optimal k)",
           status = "primary",
           solidHeader = TRUE,
           width = NULL,
           uiOutput("eval_plots_ui"),
           hr(),
           h4("How to read the plot?", style = "color: #2E3440; font-weight: bold;"),
           uiOutput("eval_text")
         )
  )
)

# STEP 3: Cluster Size
ClusterRow3 <- fluidRow(
  column(3,
         card(
           title = "Clustering Settings",
           status = "info",
           solidHeader = FALSE,
           width = NULL,
           
           selectInput("k_val", "Number of k :", choices = 2:10, selected = 3),
           
           br(),
           
           actionButton(
             "run_cluster",
             "Run Clustering",
             class = "btn-primary"
           ),
           
           br(), br(),
           
           tags$small(
             "After selecting variables, method, and k, click this button to generate the clustering results."
           )
         )
  ),
  column(9,
         card(
           title = "STEP 3: Cluster Size Distribution",
           status = "primary",
           solidHeader = TRUE,
           width = NULL,
           align = "center",
           withSpinner(plotOutput("sizePlot", height = "500px"))
         )
  )
)

# STEP 4: Interpretation (Parallel & Demographics)
ClusterRow4 <- fluidRow(
  column(3,
         card(title = "Interpretation Settings", status = "info", solidHeader = FALSE, width = NULL,
             selectInput("demo_var", "Demographic Variable:", choices = demo_vars)
         )
  ),
  column(9,
         card(title = "Behavioural Contrast", status = "primary", solidHeader = TRUE, width = NULL,
             withSpinner(parallelPlotOutput("pPlot", height = "450px"))),
         
         card(title = "Demographic Composition", status = "primary", solidHeader = TRUE, width = NULL, align = "center",
             withSpinner(plotOutput("demoPlot", height = "400px")))
  )
)

# ClusterSubTabs 
ClusterSubTabs <- navset_card_tab(
  full_screen = TRUE,
  title = "Clustering Workflow",
  nav_panel("STEP 1: Correlation", ClusterRow1),
  nav_panel("STEP 2: Method Select", ClusterRow2),
  nav_panel("STEP 3: Cluster Size", ClusterRow3),
  nav_panel("STEP 4: Interpretation", ClusterRow4)
)

ConfirmRow1 <- fluidRow(
  column(3,
         tags$div(
           class = "confirmatory-sticky-sidebar",
           card(title = "H1 Settings", status = "info", solidHeader = FALSE, width = NULL,
               selectizeInput("h1_outcome", "Outcome (numeric):", choices = confirm_numeric_vars, selected = h1_outcome_default,
                              options = list(dropdownParent = "body")),
               selectizeInput("h1_predictor", "Predictor (numeric):", choices = confirm_numeric_vars, selected = h1_predictor_default,
                              options = list(dropdownParent = "body")),
               uiOutput("h1_input_warning")
           )
         )
  ),
  column(9,
         card(title = "Regression Results", status = "primary", solidHeader = TRUE, width = NULL,
             uiOutput("h1_sentence"),
             tableOutput("h1_summary"),
             hr(),
             withSpinner(plotOutput("h1_plot", height = "420px")),
             hr(),
             tags$p(tags$b("Diagnostics:"), " quick checks for linear fit assumptions."),
             fluidRow(
               column(6, withSpinner(plotOutput("h1_diag_resid", height = "260px"))),
               column(6, withSpinner(plotOutput("h1_diag_qq", height = "260px")))
             )
         ),
         card(
           title = "Chart Interpretation",
           status = "primary",
           solidHeader = TRUE,
           width = NULL,
           collapsible = TRUE,
           collapsed = TRUE,
           tags$p(
             "Regression estimates the direction and strength of association between a numeric predictor and a numeric outcome. ",
             "The fitted line summarises the expected change in the outcome as the predictor changes."
           ),
           tags$ul(
             tags$li(tags$b("Slope"), ": direction (+/−) and magnitude of change in the outcome per 1-unit increase in the predictor."),
             tags$li(tags$b("p-value"), ": if < 0.05, evidence of an association (not necessarily causation)."),
             tags$li(tags$b("R-squared"), ": proportion of outcome variance explained by the predictor."),
             tags$li(HTML("<b>f<sup>2</sup></b>: effect size where ~0.02 is small, ~0.15 medium, ~0.35 large (rule of thumb).")),
             tags$li(tags$b("Diagnostics"), ": residual plot and Q-Q plot help check linear model assumptions.")
           ),
           tags$p("Tip: statistical significance does not always imply practical importance—consider effect size and business context.")
         )
  )
)

ConfirmRow2 <- fluidRow(
  column(3,
         tags$div(
           class = "confirmatory-sticky-sidebar",
           card(title = "H2 Settings", status = "info", solidHeader = FALSE, width = NULL,
               selectizeInput("h2_group", "Group variable:", choices = confirm_categorical_vars, selected = h2_group_default,
                              options = list(dropdownParent = "body")),
               selectizeInput("h2_value", "Value (numeric):", choices = confirm_numeric_vars, selected = h2_value_default,
                              options = list(dropdownParent = "body")),
               selectizeInput(
                 "h2_test_type",
                 "Test type:",
                 choices = c(
                   "Auto (recommended)" = "auto",
                   "Parametric (ANOVA/Welch)" = "parametric",
                   "Non-parametric (Kruskal-Wallis)" = "nonparametric"
                 ),
                 selected = "auto",
                 options = list(dropdownParent = "body")
               ),
               selectizeInput(
                 "h2_pairwise_display",
                 "Pairwise display:",
                 choices = c(
                   "Significant only" = "significant",
                   "All" = "all"
                 ),
                 selected = "significant",
                 options = list(dropdownParent = "body")
               ),
               selectizeInput(
                 "h2_p_adjust",
                 "P-value adjustment method:",
                 choices = p.adjust.methods,
                 selected = "BH",
                 options = list(dropdownParent = "body")
               )
           )
         )
  ),
  column(9,
         card(title = "ANOVA Results (or Kruskal-Wallis)", status = "primary", solidHeader = TRUE, width = NULL,
             uiOutput("h2_sentence"),
             tableOutput("h2_summary"),
             hr(),
             withSpinner(plotOutput("h2_plot", height = "420px")),
             uiOutput("h2_posthoc_ui")
         ),
         card(
           title = "Chart Interpretation",
           status = "primary",
           solidHeader = TRUE,
           width = NULL,
           collapsible = TRUE,
           collapsed = TRUE,
           tags$p(
             "Group-difference tests evaluate whether a numeric metric differs across two or more groups. ",
             "When assumptions are reasonable the app uses ANOVA; if variances differ it uses Welch ANOVA; otherwise it falls back to Kruskal-Wallis (non-parametric)."
           ),
           tags$ul(
             tags$li(tags$b("p-value"), ": if < 0.05, at least one group differs (overall test)."),
             tags$li(HTML("<b>Effect size</b>: &eta;<sup>2</sup> (ANOVA) or &epsilon;<sup>2</sup> (Kruskal-Wallis) summarise the magnitude of group differences.")),
             tags$li(tags$b("Post-hoc"), ": identifies which group pairs differ after multiple-testing adjustment."),
             tags$li(tags$b("Plot"), ": violin + boxplot + points show distribution, median/IQR, and individual observations; mean markers are annotated.")
           ),
           tags$p("Tip: focus on practical separation between distributions, not just statistical significance.")
         )
  )
)

ConfirmSubTabs <- tags$div(
  class = "confirmatory-shell",
  tabsetPanel(
    tabPanel("Regression", ConfirmRow1),
    tabPanel("ANOVA", ConfirmRow2)
  )
)

#==========================================
# Time Data Analysis UI 
#==========================================

# import custom functions
source("time_functions.R")

# variables for selection
customer_types <-  c("Gender" = "gender", 
                     "Acquisition channel" = "acquisition_channel",
                     "Customer Segment" = "customer_segment",
                     "Location" = "location",
                     "Customer Lifetime Value" = "clv_segment",
                     "Income Bracket" = "income_bracket")

time_dashboard_filters <- sidebar(
  title = h3("Dashboard Filters"),
  bg = "lightgrey",
  uiOutput("time_tx_type_ui"),
  dateRangeInput("time_date_range", "Transaction Date Range",
                 start = "2023-01-01", end = "2023-12-31", format = "yyyy-mm-dd"),
  selectInput("time_cus_seg", "Segment Customers by", choices = customer_types),
  checkboxGroupInput("time_selected_seg", "Select Segments:", choices = NULL),
  actionLink("select_all_seg", "Select All / Reset", class = "btn btn-outline-primary btn-sm")
)
  
time_key_stats <- layout_column_wrap(
  fill = TRUE,
  value_box(
    title = "TOP TX MONTH.",
    value = textOutput("stat_top_month"),
    showcase = icon("calendar"),
    theme = "primary",
    showcase_layout = "top right", 
    full_screen = FALSE
  ),
  value_box(
    title = "NEW CUSTOMERS",
    value = textOutput("stat_new_cust"),
    showcase = icon("user-plus"),
    theme = "info",
    showcase_layout = "top right", 
    full_screen = FALSE
  ),
  value_box(
    title = "M-O-M TX CHANGE",
    value = textOutput("stat_tx_change"),
    showcase = icon("chart-line"),
    theme = "success",
    showcase_layout = "top right", 
    full_screen = FALSE
  ),
  value_box(
    title = "TOTAL CUSTOMERS",
    value = textOutput("stat_total_cust"),
    showcase = icon("users-viewfinder"),
    theme = "warning",
    showcase_layout = "top right", 
    full_screen = FALSE
  )
)

bubble_plot <- card(
  full_screen = TRUE,
  card_header(
    class = "d-flex justify-content-between align-items-center",
    "Customer Transaction Behavior Over Time",
    popover(
      bs_icon("funnel"),
      title = "Filter",
      radioButtons("time_granularity", "Time Granularity:", 
                   choices = c("Month" = "month", "Week" = "week"), 
                   inline = TRUE),
      placement = "left"
    )
  ),
  card_body(
    withSpinner(plotlyOutput("animation_plot"))
    )
)

cra_plot <- card(
  full_screen = TRUE,
  card_header(
    class = "d-flex justify-content-between align-items-left",
    tags$div("Customer Retention Analysis",
              tooltip(
                bs_icon("info-circle"),
                "Retention is calculated from the date of the first transaction."
    )),
    popover(
      bs_icon("funnel"), 
      title = "Filter",
      uiOutput("heatmap_segment_ui"),
      placement = "left"
    )
  ),
  card_body(
    withSpinner(plotlyOutput("cra_heatmap"))
  )
)

seasonal_analysis_card <- navset_card_pill(
  full_screen = TRUE,
  placement = "above",
  
  nav_panel("Cycle Plot",
            card_body(
              popover(
                bs_icon("funnel"),
                title = "Filters",
                radioButtons("season_metric", "Metric:", 
                             choices = c("Transaction Count" = "tx_count", 
                                         "Transaction Volume" = "tx_amt", 
                                         "Unique Users" = "user_count")),
                placement = "right"
              ),
              # Set a fixed height to avoid the 'margins too large' error
              withSpinner(plotOutput("cycle_plot"))
            )
  ),
  
  nav_panel("STL Decomposition", 
            card_body( 
              popover(
                bs_icon("funnel"),
                title = "Filters",
                radioButtons("stl_metric", "Metric:", 
                             choices = c("Transaction Count" = "tx_count", 
                                         "Transaction Volume" = "tx_amt", 
                                         "Unique Users" = "user_count")),
                selectInput("stl_model_type", "Decomposition model:", 
                            choices = c("STL Decomposition" = "STLM", 
                                        "Classical Decomposition (Additive)" = "Classical (add)", 
                                        "Classical Decomposition (Multiplicative)" = "Classical (mult)")),
                selectInput("stl_season", "Seasonal Period:",
                            choices = c("1 week", "1 month", "3 months")),
                sliderInput("stl_season_window", "Seasonal window:",
                            value=7, min=7, max=99, step=2),
                sliderInput("stl_trend_window", "Trend window:",
                            value=11, min=11, max=99, step=2),
                placement = "right"
              ),
              withSpinner(plotOutput("stl_plot"))
            )
  )
)

Dashboard <- page_fluid(
  layout_sidebar(
    sidebar = time_dashboard_filters,
    layout_columns(col_widths = 12,
                    time_key_stats,
                    bubble_plot,
                    layout_columns(
                      col_widths = c(6,6),
                      cra_plot,
                      seasonal_analysis_card)
                  )
  ))


CashflowSubTabs <- tabsetPanel(
  id = "CashflowSubTabs",
  tabPanel("Historical Cashflow Analysis", 
           h2("Title")),
  tabPanel("Future Cashflow Prediction", 
           h2("Title2"))
)

#==========================================
# Body Layout
#==========================================
lux_morandi_theme <- bs_theme(
  version = 5,
  bootswatch = "lux",
  primary = "#2E3440",         # Dark Slate from your original sidebar
  secondary = "#9297A0",       # Morandi Grey
  success = "#A8B7AB",         # Morandi Sage
  info = "#D4C5B9",            # Morandi Sand
  warning = "#C6B48F",         # Morandi Gold
  danger = "#C3A6A0",          # Morandi Rose
  base_font = font_google("Nunito Sans"),
  heading_font = font_google("Quicksand"),
  "navbar-bg" = "#2E3440",
  "navbar-light-color" = "#FFFFFF",
  "navbar-light-active-color" = "#D4C5B9", # Morandi Sand for the active tab
  "navbar-light-hover-color" = "#A8B7AB"   # Morandi Sage for hover
)

app_ui_overrides <- tags$head(
  tags$style(HTML("\n    .navbar-brand {\n      font-size: 1.1rem;\n      font-weight: 700;\n      line-height: 1.2;\n    }\n\n    .navbar-nav .nav-link {\n      font-size: 0.98rem;\n    }\n\n    .bslib-page-navbar .container-fluid {\n      padding-left: 1.25rem;\n      padding-right: 1.25rem;\n    }\n\n    .bslib-page-navbar .card,\n    .bslib-page-navbar .value-box,\n    .bslib-page-navbar .sidebar {\n      margin-bottom: 1rem;\n    }\n\n    .bslib-page-navbar .card-header {\n      font-size: 1rem;\n      padding: 0.85rem 1rem;\n    }\n\n    .bslib-page-navbar .card-body,\n    .bslib-page-navbar .sidebar-content,\n    .bslib-page-navbar .form-label,\n    .bslib-page-navbar .form-control,\n    .bslib-page-navbar .selectize-input,\n    .bslib-page-navbar .btn {\n      font-size: 0.95rem;\n    }\n\n    .bslib-page-navbar .card-body {\n      padding: 1rem;\n    }\n\n    .bslib-page-navbar .tab-content > .tab-pane {\n      padding-top: 0.5rem;\n    }\n\n    .confirmatory-shell > .tabbable > .nav,\n    .confirmatory-shell > .nav {\n      position: sticky;\n      top: 0;\n      z-index: 20;\n      padding-top: 0.25rem;\n      padding-bottom: 0.35rem;\n      background: rgba(255, 255, 255, 0.96);\n      backdrop-filter: blur(6px);\n    }\n\n    .confirmatory-sticky-sidebar {\n      position: sticky;\n      top: 3.5rem;\n      z-index: 10;\n      align-self: flex-start;\n    }\n\n    .confirmatory-sticky-sidebar,\n    .confirmatory-sticky-sidebar .card,\n    .confirmatory-sticky-sidebar .card-body {\n      overflow: visible;\n    }\n\n    .popover,\n    .dropdown-menu,\n    .selectize-dropdown,\n    .selectize-dropdown-content {\n      z-index: 1080 !important;\n    }\n\n    .app-data-note-wrap {\n      display: flex;\n      justify-content: flex-end;\n      padding: 0 1.25rem 0.85rem;\n    }\n\n    .app-data-note {\n      max-width: 540px;\n      color: #6C757D;\n      font-size: 0.78rem;\n      line-height: 1.45;\n      text-align: right;\n    }\n\n    .app-data-note summary {\n      cursor: pointer;\n      list-style: none;\n      font-size: 0.76rem;\n      font-weight: 600;\n      color: #7E8B92;\n    }\n\n    .app-data-note summary::-webkit-details-marker {\n      display: none;\n    }\n\n    .app-data-note summary:hover {\n      color: #2E3440;\n    }\n\n    .app-data-note-content {\n      margin-top: 0.35rem;\n      padding-top: 0.35rem;\n      border-top: 1px solid #E5E7EB;\n    }\n\n    @media (max-width: 991.98px) {\n      .confirmatory-shell > .tabbable > .nav,\n      .confirmatory-shell > .nav,\n      .confirmatory-sticky-sidebar {\n        position: static;\n      }\n    }\n  "))
)

data_note_ui <- tags$div(
  class = "app-data-note-wrap",
  tags$details(
    class = "app-data-note",
    tags$summary("Data note"),
    tags$div(
      class = "app-data-note-content",
      HTML(paste0(
        "This dashboard uses the COFINFAD customer + transaction data provided for the project. ",
        "Several behavioural metrics (e.g., transaction frequency/volume features) are engineered from the raw transactions and stored in a prepared dataset (<i>shiny_base_data.rds</i>) for faster loading. ",
        "If the RDS is not found, the app will build it from the CSVs once. ",
        "The app does not generate new labels such as churn probability; it analyses the fields provided in the dataset."
      ))
    )
  )
)

ui <- page_navbar(
  title = HTML("Colombian Fintech <br> Financial Analytics"),
  theme = lux_morandi_theme,
  header = app_ui_overrides,
  
  nav_item(tags$a("Home",
                  href = "https://anneyang29.github.io/ISSS608-Group-Project/",
                  target = "_blank"
                    )),
  nav_panel("Clustering Analysis", ClusterSubTabs),
  nav_panel("Confirmatory Analysis", ConfirmSubTabs),
  
  nav_menu(
    "Time-Oriented Data Analysis",
    nav_panel("Transactions Dashboard", Dashboard),
    nav_panel("Cashflow Analysis", CashflowSubTabs)
  ),
  footer = data_note_ui
)

#====================================================
# SERVER - START
#====================================================
server <- function(input, output, session) { 
  
  time_data_rv <- reactiveVal(NULL)
  
  load_time_data <- function() {
    if (is.null(time_data_rv())) {
      if (is.na(time_data_path)) {
        stop("shiny_time_data.rds not found.", call. = FALSE)
      }
      
      message("Loading time_data from: ", time_data_path)
      td <- readRDS(time_data_path)$time_data
      time_data_rv(td)
    }
    
    time_data_rv()
  }
  
  output$time_tx_type_ui <- renderUI({
    td <- load_time_data()
    radioButtons(
      "time_tx_type",
      "Transaction type",
      choices = c("All", sort(unique(td$type))),
      selected = "All"
    )
  })
  
  raw_filtered_data <- reactive({
    req(input$time_tx_type, input$time_date_range, input$time_cus_seg, input$time_selected_seg)
    
    td <- load_time_data()
    dt <- as.data.table(td)
    
    res <- dt[date >= as.Date(input$time_date_range[1]) &
                date <= as.Date(input$time_date_range[2])]
    
    if (input$time_tx_type != "All") {
      res <- res[type == input$time_tx_type]
    }
    
    col_name <- input$time_cus_seg
    res <- res[res[[col_name]] %in% input$time_selected_seg]
    
    droplevels(as.data.frame(res))
  })
  
  confirmed_method <- eventReactive(input$confirm_method, {
    input$clust_method
  }, ignoreInit = FALSE)
  
  # --- STEP 1: Correlation Logic ---
  output$corr_warning <- renderUI({
    req(input$corr_vars)
    if(length(input$corr_vars) < 2) return(NULL)
    
    corr_matrix <- cor(base_data[, input$corr_vars], use = "pairwise.complete.obs")
    diag(corr_matrix) <- 0
    high_cor_indices <- which(abs(corr_matrix) >= 0.8, arr.ind = TRUE)
    
    if (nrow(high_cor_indices) > 0) {
      high_cor_indices <- high_cor_indices[high_cor_indices[,1] > high_cor_indices[,2], , drop = FALSE]
      pair_texts <- apply(high_cor_indices, 1, function(row) {
        paste0("• <b>", rownames(corr_matrix)[row[1]], "</b> & <b>", colnames(corr_matrix)[row[2]], "</b>")
      })
      HTML(paste0(
        "<div style='background-color: #F8D7DA; color: #9A3E41; padding: 12px; border-radius: 5px; margin-top: 15px; border-left: 4px solid #9A3E41;'>",
        " <b>Warning: High Correlation (>= |0.8|)</b><br>",
        "<span style='font-size: 0.9em;'>Please consider removing one of the following pairs to avoid multicollinearity:</span><br>",
        paste(pair_texts, collapse = "<br>"), "</div>"
      ))
    } else {
      HTML("<div style='color: #8E9775; padding: 10px; margin-top: 15px; font-weight: bold;'>No highly correlated variables detected.</div>")
    }
  })
  
  output$corrPlot <- renderPlot({
    req(input$corr_vars)
    if(length(input$corr_vars) < 2) return(NULL)
    plot_data <- base_data %>% select(all_of(input$corr_vars)) %>% mutate(across(everything(), as.numeric))
    cor_matrix <- cor(plot_data, use = "pairwise.complete.obs")
    cor_matrix[upper.tri(cor_matrix, diag = FALSE)] <- NA
    cor_long <- as.data.frame(as.table(cor_matrix)) %>% rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq) %>% drop_na()
    
    var_order <- colnames(cor_matrix)
    cor_long$Variable1 <- factor(cor_long$Variable1, levels = rev(var_order))
    cor_long$Variable2 <- factor(cor_long$Variable2, levels = var_order)
    
    ggplot(cor_long, aes(Variable1, Variable2, fill = Correlation)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "#7B8B97", mid = "#F8F9FA", high = "#C3A6A0", midpoint = 0, limits = c(-1, 1), name = "Correlation") +
      geom_text(aes(label = round(Correlation, 3)), color = "black", size = 3) +
      labs(x = "", y = "") + theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_blank())
  })
  
  # --------- STEP 2: Evaluation UI ---------

  output$eval_plots_ui <- renderUI({
    if (input$confirm_method == 0) {
      return(tags$p("Please confirm an algorithm to generate the evaluation plot."))
    }
    
    if (confirmed_method() == "K-means") {
      fluidRow(
        column(6, withSpinner(plotOutput("elbowPlot", height = "400px"))),
        column(6, withSpinner(plotOutput("ratioPlot", height = "400px")))
      )
    } else if (confirmed_method() == "PAM (CLARA)") {
      withSpinner(plotOutput("silPlot", height = "400px"))
    } else {
      withSpinner(plotOutput("bicPlot", height = "400px"))
    }
  })
  
  # --------- STEP 2: Evaluation text ---------

  output$eval_text <- renderUI({
    if (input$confirm_method == 0) {
      return(tags$p("Please confirm an algorithm first."))
    }
    
    if (confirmed_method() == "K-means") {
      HTML("
      <p><b>Elbow (WSS):</b> Look for the point where the decrease in WSS starts to slow down.</p>
      <p><b>BetweenSS / TotalSS ratio:</b> Higher values indicate stronger separation between clusters.</p>
    ")
    } else if (confirmed_method() == "PAM (CLARA)") {
      HTML("
      <p><b>Average Silhouette Score:</b> Higher values suggest better cluster compactness and separation.</p>
    ")
    } else {
      HTML("
      <p><b>GMM BIC Plot:</b> Lower BIC generally indicates a better model fit with appropriate complexity.</p>
    ")
    }
  })

  # --------- STEP 2A: K-means evaluation ---------
  
  kmeans_metrics <- eventReactive(input$confirm_method, {
    req(input$corr_vars, confirmed_method() == "K-means")
    
    X <- base_data %>%
      dplyr::select(all_of(input$corr_vars)) %>%
      dplyr::mutate(across(everything(), as.numeric)) %>%
      as.matrix()
    
    X <- scale(X)
    
    purrr::map_dfr(2:8, function(k) {
      set.seed(2022)
      km <- kmeans(X, centers = k, nstart = 10, iter.max = 50)
      
      tibble::tibble(
        k = k,
        tot_withinss = km$tot.withinss,
        between_ratio = km$betweenss / km$totss
      )
    })
  }, ignoreInit = FALSE)
  
  output$elbowPlot <- renderPlot({
    req(confirmed_method() == "K-means")
    metrics <- kmeans_metrics()
    
    ggplot(metrics, aes(x = k, y = tot_withinss)) +
      geom_line() +
      geom_point() +
      labs(
        title = "Elbow Plot (WSS)",
        x = "Number of Clusters (k)",
        y = "Total Within-Cluster Sum of Squares"
      ) +
      theme_minimal(base_size = 14)
  })
  
  output$ratioPlot <- renderPlot({
    req(confirmed_method() == "K-means")
    metrics <- kmeans_metrics()
    
    ggplot(metrics, aes(x = k, y = between_ratio)) +
      geom_line() +
      geom_point() +
      labs(
        title = "BetweenSS / TotalSS Ratio",
        x = "Number of Clusters (k)",
        y = "BetweenSS / TotalSS"
      ) +
      theme_minimal(base_size = 14)
  })

  # --------- STEP 2B: PAM (CLARA) evaluation ---------

  clara_metrics <- eventReactive(input$confirm_method, {
    req(input$corr_vars, confirmed_method() == "PAM (CLARA)")
    
    X <- base_data %>%
      dplyr::select(all_of(input$corr_vars)) %>%
      dplyr::mutate(across(everything(), as.numeric)) %>%
      as.matrix()
    
    X <- scale(X)
    
    purrr::map_dfr(2:8, function(k) {
      cl <- cluster::clara(X, k = k, samples = 3)
      
      tibble::tibble(
        k = k,
        avg_silhouette = cl$silinfo$avg.width
      )
    })
  }, ignoreInit = FALSE)
  
  output$silPlot <- renderPlot({
    req(confirmed_method() == "PAM (CLARA)")
    clara_res <- clara_metrics()
    
    ggplot(clara_res, aes(x = k, y = avg_silhouette)) +
      geom_line() +
      geom_point() +
      labs(
        title = "Average Silhouette Score",
        x = "Number of Clusters (k)",
        y = "Average Silhouette Width"
      ) +
      theme_minimal(base_size = 14)
  })

  # --------- STEP 2C: GMM evaluation ---------

  gmm_metrics <- eventReactive(input$confirm_method, {
    req(input$corr_vars, confirmed_method() == "GMM")
    
    X <- base_data %>%
      dplyr::select(all_of(input$corr_vars)) %>%
      dplyr::mutate(across(everything(), as.numeric)) %>%
      as.matrix()
    
    X <- scale(X)
    
    validate(
      need(nrow(X) <= 3000, "GMM evaluation is disabled for large datasets.")
    )
    
    gmm_fit <- mclust::Mclust(X, G = 2:8)
    
    tibble::tibble(
      k = as.numeric(names(gmm_fit$BIC)),
      BIC = as.numeric(gmm_fit$BIC)
    ) %>%
      dplyr::filter(!is.na(BIC))
  }, ignoreInit = FALSE)
  
  output$bicPlot <- renderPlot({
    req(confirmed_method() == "GMM")
    bic_df <- gmm_metrics()
    
    ggplot(bic_df, aes(x = k, y = BIC)) +
      geom_line() +
      geom_point() +
      labs(
        title = "GMM BIC Plot",
        x = "Number of Clusters (k)",
        y = "BIC"
      ) +
      theme_minimal(base_size = 14)
  })

  # --------- STEP 3 & 4: Final clustering ---------
  
  dynamic_clusters <- eventReactive(input$run_cluster, {
    req(input$corr_vars, input$k_val, confirmed_method())
    
    X <- base_data %>%
      dplyr::select(all_of(input$corr_vars)) %>%
      dplyr::mutate(across(everything(), as.numeric)) %>%
      as.matrix()
    
    X <- scale(X)
    k <- as.numeric(input$k_val)
    
    if (confirmed_method() == "K-means") {
      set.seed(2022)
      fit <- kmeans(X, centers = k, nstart = 10, iter.max = 50)
      clusters <- factor(fit$cluster)
      
    } else if (confirmed_method() == "PAM (CLARA)") {
      fit <- cluster::clara(X, k = k, samples = 3)
      clusters <- factor(fit$clustering)
      
    } else {
      validate(
        need(nrow(X) <= 3000, "GMM is only available for smaller datasets in the app.")
      )
      fit <- mclust::Mclust(X, G = k)
      clusters <- factor(fit$classification)
    }
    
    base_data %>% dplyr::mutate(cluster = clusters)
  }, ignoreInit = TRUE)

  # --------- STEP 3: Cluster size plot ---------

  output$sizePlot <- renderPlot({
    clustered_data <- dynamic_clusters()
    if (is.null(clustered_data)) return(NULL)
    
    color_palette <- colorRampPalette(morandi_colors)(
      length(unique(clustered_data$cluster))
    )
    
    ggplot(clustered_data, aes(x = cluster, fill = cluster)) +
      geom_bar(aes(y = after_stat(count) / sum(after_stat(count)))) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = color_palette) +
      labs(
        title = paste("Cluster Size Distribution -", confirmed_method()),
        x = "Cluster",
        y = "Proportion of Customers"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  # --------- STEP 4: Parallel plot ---------
  output$pPlot <- renderParallelPlot({
    clustered_data <- dynamic_clusters()
    if (is.null(clustered_data)) return(NULL)
    
    pp_data <- clustered_data %>%
      dplyr::filter(!is.na(cluster)) %>%
      dplyr::select(cluster, dplyr::all_of(input$corr_vars))
    
    sample_n <- min(1000, nrow(pp_data))
    
    if (sample_n > 0) {
      pp_data <- pp_data %>%
        dplyr::slice_sample(n = sample_n)
    }
    
    parallelPlot::parallelPlot(
      data = pp_data,
      refColumnDim = "cluster",
      rotateTitle = TRUE,
      histoVisibility = names(pp_data)
    )
  })
  
  # -------------------------------
  # STEP 4: Demographic composition
  # -------------------------------
  output$demoPlot <- renderPlot({
    req(input$demo_var)
    
    clustered_data <- dynamic_clusters()
    if (is.null(clustered_data)) return(NULL)
    
    plot_data <- clustered_data %>%
      dplyr::filter(!is.na(.data[[input$demo_var]]))
    
    num_categories <- length(unique(plot_data[[input$demo_var]]))
    color_palette <- colorRampPalette(morandi_colors)(num_categories)
    
    ggplot(plot_data, aes(x = cluster, fill = .data[[input$demo_var]])) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = color_palette) +
      labs(
        title = paste("Demographic Distribution by", input$demo_var),
        x = "Cluster",
        y = "Proportion",
        fill = input$demo_var
      ) +
      theme_minimal(base_size = 14)
  })

  # --- CONFIRMATORY: H1 Regression ---
  output$h1_input_warning <- renderUI({
    req(input$h1_outcome, input$h1_predictor)
    if (identical(input$h1_outcome, input$h1_predictor)) {
      HTML("<div style='background-color: #F8D7DA; color: #9A3E41; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #9A3E41;'><b>Warning:</b> Outcome and predictor should be different variables.</div>")
    } else {
      NULL
    }
  })

  h1_result <- reactive({
    req(input$h1_outcome, input$h1_predictor)
    validate(need(!identical(input$h1_outcome, input$h1_predictor), "Select different variables for outcome and predictor."))

    dff <- base_data %>%
      transmute(
        outcome = as.numeric(.data[[input$h1_outcome]]),
        predictor = as.numeric(.data[[input$h1_predictor]])
      ) %>%
      drop_na()

    validate(
      need(nrow(dff) >= 10, "Not enough complete rows for regression (need at least 10)."),
      need(length(unique(dff$predictor)) >= 2, "Predictor requires at least two unique values.")
    )

    fit <- lm(outcome ~ predictor, data = dff)
    fit_summary <- summary(fit)
    slope <- fit_summary$coefficients["predictor", "Estimate"]
    p_value <- fit_summary$coefficients["predictor", "Pr(>|t|)"]
    r_squared <- fit_summary$r.squared
    f2 <- if (!is.na(r_squared) && r_squared < 1) r_squared / (1 - r_squared) else NA_real_

    f2_label <- if (is.na(f2)) {
      NA_character_
    } else if (f2 < 0.02) {
      "Very small"
    } else if (f2 < 0.15) {
      "Small"
    } else if (f2 < 0.35) {
      "Medium"
    } else {
      "Large"
    }

    list(
      data = dff,
      fit = fit,
      n = nrow(dff),
      slope = slope,
      p_value = p_value,
      r_squared = r_squared,
      f2 = f2,
      f2_label = f2_label,
      decision = ifelse(p_value < 0.05, "Reject H0", "Fail to reject H0")
    )
  })

  output$h1_summary <- renderTable({
    res <- h1_result()
    tibble(
      Metric = c("Outcome", "Predictor", "N", "Slope", "p-value", "R-squared", "f^2", "Effect size (rule of thumb)", "Decision"),
      Value = c(
        input$h1_outcome,
        input$h1_predictor,
        res$n,
        formatC(res$slope, digits = 4, format = "f"),
        formatC(res$p_value, digits = 4, format = "f"),
        formatC(res$r_squared, digits = 3, format = "f"),
        ifelse(is.na(res$f2), NA_character_, formatC(res$f2, digits = 3, format = "f")),
        ifelse(is.na(res$f2_label), NA_character_, res$f2_label),
        res$decision
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

  output$h1_sentence <- renderUI({
    res <- h1_result()

    outcome_label <- humanize_var(input$h1_outcome)
    predictor_label <- humanize_var(input$h1_predictor)
    p_txt <- formatC(res$p_value, digits = 4, format = "f")

    if (is.na(res$p_value)) return(NULL)

    if (res$p_value < 0.05) {
      direction <- if (is.na(res$slope)) {
        ""
      } else if (res$slope > 0) {
        "higher"
      } else if (res$slope < 0) {
        "lower"
      } else {
        "similar"
      }

      sentence <- if (direction %in% c("higher", "lower")) {
        paste0("There is evidence that ", predictor_label, " is associated with ", direction, " ", outcome_label,
               " (p = ", p_txt, ").")
      } else {
        paste0("There is evidence of an association between ", predictor_label, " and ", outcome_label,
               " (p = ", p_txt, ").")
      }
    } else {
      sentence <- paste0("No statistically significant evidence that ", predictor_label, " affects ", outcome_label,
                         " at the 5% level (p = ", p_txt, ").")
    }

    effect_note <- if (!is.na(res$f2)) {
      paste0(
        " Effect size: f<sup>2</sup> = ",
        formatC(res$f2, digits = 3, format = "f"),
        if (!is.na(res$f2_label)) paste0(" (", res$f2_label, ")") else "",
        "."
      )
    } else ""

    churn_note <- if (!is.null(input$h1_outcome) && identical(tolower(input$h1_outcome), "churn_probability")) {
      " <i>(churn_probability is source-provided in COFINFAD)</i>"
    } else {
      ""
    }

    tags$div(
      style = "margin: 10px 0; padding: 10px 12px; background: #F8F9FA; border-left: 4px solid #D4C5B9;",
      HTML(paste0("<b>Result summary:</b> ", sentence, effect_note, churn_note))
    )
  })

  output$h1_plot <- renderPlot({
    res <- h1_result()

    ggplot(res$data, aes(x = predictor, y = outcome)) +
      geom_point(alpha = 0.35, size = 1.3) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      theme_minimal(base_size = 13) +
      labs(
        title = paste(input$h1_outcome, "vs", input$h1_predictor),
        x = input$h1_predictor,
        y = input$h1_outcome
      )
  })

  output$h1_diag_resid <- renderPlot({
    res <- h1_result()

    diag_df <- tibble(
      fitted = fitted(res$fit),
      residual = resid(res$fit)
    )

    ggplot(diag_df, aes(x = fitted, y = residual)) +
      geom_point(alpha = 0.35, size = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      theme_minimal(base_size = 12) +
      labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals")
  })

  output$h1_diag_qq <- renderPlot({
    res <- h1_result()

    diag_df <- tibble(residual = resid(res$fit))

    ggplot(diag_df, aes(sample = residual)) +
      stat_qq(alpha = 0.45) +
      stat_qq_line() +
      theme_minimal(base_size = 12) +
      labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Sample Quantiles")
  })

  # --- CONFIRMATORY: H2 Group Difference ---
  h2_result <- reactive({
    req(input$h2_group, input$h2_value, input$h2_test_type, input$h2_pairwise_display, input$h2_p_adjust)

    alpha <- 0.05

    dff <- base_data %>%
      transmute(
        group = as.character(.data[[input$h2_group]]),
        value = as.numeric(.data[[input$h2_value]])
      ) %>%
      drop_na()

    counts <- table(dff$group)
    keep_groups <- names(counts[counts >= 3])
    dff <- dff %>% filter(group %in% keep_groups)

    split_vals <- split(dff$value, dff$group)
    validate(need(length(split_vals) >= 2, "Need at least two groups with >=3 observations each."))

    group_ns <- vapply(split_vals, length, integer(1))
    approx_normal_by_clt <- all(group_ns >= 30)

    test_type <- if (is.null(input$h2_test_type) || is.na(input$h2_test_type)) "auto" else input$h2_test_type

    normality_pass <- if (test_type == "parametric") {
      TRUE
    } else if (approx_normal_by_clt) {
      TRUE
    } else {
      all(vapply(split_vals, function(vals) {
        if (length(vals) < 3) return(FALSE)
        sample_vals <- if (length(vals) > 5000) sample(vals, 5000) else vals
        if (length(unique(sample_vals)) < 3) return(FALSE)
        shapiro.test(sample_vals)$p.value >= alpha
      }, logical(1)))
    }

    var_p <- tryCatch(bartlett.test(value ~ group, data = dff)$p.value, error = function(e) NA_real_)
    variance_pass <- !is.na(var_p) && var_p >= alpha

    p_adj_method <- if (is.null(input$h2_p_adjust) || is.na(input$h2_p_adjust)) "BH" else input$h2_p_adjust

    if (test_type == "nonparametric") {
      kw <- kruskal.test(value ~ group, data = dff)
      stat <- as.numeric(kw$statistic)
      p_value <- kw$p.value
      test_name <- "Kruskal-Wallis (forced)"

      n_total <- nrow(dff)
      k_groups <- length(unique(dff$group))
      effect_size_name <- "epsilon^2"
      effect_size <- if (n_total > k_groups) {
        max(0, (stat - k_groups + 1) / (n_total - k_groups))
      } else {
        NA_real_
      }

      posthoc <- tryCatch({
        pw <- pairwise.wilcox.test(dff$value, dff$group, p.adjust.method = p_adj_method)
        mat <- pw$p.value
        if (is.null(mat)) return(tibble(Comparison = character(), p_adj = numeric()))
        idx <- which(!is.na(mat), arr.ind = TRUE)
        tibble(
          Group1 = rownames(mat)[idx[, 1]],
          Group2 = colnames(mat)[idx[, 2]],
          p_adj = mat[idx]
        ) %>%
          mutate(Comparison = paste(Group1, "vs", Group2)) %>%
          select(Comparison, p_adj)
      }, error = function(e) {
        tibble(Comparison = character(), p_adj = numeric())
      })
    } else if (normality_pass) {
      if (variance_pass) {
        fit <- aov(value ~ group, data = dff)
        stat <- summary(fit)[[1]]["group", "F value"]
        p_value <- summary(fit)[[1]]["group", "Pr(>F)"]
        test_name <- if (test_type == "parametric") {
          "One-way ANOVA (forced)"
        } else if (approx_normal_by_clt) {
          "One-way ANOVA (CLT)"
        } else {
          "One-way ANOVA"
        }

        sm <- summary(fit)[[1]]
        ss_group <- as.numeric(sm["group", "Sum Sq"])
        ss_total <- sum(as.numeric(sm[, "Sum Sq"]), na.rm = TRUE)
        effect_size_name <- "eta^2"
        effect_size <- if (!is.na(ss_group) && ss_total > 0) ss_group / ss_total else NA_real_

        posthoc <- tryCatch({
          tk <- TukeyHSD(fit)$group
          as_tibble(tk, rownames = "Comparison") %>%
            transmute(
              Comparison,
              diff = diff,
              lwr = lwr,
              upr = upr,
              p_adj = `p adj`
            )
        }, error = function(e) {
          tibble(Comparison = character(), diff = numeric(), lwr = numeric(), upr = numeric(), p_adj = numeric())
        })
      } else {
        wt <- oneway.test(value ~ group, data = dff, var.equal = FALSE)
        stat <- as.numeric(wt$statistic)
        p_value <- wt$p.value
        test_name <- if (test_type == "parametric") {
          "Welch ANOVA (forced)"
        } else if (approx_normal_by_clt) {
          "Welch ANOVA (CLT)"
        } else {
          "Welch ANOVA"
        }

        effect_size_name <- "eta^2"
        effect_size <- NA_real_

        posthoc <- tryCatch({
          pw <- pairwise.t.test(dff$value, dff$group, p.adjust.method = p_adj_method, pool.sd = FALSE)
          mat <- pw$p.value
          if (is.null(mat)) return(tibble(Comparison = character(), p_adj = numeric()))
          idx <- which(!is.na(mat), arr.ind = TRUE)
          tibble(
            Group1 = rownames(mat)[idx[, 1]],
            Group2 = colnames(mat)[idx[, 2]],
            p_adj = mat[idx]
          ) %>%
            mutate(Comparison = paste(Group1, "vs", Group2)) %>%
            select(Comparison, p_adj)
        }, error = function(e) {
          tibble(Comparison = character(), p_adj = numeric())
        })
      }
    } else {
      kw <- kruskal.test(value ~ group, data = dff)
      stat <- as.numeric(kw$statistic)
      p_value <- kw$p.value
      test_name <- "Kruskal-Wallis"

      n_total <- nrow(dff)
      k_groups <- length(unique(dff$group))
      effect_size_name <- "epsilon^2"
      effect_size <- if (n_total > k_groups) {
        max(0, (stat - k_groups + 1) / (n_total - k_groups))
      } else {
        NA_real_
      }

      posthoc <- tryCatch({
        pw <- pairwise.wilcox.test(dff$value, dff$group, p.adjust.method = p_adj_method)
        mat <- pw$p.value
        if (is.null(mat)) return(tibble(Comparison = character(), p_adj = numeric()))
        idx <- which(!is.na(mat), arr.ind = TRUE)
        tibble(
          Group1 = rownames(mat)[idx[, 1]],
          Group2 = colnames(mat)[idx[, 2]],
          p_adj = mat[idx]
        ) %>%
          mutate(Comparison = paste(Group1, "vs", Group2)) %>%
          select(Comparison, p_adj)
      }, error = function(e) {
        tibble(Comparison = character(), p_adj = numeric())
      })
    }

    posthoc <- posthoc %>%
      mutate(p_adj = as.numeric(p_adj)) %>%
      arrange(p_adj)

    display_mode <- if (is.null(input$h2_pairwise_display) || is.na(input$h2_pairwise_display)) "significant" else input$h2_pairwise_display
    display_note <- NULL
    if (identical(display_mode, "significant") && nrow(posthoc) > 0) {
      posthoc <- posthoc %>% filter(!is.na(p_adj) & p_adj < alpha)
      if (nrow(posthoc) == 0) {
        display_note <- paste0("No significant pairwise differences after ", p_adj_method, " adjustment (alpha = ", alpha, ").")
      }
    }

    max_rows <- 300
    posthoc_note <- NULL
    if (nrow(posthoc) > max_rows) {
      posthoc <- posthoc %>% slice_head(n = max_rows)
      posthoc_note <- paste0("Post-hoc table truncated to top ", max_rows, " comparisons (smallest adjusted p-values).")
    }

    if (is.null(posthoc_note) && !is.null(display_note)) posthoc_note <- display_note

    list(
      data = dff,
      test_name = test_name,
      statistic = stat,
      p_value = p_value,
      n = nrow(dff),
      k = length(unique(dff$group)),
      effect_size_name = effect_size_name,
      effect_size = effect_size,
      posthoc = posthoc,
      posthoc_note = posthoc_note,
      decision = ifelse(p_value < 0.05, "Reject H0", "Fail to reject H0")
    )
  })

  output$h2_summary <- renderTable({
    res <- h2_result()
    tibble(
      Metric = c("Group variable", "Value variable", "N", "# Groups", "Test used", "Statistic", "p-value", "Effect size", "Decision"),
      Value = c(
        input$h2_group,
        input$h2_value,
        res$n,
        res$k,
        res$test_name,
        formatC(res$statistic, digits = 3, format = "f"),
        formatC(res$p_value, digits = 4, format = "f"),
        if (is.na(res$effect_size)) {
          if (grepl("Welch", res$test_name, fixed = TRUE)) {
            "Effect size not computed for Welch ANOVA"
          } else {
            "NA"
          }
        } else {
          paste0(res$effect_size_name, " = ", formatC(res$effect_size, digits = 3, format = "f"))
        },
        res$decision
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

  output$h2_plot <- renderPlot({
    res <- h2_result()

    means_df <- res$data %>%
      group_by(group) %>%
      summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

    value_range <- range(res$data$value, na.rm = TRUE)
    nudge_y <- 0.03 * diff(value_range)
    if (!is.finite(nudge_y) || nudge_y == 0) nudge_y <- 0

    ggplot(res$data, aes(x = group, y = value)) +
      geom_violin(aes(color = group), fill = NA, alpha = 0.35, show.legend = FALSE, trim = FALSE) +
      geom_boxplot(aes(color = group), width = 0.22, outlier.alpha = 0, show.legend = FALSE) +
      geom_point(aes(color = group), position = position_jitter(width = 0.14, height = 0), alpha = 0.35, size = 1.1, show.legend = FALSE) +
      geom_point(data = means_df, aes(x = group, y = mean_value, color = group), inherit.aes = FALSE, size = 2.2, show.legend = FALSE) +
      geom_label(
        data = means_df,
        aes(x = group, y = mean_value, label = paste0("Mean = ", formatC(mean_value, digits = 1, format = "f"))),
        inherit.aes = FALSE,
        nudge_y = nudge_y,
        size = 3,
        label.size = 0.2,
        show.legend = FALSE
      ) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
      labs(
        title = paste(input$h2_value, "by", input$h2_group),
        x = input$h2_group,
        y = input$h2_value
      )
  })

  output$h2_sentence <- renderUI({
    res <- h2_result()

    group_label <- humanize_var(input$h2_group)
    value_label <- humanize_var(input$h2_value)
    p_txt <- formatC(res$p_value, digits = 4, format = "f")

    if (is.na(res$p_value)) return(NULL)

    if (res$p_value < 0.05) {
      sentence <- paste0("There is evidence that ", value_label, " differs across ", group_label,
                         " groups (", res$test_name, ", p = ", p_txt, ").")
    } else {
      sentence <- paste0("No statistically significant evidence that ", value_label, " differs across ", group_label,
                         " groups at the 5% level (", res$test_name, ", p = ", p_txt, ").")
    }

    effect_note <- if (!is.na(res$effect_size)) {
      paste0(" Effect size: ", res$effect_size_name, " = ", formatC(res$effect_size, digits = 3, format = "f"), ".")
    } else {
      ""
    }

    tags$div(
      style = "margin: 10px 0; padding: 10px 12px; background: #F8F9FA; border-left: 4px solid #D4C5B9;",
      HTML(paste0("<b>Result summary:</b> ", sentence, effect_note))
    )
  })

  output$h2_posthoc_ui <- renderUI({
    res <- h2_result()

    if (identical(res$decision, "Fail to reject H0")) {
      return(tags$div(style = "margin-top: 10px;", tags$em("Post-hoc comparisons are shown only when the overall test is significant (p < 0.05).")))
    }

    if (is.null(res$posthoc) || nrow(res$posthoc) == 0) {
      msg <- if (!is.null(res$posthoc_note)) res$posthoc_note else "No post-hoc results available for the current selection."
      return(tags$div(style = "margin-top: 10px;", tags$em(msg)))
    }

    tagList(
      hr(),
      tags$p(tags$b("Post-hoc comparisons:"), " pairwise differences with multiple-testing adjustment."),
      if (!is.null(res$posthoc_note)) tags$p(tags$em(res$posthoc_note)),
      tableOutput("h2_posthoc")
    )
  })

  output$h2_posthoc <- renderTable({
    res <- h2_result()
    res$posthoc
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")
  
  
  # --- TIME ANALYSIS : DASHBOARD ---
  
  # -- 1.1 DASHBOARD FILTERS -- 
  
  observeEvent(input$time_cus_seg, {
    seg_choices <- base_data[[input$time_cus_seg]] %>% 
      unique() %>% 
      sort() 
    
    # Initialize with all selected
    updateCheckboxGroupInput(
      session,
      inputId = "time_selected_seg",
      choices = seg_choices,
      selected = seg_choices 
    )
    prev_valid_sel(seg_choices)
  }, ignoreInit = FALSE, priority = 10)
  
  observeEvent(input$select_all_seg, {
    seg_choices <- base_data[[input$time_cus_seg]] %>% 
      unique() %>% 
      sort() %>% 
      as.character()
    
    updateCheckboxGroupInput(
      session,
      "time_selected_seg",
      selected = seg_choices
    )
  })
  
  # If user unchecks the last box, we force the previous selection back
  prev_valid_sel <- reactiveVal(character(0))
  
  observeEvent(input$time_selected_seg, {
    current <- input$time_selected_seg
    
    if (is.null(current) || length(current) == 0) {
      # Revert to previous valid selection
      updateCheckboxGroupInput(session, "time_selected_seg", selected = prev_valid_sel())
      showNotification("At least one segment must be selected.", type = "warning", duration = 2)
    } else {
      # Update the tracker with the valid selection
      prev_valid_sel(current)
    }
  }, ignoreNULL = FALSE)
  
  # Apply a 400ms delay after user clicks checkboxes before app recalculates
  dashboard_data <- raw_filtered_data %>% debounce(400)
  
  # --- 1.2 KEY STATS ----
  key_stats_data <- reactive({
    data <- dashboard_data()
    req(nrow(data)>0)
    list(
      top_month = get_top_month(data),
      new_cust = get_new_cust(data, input$time_date_range[2]),
      change = get_tx_change(data),
      total = n_distinct(data$customer_id)
    )
  })
  
  # Top tx Month
  output$stat_top_month <- renderText({
    res <- key_stats_data()
    as.character(res$top_month[1])
  })
  
  # 2. Monthly New Customers
  output$stat_new_cust <- renderText({
    stats <- key_stats_data()
    res <- stats$new_cust
    if(is.null(res) || length(res) == 0) return("0")
    res
  })
  
  # 3. Transaction Change (%)
  output$stat_tx_change <- renderText({
    stats <- key_stats_data()
    res <- stats$change
    paste0(round(res, 1), "%")
  })
  
  # 4. Total Customers
  output$stat_total_cust <- renderText({
    stats <- key_stats_data()
    stats$total
  })
  
  # --- 1.3 BUBBLE PLOT ---
  animation_data_ready <- reactive({
    req(dashboard_data(), input$time_cus_seg, input$time_granularity)
    
    # Call your helper function
    get_animation_data(
      dashboard_data(), 
      selected_type = input$time_cus_seg, 
      time_metric = input$time_granularity
    )
  })
  
  output$animation_plot <- renderPlotly({
    req(session$clientData$output_animation_plot_width > 0)
    
    df <- animation_data_ready()
    req(nrow(df) > 0, input$time_granularity)
    
    plot<- get_bubble_plot(df, input$time_cus_seg, input$time_granularity)
    
    ggplotly(plot, tooltip="text")%>%
      layout(autosize = TRUE)
  })
  
  # -- 1.4 CRA HEATMAP --
  output$heatmap_segment_ui <- renderUI({
    req(dashboard_data())
    
    data <- dashboard_data()
    
    choices <- data[[input$time_cus_seg]] %>% 
      unique() %>% 
      sort() %>% 
      stats::na.omit()
    
    radioButtons(
      inputId = "heatmap_specific_segment",
      label = paste("Select", humanize_var(input$time_cus_seg), ":"),
      choices = c("All", choices),
      selected = "All"
      )
  })
  
  cohort_data_ready <- reactive({
    req(dashboard_data(), input$time_cus_seg)
    
    segment_val <- if (!is.null(input$heatmap_specific_segment)) {
      input$heatmap_specific_segment
    } else {
      "All"
    }
    
    get_cohort_data(
      dashboard_data(),
      selected_type = input$time_cus_seg,
      segment_value = segment_val
    ) 
  })
  
  output$cra_heatmap <- renderPlotly({
    
    df <- cohort_data_ready()
    req(nrow(df) > 0)
    
    p <- get_cra_heatmap(df, input$time_cus_seg, input$heatmap_specific_segment)
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        autosize = TRUE) %>%
      hide_colorbar()
    
  })
  
  # --- 1.5 SEASONAL TREND ANALYSIS---
  seasonal_ts_ready <- reactive({
    req(dashboard_data(), input$time_cus_seg, input$time_selected_seg)
    
    get_seasonal_data(
      dashboard_data(),
      selected_type = input$time_cus_seg,
      selected_value = input$time_selected_seg
    )
  })
  
  output$cycle_plot <- renderPlot({
    df <- seasonal_ts_ready()
    req(nrow(df) > 0, input$season_metric)
    
    get_cycle_plot(df, input$season_metric) +
      scale_color_manual(values = morandi_colors)
  })
  
  output$stl_plot <- renderPlot({
    df <- seasonal_ts_ready()
    req(nrow(df) > 0, input$stl_metric, input$stl_model_type, input$stl_season,
        input$stl_season_window, input$stl_trend_window)
    
    get_stl(
      data = df,
      metric=input$stl_metric,
      modelType=input$stl_model_type, 
      speriod=input$stl_season, 
      swindow=input$stl_season_window, 
      twindow=input$stl_trend_window
    )
  })

}

#====================================================
# RUN APP
#====================================================
shinyApp(ui = ui, server = server)