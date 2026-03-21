#====================================================
# ISSS608 Group Project - Main Dashboard Application
#====================================================

# load R packages
#====================================================
pacman::p_load(shiny, shinydashboard, shinycssloaders, 
               tidyverse, dplyr, readr, ggcorrplot, parallelPlot, 
               cluster, mclust, fresh)

#Import data 
#====================================================
find_existing_path <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) > 0) normalizePath(existing[1], winslash = "/", mustWork = TRUE) else NA_character_
}

rds_candidates <- c(
  file.path("RShiny", "data", "shiny_base_data.rds"),
  file.path("data", "shiny_base_data.rds"),
  file.path("..", "RShiny", "data", "shiny_base_data.rds"),
  file.path("..", "data", "shiny_base_data.rds")
)

base_data_path <- find_existing_path(rds_candidates)

build_base_data_from_csv <- function(customer_csv_path, tx_csv_path) {
  customer_raw <- readr::read_csv(customer_csv_path, show_col_types = FALSE) %>%
    mutate(customer_id = as.numeric(customer_id))

  tx_clean <- readr::read_csv(tx_csv_path, show_col_types = FALSE) %>%
    mutate(
      customer_id = as.numeric(customer_id),
      date = as.Date(date),
      amount = as.numeric(amount),
      type = as.character(type)
    ) %>%
    filter(!is.na(customer_id), !is.na(date), !is.na(amount), amount > 0)

  if (nrow(tx_clean) == 0) {
    stop("transactions_data.csv has no valid rows after cleaning.", call. = FALSE)
  }

  snapshot_date <- max(tx_clean$date, na.rm = TRUE)

  tx_features <- tx_clean %>%
    group_by(customer_id) %>%
    summarise(
      monthly_transaction_count = n() / pmax(n_distinct(format(date, "%Y-%m")), 1),
      average_transaction_value = mean(amount, na.rm = TRUE),
      total_transaction_volume = sum(amount, na.rm = TRUE),
      transaction_frequency = n() / pmax(as.numeric(max(date) - min(date)) + 1, 1),
      avg_daily_transactions = n() / pmax(n_distinct(date), 1),
      weekend_transaction_ratio = mean(weekdays(date) %in% c("Saturday", "Sunday"), na.rm = TRUE),
      feature_usage_diversity = n_distinct(type),
      customer_tenure = as.numeric(snapshot_date - min(date)) + 1,
      .groups = "drop"
    )

  # Avoid duplicated names like foo.x/foo.y when customer_raw already contains
  # similarly named engineered columns. We prefer the transaction-engineered versions.
  overlap <- intersect(setdiff(names(tx_features), "customer_id"), names(customer_raw))
  base_data <- customer_raw %>%
    select(-any_of(overlap)) %>%
    left_join(tx_features, by = "customer_id")

  product_flag_cols <- intersect(
    c(
      "savings_account", "credit_card", "personal_loan", "investment_account", "insurance_product",
      "bill_payment_user", "auto_savings_enabled"
    ),
    names(base_data)
  )

  if (length(product_flag_cols) > 0) {
    base_data <- base_data %>%
      mutate(
        active_products = rowSums(
          across(
            all_of(product_flag_cols),
            ~ as.numeric(tolower(as.character(.x)) %in% c("1", "true", "yes", "y", "active"))
          ),
          na.rm = TRUE
        )
      )
  }

  if ("app_logins_frequency" %in% names(base_data)) {
    base_data <- base_data %>% mutate(app_logins_frequency = as.numeric(app_logins_frequency))
  }

  base_data
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

if (!is.na(base_data_path)) {
  base_data <- readRDS(base_data_path)
} else {
  customer_csv_path <- find_existing_path(customer_csv_candidates)
  tx_csv_path <- find_existing_path(tx_csv_candidates)

  if (is.na(customer_csv_path) || is.na(tx_csv_path)) {
    stop(
      paste0(
        "Unable to find shiny_base_data.rds AND required CSV files. ",
        "RDS paths tried: ", paste(rds_candidates, collapse = ", "), ". ",
        "Customer CSV paths tried: ", paste(customer_csv_candidates, collapse = ", "), "; ",
        "transactions CSV paths tried: ", paste(tx_csv_candidates, collapse = ", "), "."
      ),
      call. = FALSE
    )
  }

  base_data <- build_base_data_from_csv(customer_csv_path, tx_csv_path)

  output_rds_candidates <- c(
    file.path("RShiny", "data", "shiny_base_data.rds"),
    file.path("data", "shiny_base_data.rds")
  )

  output_path <- output_rds_candidates[dir.exists(dirname(output_rds_candidates))][1]

  if (!is.na(output_path)) {
    tryCatch(
      {
        saveRDS(base_data, output_path)
      },
      error = function(e) {
        message("Could not save shiny_base_data.rds: ", conditionMessage(e))
      }
    )
  }
}


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

sidebar <- dashboardSidebar(
  sidebarMenu(
    width = 100,
    selected = "Clustering",
    menuItem(
      "Home",
      icon = icon("home"),
      href = "https://anneyang29.github.io/ISSS608-Group-Project/",
      newtab = TRUE
    ),
    menuItem("Clustering Analysis", tabName = "Clustering", icon = icon("users")),
    menuItem("EDA (Member 2)", tabName = "EDA", icon = icon("chart-bar")),
    menuItem("Confirmatory Analysis", tabName = "Confirmatory", icon = icon("chart-line"))
  )
)

mytheme <- create_theme(
  adminlte_color(light_blue = "#D4C5B9"),
  adminlte_sidebar(width = "250px", dark_bg = "#2E3440", dark_hover_bg = "#D4C5B9", dark_color = "#FFFFFF"),
  adminlte_global(content_bg = "#F8F9FA", box_bg = "#FFFFFF", info_box_bg = "#FFFFFF")
)

#====================================================
# CLUSTERING UI - START
#====================================================

# STEP 1: Correlation 
ClusterRow1 <- fluidRow(
  column(3,
         box(title = "Filter Variables", status = "info", solidHeader = FALSE, width = NULL,
             selectizeInput("corr_vars", "Select Variables:", 
                            choices = cluster_vars, multiple = TRUE, 
                            selected = cluster_vars,
                            options = list(plugins = list("remove_button"))), 
             uiOutput("corr_warning")
         )
  ),
  column(9,
         box(title = "Correlation Matrix", status = "primary", solidHeader = TRUE, width = NULL, align = "center",
             withSpinner(plotOutput("corrPlot", height = "600px")))
  )
)

# STEP 2: Method Selection & Evaluation
ClusterRow2 <- fluidRow(
  column(3,
         box(title = "Clustering Method", status = "info", solidHeader = FALSE, width = NULL,
             radioButtons("clust_method", "Select Algorithm:", 
                          choices = c("K-means", "PAM (CLARA)", "GMM"), 
                          selected = "K-means")
         )
  ),
  column(9,
         box(title = "Model Evaluation (Optimal k)", status = "primary", solidHeader = TRUE, width = NULL,
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
         box(title = "Number of k :", status = "info", solidHeader = FALSE, width = NULL,
             selectInput("k_val", "Number of k :", choices = 2:10, selected = 3)
         )
  ),
  column(9,
         box(title = "STEP3: Interpretation", status = "primary", solidHeader = TRUE, width = NULL, align = "center",
             withSpinner(plotOutput("sizePlot", height = "500px")))
  )
)

# STEP 4: Interpretation (Parallel & Demographics)
ClusterRow4 <- fluidRow(
  column(3,
         box(title = "Interpretation Settings", status = "info", solidHeader = FALSE, width = NULL,
             selectInput("demo_var", "Demographic Variable:", choices = demo_vars)
         )
  ),
  column(9,
         box(title = "Behavioural Contrast", status = "primary", solidHeader = TRUE, width = NULL,
             withSpinner(parallelPlotOutput("pPlot", height = "450px"))),
         
         box(title = "Demographic Composition", status = "primary", solidHeader = TRUE, width = NULL, align = "center",
             withSpinner(plotOutput("demoPlot", height = "400px")))
  )
)

# ClusterSubTabs 
ClusterSubTabs <- tabsetPanel(
  tabPanel("STEP 1: Correlation", ClusterRow1),
  tabPanel("STEP 2: Method Select", ClusterRow2),
  tabPanel("STEP 3: Cluster Size", ClusterRow3),
  tabPanel("STEP 4: Interpretation", ClusterRow4)
)

ConfirmRow1 <- fluidRow(
  column(3,
         box(title = "H1 Settings", status = "info", solidHeader = FALSE, width = NULL,
             selectInput("h1_outcome", "Outcome (numeric):", choices = confirm_numeric_vars, selected = h1_outcome_default),
             selectInput("h1_predictor", "Predictor (numeric):", choices = confirm_numeric_vars, selected = h1_predictor_default),
             uiOutput("h1_input_warning")
         )
  ),
  column(9,
         box(title = "Regression Results", status = "primary", solidHeader = TRUE, width = NULL,
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
         box(
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
         box(title = "H2 Settings", status = "info", solidHeader = FALSE, width = NULL,
             selectInput("h2_group", "Group variable:", choices = confirm_categorical_vars, selected = h2_group_default),
             selectInput("h2_value", "Value (numeric):", choices = confirm_numeric_vars, selected = h2_value_default),
             selectInput(
               "h2_test_type",
               "Test type:",
               choices = c(
                 "Auto (recommended)" = "auto",
                 "Parametric (ANOVA/Welch)" = "parametric",
                 "Non-parametric (Kruskal-Wallis)" = "nonparametric"
               ),
               selected = "auto"
             ),
             selectInput(
               "h2_pairwise_display",
               "Pairwise display:",
               choices = c(
                 "Significant only" = "significant",
                 "All" = "all"
               ),
               selected = "significant"
             ),
             selectInput(
               "h2_p_adjust",
               "P-value adjustment method:",
               choices = p.adjust.methods,
               selected = "BH"
             )
         )
  ),
  column(9,
         box(title = "ANOVA Results (or Kruskal-Wallis)", status = "primary", solidHeader = TRUE, width = NULL,
             uiOutput("h2_sentence"),
             tableOutput("h2_summary"),
             hr(),
             withSpinner(plotOutput("h2_plot", height = "420px")),
             uiOutput("h2_posthoc_ui")
         ),
         box(
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

ConfirmSubTabs <- tabsetPanel(
  tabPanel("Regression", ConfirmRow1),
  tabPanel("ANOVA", ConfirmRow2)
)

body <- dashboardBody(
  use_theme(mytheme),
  tabItems(
    tabItem(tabName = "Clustering", ClusterSubTabs),
    tabItem(tabName = "EDA", h2("EDA goes here")),
    tabItem(tabName = "Confirmatory", ConfirmSubTabs)
  ),
  tags$div(
    style = "margin: 10px; padding: 10px 12px; background: #F8F9FA; border-left: 4px solid #D4C5B9;",
    HTML(
      paste0(
        "<b>Data note:</b> This dashboard uses the COFINFAD customer + transaction data provided for the project. ",
        "Several behavioural metrics (e.g., transaction frequency/volume features) are engineered from the raw transactions and stored in a prepared dataset (<i>shiny_base_data.rds</i>) for faster loading. ",
        "If the RDS is not found, the app will build it from the CSVs once. ",
        "The app does not generate new labels such as churn probability; it analyses the fields provided in the dataset."
      )
    )
  )
)

ui <- dashboardPage(title = 'Colombian Fintech', header, sidebar, body)    

#====================================================
# SERVER - START
#====================================================
server <- function(input, output, session) { 
  
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
  
  # --- STEP 2: Method Evaluation Logic ---
  output$eval_plots_ui <- renderUI({
    if (input$clust_method == "K-means") {
      fluidRow(
        column(6, withSpinner(plotOutput("elbowPlot", height = "400px"))),
        column(6, withSpinner(plotOutput("ratioPlot", height = "400px")))
      )
    } else if (input$clust_method == "PAM (CLARA)") {
      withSpinner(plotOutput("silPlot", height = "400px"))
    } else {
      withSpinner(plotOutput("bicPlot", height = "400px"))
    }
  })
  
  output$eval_text <- renderUI({
    if(input$clust_method == "K-means") {
      HTML("<p><b>Elbow (WSS):</b> The “elbow” point represents a balance between model simplicity and cluster compactness. After this point, increasing the number of clusters yields diminishing returns in terms of improved cluster cohesion.</p>
            <p><b>BetweenSS / TotalSS ratio:</b> This ratio measures how much of the total variance in the data is explained by the clustering structure. As k increases, the ratio typically increases but eventually stabilises. The optimal k is often found where the improvement becomes marginal.</p>")
    } else if(input$clust_method == "PAM (CLARA)") {
      HTML("<p><b>CLARA Score:</b> For each observation, the silhouette value ranges from −1 to 1. Values close to 1 indicate well-separated clusters. The optimal number of clusters is typically the value of k that maximises the average silhouette score.</p>")
    } else {
      HTML("<p><b>GMM BIC Plot:</b> Unlike K-means and PAM, GMM does not assume spherical clusters. Model selection is based on the Bayesian Information Criterion (BIC). The model with the highest BIC (or clear peak) is generally selected as the optimal model.</p>")
    }
  })
  
  kmeans_metrics <- reactive({
    req(input$corr_vars, input$clust_method == "K-means")
    X <- base_data %>% select(all_of(input$corr_vars)) %>% scale()
    purrr::map_dfr(2:15, function(k) {
      set.seed(2022)
      km <- kmeans(X, centers = k, nstart = 25)
      tibble(k = k, tot_withinss = km$tot.withinss, between_ratio = km$betweenss / km$totss)
    })
  })
  
  output$elbowPlot <- renderPlot({
    df <- kmeans_metrics()
    if(is.null(df)) return(NULL)
    ggplot(df, aes(k, tot_withinss)) + geom_line(color = "#7B8B97", linewidth = 1) + geom_point(color = "#C3A6A0", size = 3) +
      labs(title = "Elbow Method", x = "Number of Clusters (k)", y = "Total Within-Cluster Sum of Squares") + theme_minimal()
  })
  
  output$ratioPlot <- renderPlot({
    df <- kmeans_metrics()
    if(is.null(df)) return(NULL)
    ggplot(df, aes(k, between_ratio)) + geom_line(color = "#7B8B97", linewidth = 1) + geom_point(color = "#A8B7AB", size = 3) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(title = "BetweenSS / TotalSS Ratio", x = "Number of Clusters (k)", y = "BetweenSS / TotalSS") + theme_minimal()
  })
  
  output$silPlot <- renderPlot({
    req(input$corr_vars, input$clust_method == "PAM (CLARA)")
    X <- base_data %>% select(all_of(input$corr_vars)) %>% scale()
    clara_res <- purrr::map_dfr(2:15, function(k) {
      cl <- cluster::clara(X, k = k, samples = 5)
      tibble(k = k, avg_silhouette = cl$silinfo$avg.width)
    })
    ggplot(clara_res, aes(k, avg_silhouette)) + 
      geom_line(color = "#7B8B97", linewidth = 1) + geom_point(color = "#C3A6A0", size = 3) +
      labs(title = "CLARA Score", x = "Number of Clusters (k)", y = "Average Silhouette") + theme_minimal()
  })
  
  output$bicPlot <- renderPlot({
    req(input$corr_vars, input$clust_method == "GMM")
    X <- base_data %>% select(all_of(input$corr_vars)) %>% scale()
    gmm_fit <- mclust::Mclust(X, G = 2:15)
    par(xpd = TRUE, mar = c(5, 4, 4, 13))  
    plot(gmm_fit, what = "BIC", legendArgs = list(x = "right", inset = c(-0.6, 0), cex = 0.8))
    par(xpd = FALSE)
  })
  
  # --- STEP 3 & 4: Final Clustering & Profiling ---
  dynamic_clusters <- reactive({
    req(input$corr_vars, input$k_val, input$clust_method)
    X <- base_data %>% select(all_of(input$corr_vars)) %>% mutate(across(everything(), as.numeric)) %>% scale()
    k <- as.numeric(input$k_val)
    
    if (input$clust_method == "K-means") {
      set.seed(2022)
      km <- kmeans(X, centers = k, nstart = 25)
      clusters <- factor(km$cluster)
    } else if (input$clust_method == "PAM (CLARA)") { 
      cl <- cluster::clara(X, k = k, samples = 5)
      clusters <- factor(cl$clustering)
    } else {
      gmm <- mclust::Mclust(X, G = k)
      clusters <- factor(gmm$classification)
    }
    
    base_data %>% mutate(cluster = clusters)
  })
  
  output$sizePlot <- renderPlot({
    clustered_data <- dynamic_clusters()
    if(is.null(clustered_data)) return(NULL)
    
    color_palette <- colorRampPalette(morandi_colors)(length(unique(clustered_data$cluster)))
    
    ggplot(clustered_data, aes(x = cluster, fill = cluster)) +
      geom_bar(aes(y = after_stat(count)/sum(after_stat(count)))) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = color_palette) + 
      labs(title = paste("Cluster Size Distribution -", input$clust_method),
           x = "Cluster", y = "Proportion of Customers") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  output$pPlot <- renderParallelPlot({
    clustered_data <- dynamic_clusters()
    if(is.null(clustered_data)) return(NULL)
    
    pp_data <- clustered_data %>%
      dplyr::filter(!is.na(cluster)) %>%
      dplyr::select(cluster, dplyr::all_of(input$corr_vars)) 
    
    histoVisibility <- names(pp_data)
    
    parallelPlot::parallelPlot(
      data = pp_data,
      refColumnDim = "cluster",
      rotateTitle = TRUE,
      histoVisibility = histoVisibility
    )
  })
  
  output$demoPlot <- renderPlot({
    req(input$demo_var)
    clustered_data <- dynamic_clusters()
    if(is.null(clustered_data)) return(NULL)

    plot_data <- clustered_data %>% filter(!is.na(.data[[input$demo_var]]))
    
    num_categories <- length(unique(plot_data[[input$demo_var]]))
    color_palette <- colorRampPalette(morandi_colors)(num_categories)
    
    ggplot(plot_data, aes(x = cluster, fill = .data[[input$demo_var]])) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = color_palette) + 
      labs(title = paste("Demographic Distribution by", input$demo_var),
           x = "Cluster", y = "Proportion", fill = input$demo_var) +
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
}

#====================================================
# RUN APP
#====================================================
shinyApp(ui = ui, server = server)