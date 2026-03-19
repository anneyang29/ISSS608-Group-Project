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
base_data <- readRDS("../Proposal/data/shiny_base_data.rds")

# Global Variables
cluster_vars <- c("monthly_transaction_count", "average_transaction_value", 
                  "total_transaction_volume", "transaction_frequency", 
                  "avg_daily_transactions", "app_logins_frequency", 
                  "feature_usage_diversity", "active_products", 
                  "weekend_transaction_ratio", "customer_tenure")

demo_vars <- c("gender", "income_bracket", "education_level", 
               "marital_status", "investment_account", "insurance_product",
               "acquisition_channel", "customer_segment", "savings_account",
               "credit_card", "personal_loan", "bill_payment_user", "auto_savings_enabled")

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
    menuItem("Clustering Analysis", tabName = "Clustering", icon = icon("users")),
    menuItem("EDA (Member 2)", tabName = "EDA", icon = icon("chart-bar")),
    menuItem("Geospatial (Member 3)", tabName = "Geo", icon = icon("globe"))
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

body <- dashboardBody(
  use_theme(mytheme),
  tabItems(
    tabItem(tabName = "Clustering", ClusterSubTabs),
    tabItem(tabName = "EDA", h2("EDA goes here")),
    tabItem(tabName = "Geo", h2("Geo goes here"))
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
}

#====================================================
# RUN APP
#====================================================
shinyApp(ui = ui, server = server)