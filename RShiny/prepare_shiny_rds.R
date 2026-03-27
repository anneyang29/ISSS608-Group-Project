library(tidyverse)

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

data_bundle <- build_base_data_from_csv(customer_csv_candidates, tx_csv_candidates)

saveRDS(
  list(base_data = data_bundle$base_data),
  "RShiny/data/shiny_base_data_light.rds"
)

saveRDS(
  list(time_data = data_bundle$time_data),
  "RShiny/data/shiny_time_data.rds"
)

cat("Done: created shiny_base_data_light.rds and shiny_time_data.rds\n")

