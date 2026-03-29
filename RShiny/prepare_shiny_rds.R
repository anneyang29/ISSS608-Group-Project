library(arrow)

build_base_data_from_csv <- function(customer_csv_path, tx_csv_path) {
  customer_raw <- readr::read_csv(customer_csv_path, show_col_types = FALSE) %>%
    dplyr::mutate(customer_id = as.numeric(customer_id))
  
  tx_clean <- readr::read_csv(tx_csv_path, show_col_types = FALSE) %>%
    dplyr::mutate(
      customer_id = as.numeric(customer_id),
      date = as.Date(date),
      amount = as.numeric(amount),
      type = as.factor(type)
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
      customer_segment = factor(customer_segment, 
                                levels = c("inactive", "occasional", "regular", "power"), 
                                ordered = TRUE),
      clv_segment = factor(clv_segment, 
                           levels = c("Bronze", "Silver", "Gold", "Platinum"), 
                           ordered = TRUE),
      income_bracket = factor(income_bracket, 
                              levels = c("Low", "Medium", "High", "Very High"), 
                              ordered = TRUE),
      gender = as.factor(gender),
      acquisition_channel = as.factor(acquisition_channel),
      location = as.factor(location)
    ) %>%
    select(customer_id, gender, acquisition_channel, customer_segment,
           location, clv_segment, income_bracket)
  
  weekly_tx <- tx_clean%>%
    mutate(date = floor_date(date, unit = "week", week_start = 1)) %>%
    # Group by the customer, the week, and the transaction type
    group_by(customer_id, date, type) %>%
    # Calculate metrics for that specific type in that specific week
    summarise(
      tx_count = n(),
      tx_volume = sum(amount, na.rm = TRUE),
      .groups = "drop"
    ) 
  
  time_data <- weekly_tx %>%
    dplyr::inner_join(cus_clean, by = "customer_id")
  
  forecast_data <- tx_clean %>%
    left_join(cus_clean, by="customer_id") %>%
    pivot_wider(names_from = type, 
                values_from = amount,
                values_fn = sum) %>%
    replace_na(list(Withdrawal=0, Deposit=0)) %>%
    group_by(date) %>%
    summarise(
      Inflow=sum(Deposit),
      Outflow=sum(Withdrawal)
    ) %>%
    mutate(net_liquidity = Inflow-Outflow)
  
  list(
    base_data = base_data,
    time_data = time_data,
    forecast_data=forecast_data
  )
}


find_existing_path <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) > 0) {
    normalizePath(existing[1], winslash = "/", mustWork = TRUE)
  } else {
    NA_character_
  }
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

customer_path <- find_existing_path(customer_csv_candidates)
tx_path <- find_existing_path(tx_csv_candidates)

data_bundle <- build_base_data_from_csv(customer_path, tx_path)

saveRDS(
  list(base_data = data_bundle$base_data),
  "RShiny/data/shiny_base_data_light.rds"
)

write_parquet(data_bundle$time_data, 
              "RShiny/data/shiny_time_data.parquet")

saveRDS(data_bundle$forecast_data,
  "RShiny/data/shiny_forecast_data.rds"
)

cat("Done: created shiny_base_data_light.rds, shiny_time_data.rds, shiny_forecast_data.rd\n")



