
# Filtering data set functions

filter_tx_dates<- function(data, start_date, end_date){
  result <- data %>%
    filter(date>=ymd(start_date),
           date<=ymd(end_date))
  
  return(result)
}

filter_tx <- function(data, selected_type='All'){
  if(selected_type!='All'){
    result <- data %>%
      filter(type==selected_type)
    return(result)
  }
  else {
    return(data)
  }
}

filter_customer <- function(data, selected_type, selected_value){
  if (is.null(data)) return(NULL)
  
  data %>% filter(.data[[selected_type]] %in% selected_value)
}

#====================================
# Dashboard Functions
#====================================

# get key statistics functions

get_top_month <- function(data){
  
  result <- data %>%
    mutate(month=lubridate::month(date, label=TRUE))%>%
    group_by(month) %>%
    summarise(total_tx = n())%>%
    ungroup %>%
    filter(total_tx == max(total_tx))%>%
    pull(month)
  
  return(result)
}

get_tx_change <- function(data){
  
  monthly_tx <- data %>%
    mutate(month=lubridate::month(date, label=TRUE))%>%
    group_by(month) %>%
    summarise(total_tx = n())%>%
    ungroup 
  
  if (nrow(monthly_tx)==1) {
    result = "N/A"
  } else {
    result <-  monthly_tx %>%
      mutate(
        prev_tx = lag(total_tx),
        pct_change = (total_tx - prev_tx) / prev_tx
      ) %>%
      slice_tail(n = 1) %>%
      pull(pct_change) *100
  }
  
  return(result)
}

get_new_cust <- function(data, end_date){
  
  latest_month <- lubridate::month(ymd(end_date))
  
  result <- data %>%
    filter(first_tx <= ymd(end_date)) %>%
    mutate(month = lubridate::month(first_tx)) %>%
    group_by(month) %>%
    summarise(new_users=n_distinct(customer_id))%>%
    ungroup()%>%
    filter(month == latest_month)%>%
    pull(new_users)
  
  return(result)
}

get_total_cust <- function(end_date, data){
  n_distinct(data$customer_id)
}


# Bubble Chart 

get_animation_data <- function(data,  selected_type, time_metric){   
  if (time_metric=="month"){     
    result <- data %>%       
      mutate(month = lubridate::month(date, label=TRUE)) %>%       
      group_by(month, .data[[selected_type]]) %>%       
      summarise(         
        avg_tx_value = mean(amount),
        avg_tx_count = n()/n_distinct(customer_id),
        unique_customers = n_distinct(customer_id),
        .groups = 'drop'         
      ) 
    
  } else if (time_metric=="week"){     
    result <- data  %>%       
      mutate(week = yearweek(date)) %>%       
      group_by(week, .data[[selected_type]]) %>%       
      summarise(         
        avg_tx_value = mean(amount),         
        avg_tx_count = n()/n_distinct(customer_id),         
        unique_customers = n_distinct(customer_id),         
        .groups = 'drop'         
      )   
  }    
  return(result) 
}

get_bubble_plot <- function(data, selected_type, time_metric){
  
  if (time_metric=="week"){
    data<- data %>% 
      mutate(frame_id=as.character(week))
  } else if (time_metric=="month"){
    data <- data %>%
      mutate(frame_id=month)
  }
  
  result <- ggplot(data, 
                   aes(x = avg_tx_value,
                       y = avg_tx_count,
                       size = unique_customers,
                       text = paste(!!sym(selected_type), 
                                    "<br>Number of customers:" , unique_customers,
                                    "<br>Average transaction value:",
                                    prettyNum(avg_tx_value,
                                              big.mark = ",",
                                              scientific = FALSE),
                                    "<br>Average transaction count:",
                                    round(avg_tx_count,2)
                       ))) +     
    geom_point(aes(size = unique_customers,
                   color = !!sym(selected_type),
                   ids = !!sym(selected_type),
                   frame = frame_id
    ),
    alpha = 0.7, show.legend = FALSE) +     
    theme_grey() + 
    scale_size(range = c(1,10), limits = c(1, 20000)) +
    labs(       
         x = "Average User Transaction Value",          
         y = "Average User Transaction Counts") 
  
  return(result) }

# Cohort retention heatmap
get_cohort_data <- function(data, selected_type, segment_value){
  if (segment_value != 'All'){
    plot_data <- data %>%
      filter(.data[[selected_type]] == segment_value)
  }
  else{
    plot_data <- data
  }
  
  cohort_counts  <- plot_data %>%
    mutate(
      cohort_month = lubridate::floor_date(first_tx, "month"),
      activity_month = lubridate::floor_date(as.Date(date), "month"),
      month_number = (lubridate::year(activity_month) - lubridate::year(cohort_month)) * 12 + 
        (lubridate::month(activity_month) - lubridate::month(cohort_month))
    ) %>%
    group_by(cohort_month, month_number) %>%
    summarise(active_users = n_distinct(customer_id), .groups = 'drop')
  
  initial_cohorts <- cohort_counts %>%
    filter(month_number == 0) %>%
    select(cohort_month, initial_users = active_users)
  
  retention_matrix <- cohort_counts %>%
    left_join(initial_cohorts, by = "cohort_month") %>%
    mutate(retention_pct = active_users / initial_users)
  
  return(retention_matrix)
}

get_cra_heatmap <- function(cra_data, selected_type, segment_value) {
  
  result <- ggplot(cra_data, 
                   aes(x = month_number, 
                       y = factor(cohort_month), 
                       fill = retention_pct,
                       text = paste0("Months since acquistion: ", 
                                     month_number, "<br>Retention rate: ", 
                                     round(retention_pct,2)*100, "%"))) +
    geom_tile() +
    scale_fill_gradient2(low = "firebrick",
                         mid = "white",
                         high = "steelblue",
                         midpoint = 0.5,
                         labels = scales::percent,
                         limits = c(0, 1),
                         guide = guide_colorbar(
                           direction = "horizontal", 
                           title.position = "top",   # Puts the title 'Retention Rate' above the bar
                           barwidth = 15             # Makes the bar a bit wider
                         )) +
    theme_grey() +
    theme(
      legend.position = "none"    
    ) +
    labs(subtitle = "Each cell represents the percentage of customers who made at \nleast one transaction in the months following their first transaction",
         x = "Months Since Acquisition",
         y = "Acquisition Cohort",
         fill = "Retention Rate") 

  return(result)

}

# seasonal data 
get_seasonal_data <- function(data, selected_type, selected_value){
  result <- data %>%
    group_by(date, !!sym(selected_type)) %>%
    summarise(tx_count = n(),
              tx_amt = sum(amount),
              user_count = n_distinct(customer_id),
              .groups="drop") %>%
    mutate(!!sym(selected_type) := fct_drop(!!sym(selected_type)))%>%
    as_tsibble(index = date, 
               key = !!sym(selected_type))
}

# cycle plot
get_cycle_plot <- function (data, metric){
  if (metric == "tx_count"){
    titleText = "Transaction Count Cycle Plot"
    ylab = "Total Count of Transactions"
  } else if (metric == "tx_amt") {
    ylab = "Total Transaction Amount"
    titleText = "Transaction Amount Cycle Plot"
  } else if (metric == "user_count") {
    ylab = "Total Unique Customers"
    titleText = "User Count Cycle Plot"
    
  }
  
  result <- data %>% 
    gg_subseries(.data[[metric]], period="1 week") +
    labs(title=titleText,
         y=ylab, x='Date') +
    theme_grey()+
    theme(axis.text.x = element_blank(),
          axis.ticks.x=element_blank())
  
  return(result)
}

# stl plot
get_stl <- function(data, metric, modelType, speriod, swindow=7, twindow=11) {
  if (modelType == "STLM") {
    result <- data %>%
      model(STLM = STL(!!sym(metric) ~ season(period = speriod, window = swindow) + trend(window = twindow), robust = TRUE)) %>%
      components() %>%
      autoplot() +
      theme_grey() +
      labs(title = "STL Decomposition")
    
  } else if (modelType == "Classical (add)") {
    result <- data %>%
      model(`Classical (add.)` = classical_decomposition(!!sym(metric) ~ season(period = speriod), type = "additive")) %>%
      components() %>%
      autoplot() +
      theme_grey() +
      labs(title = "Classical Decomposition (Additive)")
    
  } else if (modelType == "Classical (mult)") {
    result <- data %>%
      model(`Classical (mult.)` = classical_decomposition(!!sym(metric) ~ season(period = speriod), type = "multiplicative")) %>%
      components() %>%
      autoplot() +
      theme_grey() +
      labs(title = "Classical Decomposition (Multiplicative)")
  }
  
  return(result)
}