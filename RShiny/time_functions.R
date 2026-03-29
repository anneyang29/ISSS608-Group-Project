
humanize_var <- function(x) {
  if (is.na(x) || is.null(x)) return("")
  x %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_squish()
}

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
  
  # If "All" is selected, just give back everything immediately
  if ("All" %in% selected_value) {
    return(data)
  }
  
  # Otherwise, return the filtered result
  return(data %>% filter(.data[[selected_type]] %in% selected_value))
}

#====================================
# Dashboard Functions
#====================================

# get key statistics functions

get_top_month <- function(data){
  
  result <- data %>%
    mutate(month=lubridate::month(date, label=TRUE))%>%
    group_by(month) %>%
    summarise(total_tx = sum(tx_count))%>%
    ungroup %>%
    filter(total_tx == max(total_tx))%>%
    pull(month)
  
  return(result)
}

get_tx_change <- function(data){
  
  monthly_tx <- data %>%
    mutate(month=lubridate::month(date, label=TRUE))%>%
    group_by(month) %>%
    summarise(total_tx = sum(tx_count))%>%
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
        avg_tx_value = sum(tx_volume) / sum(tx_count),
        avg_tx_count = sum(tx_count)/n_distinct(customer_id),
        unique_customers = n_distinct(customer_id),
        .groups = 'drop'         
      ) 
    
  } else if (time_metric=="week"){     
    result <- data  %>%       
      mutate(week = yearweek(date)) %>%       
      group_by(week, .data[[selected_type]]) %>%       
      summarise(         
        avg_tx_value = sum(tx_volume) / sum(tx_count),,         
        avg_tx_count = sum(tx_count) / n_distinct(customer_id),         
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
    scale_size(range = c(1,10), limits = c(1, 25000)) +
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
  
  cohort_counts <- plot_data %>%
    mutate(
      # Ensure both are floors of the month
      cohort_month = lubridate::floor_date(as.Date(first_tx), "month"),
      activity_month = lubridate::floor_date(as.Date(date), "month")
    ) %>%
    # Calculate month difference
    mutate(
      month_number = (lubridate::year(activity_month) - lubridate::year(cohort_month)) * 12 + 
        (lubridate::month(activity_month) - lubridate::month(cohort_month))
    ) %>%
    # Filter out any activity recorded before the cohort month 
    # (Fixes the "Monday shift"from aggregating data to a weekly basis)
    filter(month_number >= 0) %>%
    group_by(cohort_month, month_number) %>%
    summarise(active_users = n_distinct(customer_id), .groups = 'drop')
  
  initial_cohorts <- cohort_counts %>%
    filter(month_number == 0) %>%
    select(cohort_month, initial_users = active_users)
  
  retention_matrix <- cohort_counts %>%
    left_join(initial_cohorts, by = "cohort_month") %>%
    mutate(retention_pct = active_users / initial_users) %>%
    filter(month_number >= 0)
  
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
    summarise(tx_count = sum(tx_count),
              tx_amt = sum(tx_volume),
              user_count = n_distinct(customer_id),
              .groups="drop") %>%
    mutate(!!sym(selected_type) := fct_drop(!!sym(selected_type)),
           date = tsibble::yearweek(date))%>%
    as_tsibble(index = date, 
               key = !!sym(selected_type))
}

# cycle plot
get_cycle_plot <- function (data, selected_type, metric) {
  
  # 2. Process data: Ensure Week of Month is a clean factor
  plot_df <- data %>%
    as_tibble() %>%
    mutate(
      date = as.Date(date),
      month = lubridate::month(date, label = TRUE, abbr = TRUE),
      # Create clean Week Labels
      week_of_month = paste("Week", ceiling(lubridate::day(date) / 7))
    )
  
  # 3. Calculate Average per "Week Type" per Segment
  # This creates the horizontal reference line for each facet
  week_type_avgs <- plot_df %>%
    group_by(week_of_month, !!sym(selected_type)) %>%
    summarise(avg_val = mean(.data[[metric]], na.rm = TRUE), .groups = "drop")
  
  # 4. Build the Plot
  result <- ggplot(plot_df, aes(x = month, y = .data[[metric]], color = !!sym(selected_type))) +
    # The Horizontal Reference Line (Annual average for that specific week number)
    geom_hline(data = week_type_avgs, aes(yintercept = avg_val, color = !!sym(selected_type)), 
               linetype = "dashed", alpha = 0.6) +
    
    # Use geom_line with group = interaction to ensure lines stay WITHIN facets
    geom_line(aes(group = interaction(!!sym(selected_type), week_of_month)), linewidth = 0.8) +
    geom_point(size = 1.2) +
    
    # FACET BY WEEK NUMBER
    # scales = "free_x" is optional, but helps if some months don't have Week 5
    facet_grid(~week_of_month) + 
    
    labs(y = ylab, 
         x = "Month",
         color = humanize_var(selected_type)) +
    theme_grey() +
    theme(
      # Rotate x-axis labels to prevent overlapping
      plot.margin = margin(2, 2, 2, 2, "pt"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      panel.spacing = unit(0.2, "lines"), 
      strip.background = element_rect(fill = "#343a40"),
      strip.text = element_text(face = "bold", color = "white"),
      legend.position = "bottom",
      legend.margin = margin(t = -10),  
      panel.grid.minor = element_blank()
    )
  
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
  
  result <- result +
    theme(
      plot.margin = margin(2, 2, 2, 2, "pt"),
      legend.margin = margin(t = -10)
    )
  return(result)
}


#====================================
# Cashflow module Functions
#====================================

# filter cashflow data

get_liquidity_data <- function(data, location_value, start_date, end_date){
  
  filtered_data <- filter_tx_dates(data, start_date, end_date) %>%
    filter_customer("location", location_value)
    result <- filtered_data %>%
      pivot_wider(names_from = type, 
                  values_from = tx_volume,
                  values_fn = sum) %>%
      replace_na(list(Withdrawal=0, Deposit=0)) %>%
      mutate(week = yearweek(date),
             date=as.Date(week))%>%
      group_by(date) %>%
      summarise(
        Inflow=sum(Deposit),
        Outflow=sum(Withdrawal)
      ) %>%
      mutate(Group = ifelse(Inflow > Outflow, "Inflow Higher", "Outflow Higher"),
             net_liquidity = Inflow-Outflow)
  
  return(result)
}

#create cashflow linegraph
get_cashflow_graph <- function(data){
  ggplot(data, aes(x = date, 
                   # Add group = 1 here so lines/ribbons can connect
                   group = 1,
                   text = paste0("Date: ", date, 
                                 "<br>Inflow: $", scales::comma(Inflow), 
                                 "<br>Outflow: $", scales::comma(Outflow),
                                 "<br>Net: $", scales::comma(net_liquidity)))) +
    # Ensure fill is mapped inside geom_ribbon
    geom_ribbon(aes(ymin = pmin(Inflow, Outflow), 
                    ymax = pmax(Inflow, Outflow), 
                    fill = Group), alpha = 0.3) +
    geom_line(aes(y = Inflow, color = "Inflow"), linewidth = 1) +
    geom_line(aes(y = Outflow, color = "Outflow"), linewidth = 1) +
    scale_color_manual(values = c("Inflow" = "steelblue", "Outflow" = "coral")) +
    scale_fill_manual(values = c("Inflow Higher" = "steelblue", "Outflow Higher" = "coral")) +
    theme_grey() +
    labs(title = "Inflow vs. Outflow Over Time", 
         fill = "Net liquidity", color = "Series", 
         y="Amount ($)",
         x="Date") +
    theme(legend.position = "bottom", 
          legend.direction = "horizontal",
          legend.box = "horizontal")
}


get_liquidity_cust_data <- function(data, location_value, start_date, end_date, selected_type){
  
  filtered_data <- data %>%
    filter_tx_dates(start_date, end_date) %>%
    filter_customer("location", location_value)
  
  result <- filtered_data %>%
    group_by(.data[[selected_type]]) %>%
    summarise(
      Inflow_amt=sum(ifelse(type=='Deposit', tx_volume, 0)),
      Outflow_amt=sum(ifelse(type=='Withdrawal', tx_volume, 0)),
      Inflow_count=sum(ifelse(type=='Deposit', tx_count,0)),
      Outflow_count=sum(ifelse(type=='Withdrawal', tx_count,0)),
      Inflow_users = n_distinct(customer_id[type == 'Deposit']),          
      Outflow_users=n_distinct(customer_id[type == 'Withdrawal'])
    )%>%
    ungroup()
  
  return(result)
}

#create customer breakdown graphs

get_barcharts <- function(data, selected_type, metric, viewtype){
  if (metric=="tx_amt"){
    selected_cols=c('Inflow_amt', 'Outflow_amt')
    remove="_amt"
    y_label <- "Total Amount"
    
  } else if(metric=="tx_count"){
    selected_cols=c('Inflow_count', 'Outflow_count')
    remove="_count"
    y_label <- "Transaction Count"
    
  } else if (metric=="user_count"){
    selected_cols=c('Inflow_users', 'Outflow_users')
    remove="_users"
    y_label <- "Unique Users"
      
  }
  
    plot_data <- data %>%
    select(.data[[selected_type]], all_of(selected_cols)) %>%
    pivot_longer(
      cols = selected_cols, 
      names_to = "Type", 
      values_to = "Amount"
    ) %>%
    mutate(
      Type = str_remove(Type, remove))%>%
    group_by(Type) %>%
    mutate(
      tooltip_label = if(viewtype == "amt") {
        scales::comma(Amount)
      } else {
        scales::percent(Amount / sum(Amount), accuracy = 0.1)
      }
    ) %>%
    ungroup()
  
  plot <- ggplot(plot_data, 
                 aes(x = Type, y = Amount, fill = .data[[selected_type]],
                     text = paste0(humanize_var(selected_type), ": ", .data[[selected_type]],
                                   "<br>Value: ", tooltip_label))) +
    geom_col(position=ifelse(viewtype=="amt", "stack", "fill")) + 
    scale_y_continuous(labels = if(viewtype == "amt") scales::label_number(scale_cut = scales::cut_short_scale()) else scales::label_percent()) +
    scale_fill_brewer(palette = "Set2") +
    theme_grey() +
    labs(
      title = paste0("Inflow vs Outflow Composition by ", selected_type),
      x = "Cashflow",
      y = ifelse(viewtype == "amt", y_label, "Percentage Contribution"),
      fill = selected_type
    )+
    theme(legend.position = "none")
  
  return(plot)
}
