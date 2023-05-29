
# Wczytanie ramek

setwd("C:/Airline/DataSets")

airports <- read.csv("airports.csv")
carriers <- read.csv("carriers.csv")
plane_data <- read.csv("plane-data.csv")
variable_descriptions <- read.csv("variable-descriptions.csv")


library(dplyr)

library(tidyverse)


write_dataframes_to_csv <- function(dataframes, file_prefix) {
  for (i in seq_along(dataframes)) {
    filename <- paste0(file_prefix, "/", file_prefix, "_", i + 1986, ".csv")
    write.csv(dataframes[[i]], file = filename, row.names = FALSE)
  }
}

write_month_dataframes_to_csv <- function(dataframes, file_prefix) {
  for (i in seq_along(dataframes)) {
    filename <- paste0(file_prefix, "/", file_prefix, "_", i , ".csv")
    write.csv(dataframes[[i]], file = filename, row.names = FALSE)
  }
}

read_dataframes_from_csv <- function(file_prefix) {
  dataframes <- list()
  for (i in 1:22) {
    filename <- paste0(file_prefix, "/", file_prefix, "_", i + 1986, ".csv")
    df <- read.csv(filename)
    dataframes[[i]] <- df
  }
  return(dataframes)
}

read_flight_dataframes_from_csv <- function(directory_name) {
  dataframes <- list()
  for (i in 1:22) {
    filename <- paste0(directory_name, "/", i + 1986, ".csv")
    df <- read.csv(filename)
    dataframes[[i]] <- df
  }
  return(dataframes)
}

read_month_dataframes_from_csv <- function(directory_name) {
  dataframes <- list()
  for (i in 1:12) {
    filename <- paste0(directory_name, "/", i , ".csv")
    df <- read.csv(filename)
    dataframes[[i]] <- df
  }
  return(dataframes)
}

ramki <- read_flight_dataframes_from_csv("flights")

############################

#Średni czas opóźnienia każdej firmy w każdym roku

mean_of_delay_time_for_each_year <- function(ramki){

  mean_of_delay_time <- function(data){
    
    data <- data %>%
      filter(!(is.na(ArrDelay))) %>%
      filter(ArrDelay > 0)
    
    sum_of_delay_times <- data %>%
      group_by(UniqueCarrier) %>%
      summarise(DelayedFlights = n(), 
                DelayTime = sum(ArrDelay),
                ) %>%
      mutate(Mean = DelayTime / DelayedFlights)
    
  }
  
  mean_of_delay_time_for_each_year <- lapply(ramki, mean_of_delay_time)
  return(mean_of_delay_time_for_each_year)
  
}

result_mean_of_delay_time_for_each_year <- mean_of_delay_time_for_each_year(ramki)

write_dataframes_to_csv(result_mean_of_delay_time_for_each_year, "mean_of_delay_time_for_each_year")

result_mean_of_delay_time_for_each_year <- read_dataframes_from_csv("mean_of_delay_time_for_each_year")

############################

# Średni czas opóźnienia każdej firmy łącznie

summarised_mean_of_delay_time <- function(ramki){
  
  mean_of_delay_time <- function(data){
    data <- data %>%
      filter(!(is.na(ArrDelay))) %>%
      filter(ArrDelay > 0)
    
    result <- data %>%
      group_by(UniqueCarrier) %>%
      mutate(
        SummarisedTime = sum(ArrDelay),
        NumberOfDelayedFlights = n()
      ) %>%
      distinct(UniqueCarrier, .keep_all = TRUE)
    return(result)
    
  }
  
  mean_of_delay_time_for_every_year <- lapply(ramki, mean_of_delay_time)

  
  merged_mean <- function(list){
    merged_data <- bind_rows(list) %>%
      group_by(UniqueCarrier) %>%
      summarise(across(c(SummarisedTime, NumberOfDelayedFlights), list(sum = sum))) %>%
      mutate(Mean = SummarisedTime_sum / NumberOfDelayedFlights_sum)
    
    return(merged_data[c("UniqueCarrier", "Mean")])
  }
  
  result <- merged_mean(mean_of_delay_time_for_every_year)
}

result_summarised_mean_of_delay_time <- summarised_mean_of_delay_time(ramki)

write.csv(result_summarised_mean_of_delay_time, "summarised_mean_of_delay_time.csv")

result_summarised_mean_of_delay_time <- read.csv("summarised_mean_of_delay_time.csv")

#########################

# Jaka część wszystkich lotów była przeprowadzona przez danego przewoźnika w danym roku? Ile lotów wykonał dany przewoźnik w danym roku?

number_and_fraction_of_flights_of_each_carrier_for_each_year <- function(ramki){
  fraction_of_each_carrier <- function(data){
    data_2 <- data %>%
      group_by(UniqueCarrier) %>%
      summarise(Sum  = n()) %>%
      mutate(Fraction = Sum / nrow(data))
    return(data_2)
  }
  
  fraction_of_each_carrier_for_each_year <- lapply(ramki, fraction_of_each_carrier)
  return(fraction_of_each_carrier_for_each_year)
  
}

result_number_and_fraction_of_flights_of_each_carrier_for_each_year <- number_and_fraction_of_flights_of_each_carrier_for_each_year(ramki)

write_dataframes_to_csv(result_number_and_fraction_of_flights_of_each_carrier_for_each_year, "number_and_fraction_of_flights_of_each_carrier_for_each_year")

result_number_and_fraction_of_flights_of_each_carrier_for_each_year <- read_dataframes_from_csv("number_and_fraction_of_flights_of_each_carrier_for_each_year")

################################

# Ile lotów wykonał każdy przewoźnik przez wszytkie lata? Jaka to część wszystkich lotów?

summarised_number_and_fraction_of_flights_of_each_carrier <- function(ramki){
  sum_of_each_carrier_flight <- function(data){
    data_2 <- data %>%
      group_by(UniqueCarrier) %>%
      summarise(NumberOfFlights = n())
  }
  
  sum_of_each_carrier_flight_for_each_year <- lapply(ramki, sum_of_each_carrier_flight)
  
  merged_sum <- function(list){
      merged_data <- bind_rows(list) %>%
        group_by(UniqueCarrier) %>%
        summarise(SumNumberOfFlights = sum(NumberOfFlights, na.rm = TRUE)) %>%
        mutate(Fraction = SumNumberOfFlights / sum(SumNumberOfFlights))
      
      return(merged_data)
  }
  
  result_sum_of_each_carrier_flight <- merged_sum(sum_of_each_carrier_flight_for_each_year)
  return(result_sum_of_each_carrier_flight)
}

result_summarised_number_and_fraction_of_flights_of_each_carrier <- summarised_number_and_fraction_of_flights_of_each_carrier(ramki)

write.csv(result_summarised_number_and_fraction_of_flights_of_each_carrier, "summarised_number_and_fraction_of_flights_of_each_carrier.csv")

result_summarised_number_and_fraction_of_flights_of_each_carrier <- read.csv("summarised_number_and_fraction_of_flights_of_each_carrier.csv")

change_codes_into_fullnames <- function(data){
  carriers <- read.csv("carriers.csv")
  merged_df <- data %>%
    left_join(carriers, join_by("UniqueCarrier" == "Code")) %>%
    select(c("UniqueCarrier", "Description", "SumNumberOfFlights", "Fraction")) %>%
  return(merged_df)
}

summarised_popularity_of_airlines_with_full_names <- change_codes_into_fullnames(result_summarised_number_and_fraction_of_flights_of_each_carrier)

###################################

# Statystyki dla każdego roku oddzielnie - jaka częśc przylotów była opóźniona, ile było wszystkich lotów, których znamy ArrivalDelay,  ile było opóźnionych przylotów?

number_and_fraction_of_flights_and_delays_for_each_year <- function(ramki){
  fraction_of_delay_flights_for_each_year <- function(data){
    data <- data %>%
      filter(!(is.na(ArrDelay)))
    
    sum_of_flights <- data %>%
      group_by(UniqueCarrier) %>%
      summarise(AllFlights = n())
    
    data_2 <- data %>%
      filter(ArrDelay > 0)
    
    
    sum_of_delayed_flights <- data_2 %>%
      group_by(UniqueCarrier) %>%
      summarise(DelayedFlights = n())
    
    data_3 <- inner_join(sum_of_flights, sum_of_delayed_flights, by = "UniqueCarrier") %>%
      mutate(Fraction = DelayedFlights / AllFlights)
    return(data_3)
  
  }
  
  fraction_of_delay_flights_for_each_year <- lapply(ramki, fraction_of_delay_flights_for_each_year)
  return(fraction_of_delay_flights_for_each_year)
}

result_number_and_fraction_of_flights_and_delays_for_each_year <- number_and_fraction_of_flights_and_delays_for_each_year(ramki)
  
write_dataframes_to_csv(result_number_and_fraction_of_flights_and_delays_for_each_year, "number_and_fraction_of_flights_and_delays_for_each_year")

result_number_and_fraction_of_flights_and_delays_for_each_year <- read_dataframes_from_csv("number_and_fraction_of_flights_and_delays_for_each_year")

#####################################

#Ile lotów w sumie miała każda linia, ile opóźnionych przylotów i jaka to część?

summarised_number_and_fraction_of_flights_and_delays <- function(ramki){

  fraction_of_delay_flights <- function(data){
    data <- data %>%
      filter(!(is.na(ArrDelay)))
    
    sum_of_flights <- data %>%
      group_by(UniqueCarrier) %>%
      summarise(AllFlights = n())
    
    data_2 <- data %>%
      filter(ArrDelay > 0)
    
    
    sum_of_delayed_flights <- data_2 %>%
      group_by(UniqueCarrier) %>%
      summarise(DelayedFlights = n())
    
    data_3 <- inner_join(sum_of_flights, sum_of_delayed_flights, by = "UniqueCarrier")
    return(data_3)
    
  }
  
  fraction_of_delay_flights_result <- lapply(ramki, fraction_of_delay_flights)
  
  
  merged_fraction <- function(list){
    merged_data <- bind_rows(list) %>%
      group_by(UniqueCarrier) %>%
      summarise(across(c(AllFlights, DelayedFlights), list(sum = sum))) %>%
      mutate(Fraction = DelayedFlights_sum / AllFlights_sum)
      
    return(merged_data)
  }
  
  merged_fraction_result <- merged_fraction(fraction_of_delay_flights_result)
  return(merged_fraction_result)
  
}

result_summarised_number_and_fraction_of_flights_and_delays <- summarised_number_and_fraction_of_flights_and_delays(ramki)

write.csv(result_summarised_number_and_fraction_of_flights_and_delays, "summarised_number_and_fraction_of_flights_and_delays.csv")

result_summarised_number_and_fraction_of_flights_and_delays <- read.csv("summarised_number_and_fraction_of_flights_and_delays.csv")

change_codes_into_fullnames_2 <- function(data){
  carriers <- read.csv("carriers.csv")
  merged_df <- data %>%
    left_join(carriers, join_by("UniqueCarrier" == "Code")) %>%
    select(c("UniqueCarrier", "Description", "Fraction")) %>%
    return(merged_df)
}

summarised_fraction_of_delays <- change_codes_into_fullnames_2(result_summarised_number_and_fraction_of_flights_and_delays)

######################################

# Jaka część lotów każdego przewoźnika była odwołana w każdym roku?

cancelled_flights_for_each_year_for_each_carrier <- function(ramki){
  
  cancelled_flights <- function(data){
    
    data <- data %>%
      filter(!(is.na(Cancelled))) %>%
      group_by(UniqueCarrier) %>%
      summarise(AllFlights = n(), CancelledFlights = sum(Cancelled)) %>%
      mutate(Fraction = CancelledFlights / AllFlights)
  }
  
  result <- lapply(ramki, cancelled_flights)
  return(result)
  
}

result_cancelled_flights_for_each_year_for_each_carrier <- cancelled_flights_for_each_year_for_each_carrier(ramki)

write_dataframes_to_csv(result_cancelled_flights_for_each_year_for_each_carrier, "cancelled_flights_for_each_year_for_each_carrier")

result_cancelled_flights_for_each_year_for_each_carrier <- read_dataframes_from_csv("cancelled_flights_for_each_year_for_each_carrier")

#########################################

# Jaka część lotów każdego przewoźnika była odwołana?

cancelled_flights_for_each_carrier <- function(ramki){
  
  cancelled_flights <- function(data){
    
    data <- data %>%
      filter(!(is.na(Cancelled)))
      
      
    number_of_flights <- data %>%
      group_by(UniqueCarrier) %>%
      summarise(AllFlights = n())
    
    number_of_cancelled_flights <- data %>%
      group_by(UniqueCarrier) %>%
      summarise(CancelledFlights = sum(Cancelled))
    
    data_2 <- inner_join(number_of_flights, number_of_cancelled_flights, by = "UniqueCarrier")
    return(data_2)
  }
  
  cancelled_flights_for_each_year <- lapply(ramki, cancelled_flights)
  
  merged_cancelled_flights <- function(list){
    merged_data <- bind_rows(list) %>%
      group_by(UniqueCarrier) %>%
      summarise(across(c(AllFlights, CancelledFlights), list(sum = sum))) %>%
      mutate(Fraction = CancelledFlights_sum / AllFlights_sum)
    return(merged_data)
  }
  
  result <- merged_cancelled_flights(cancelled_flights_for_each_year)
  return(result)
  
}

result_cancelled_flights_for_each_carrier <- cancelled_flights_for_each_carrier(ramki)

write.csv(result_cancelled_flights_for_each_carrier, "cancelled_flights_for_each_carrier.csv")

result_cancelled_flights_for_each_carrier <- read.csv("cancelled_flights_for_each_carrier.csv")

################################

#Średni czas opóźnienia każdego dnia tygodnia w każdym roku

mean_of_delay_time_for_each_day_of_week_for_each_year <- function(ramki){
  
  mean_of_delay_time <- function(data){
    
    data <- data %>%
      filter(!(is.na(ArrDelay))) %>%
      filter(ArrDelay > 0)
    
    sum_of_delay_times <- data %>%
      group_by(DayOfWeek) %>%
      summarise(DelayMean = mean(ArrDelay))
    
  }
  
  mean_of_delay_time_for_each_day_for_each_year <- lapply(ramki, mean_of_delay_time)
  return(mean_of_delay_time_for_each_day_for_each_year)
  
}

result_mean_of_delay_time_for_each_day_of_week_for_each_year <- mean_of_delay_time_for_each_day_of_week_for_each_year(ramki)

write_dataframes_to_csv(result_mean_of_delay_time_for_each_day_of_week_for_each_year, "mean_of_delay_time_for_each_day_of_week_for_each_year")

result_mean_of_delay_time_for_each_day_of_week_for_each_year <- read_dataframes_from_csv("mean_of_delay_time_for_each_day_of_week_for_each_year")

#################################

# Średni czas opóźnienia każdego dnia tygodnia

mean_of_delay_time_for_each_day_of_week <- function(ramki){
  
  mean_of_delay_time <- function(data){
    data <- data %>%
      filter(!(is.na(ArrDelay))) %>%
      filter(ArrDelay > 0) 
    
    result <- data %>%
      group_by(DayOfWeek) %>%
      mutate(
        SummarisedTime = sum(ArrDelay),
        NumberOfDelayedFlights = n()
      ) %>%
      distinct(DayOfWeek, .keep_all = TRUE)
    return(result)
    
  }
  
  mean_of_delay_time_for_every_year <- lapply(ramki, mean_of_delay_time)
  
  merged_mean <- function(list){
    merged_data <- bind_rows(list) %>%
      group_by(DayOfWeek) %>%
      summarise(across(c(SummarisedTime, NumberOfDelayedFlights), list(sum = sum))) %>%
      mutate(Mean = SummarisedTime_sum / NumberOfDelayedFlights_sum)
    
    return(merged_data[c("DayOfWeek", "Mean")])
  }
  
  result <- merged_mean(mean_of_delay_time_for_every_year)  
}

result_mean_of_delay_time_for_each_day_of_week <- mean_of_delay_time_for_each_day_of_week(ramki)

write.csv(result_mean_of_delay_time_for_each_day_of_week, "mean_of_delay_time_for_each_day_of_week.csv")

result_mean_of_delay_time_for_each_day_of_week <- read.csv("mean_of_delay_time_for_each_day_of_week.csv")

#################################

# Średni czas opóźnienia każdego dnia tygodnia ze względu na miesiące

mean_of_delay_time_for_each_day_of_week_for_each_month <- function(ramki){
  
  mean_of_delay_time <- function(data){
    data <- data %>%
      filter(!(is.na(ArrDelay))) %>%
      filter(ArrDelay > 0) 
    
    result <- data %>%
      group_by(Month, DayOfWeek) %>%
      mutate(
        SummarisedTime = sum(ArrDelay),
        NumberOfDelayedFlights = n()
      ) %>%
      distinct(Month, DayOfWeek, .keep_all = TRUE)
    return(result)
    
  }
  
  mean_of_delay_time_for_every_year <- lapply(ramki, mean_of_delay_time)
  
  merged_mean <- function(list){
    merged_data <- bind_rows(list) %>%
      group_by(Month, DayOfWeek) %>%
      summarise(across(c(SummarisedTime, NumberOfDelayedFlights), list(sum = sum))) %>%
      mutate(Mean = SummarisedTime_sum / NumberOfDelayedFlights_sum)
    return(merged_data[c("Month", "DayOfWeek", "Mean")])
  }
  
  data_frame <- merged_mean(mean_of_delay_time_for_every_year)  
  
  result <- split(data_frame, f = data_frame$Month)
  
  return(result)
  
}

result_mean_of_delay_time_for_each_day_of_week_for_each_month <- mean_of_delay_time_for_each_day_of_week_for_each_month(ramki)

write_month_dataframes_to_csv(result_mean_of_delay_time_for_each_day_of_week_for_each_month, "mean_of_delay_time_for_each_day_of_week_for_each_month")

result_mean_of_delay_time_for_each_day_of_week_for_each_month <- read_month_dataframes_from_csv("mean_of_delay_time_for_each_day_of_week_for_each_month")

##################################

# Średnia opóźnień dla każdego dnia tygodnia - wykres


plot_of_delays_for_each_day_of_week <- function(data){
  df <- data[, c(2,3)]
  df$DayOfWeek <- factor(df$DayOfWeek,
                         labels = c("Poniedziałek", "Wtorek", "Środa", "Czwartek", "Piątek", "Sobota", "Niedziela"))
  
  # Create the line plot
  plot_1 <- ggplot(df, aes(x = DayOfWeek, y = Mean)) +
    geom_line(aes(group = 1), color = "red", size = 1) +
    geom_point(color = "red", size = 3) +
    labs(x = "Dzień tygodnia", y = "Średni czas opóźnienia [min]") +
    ggtitle("Średni czas opóźnienia lotów w zależności od dnia tygodnia") + 
    coord_cartesian(ylim = c(0, 30)) + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18), 
          legend.title = element_text(hjust = 0.5, face = "bold", size = 16), 
          axis.text.y = element_text(size = 14, margin = margin(r = 0.5, unit = "cm")),
          legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16, margin = margin(t = 0.5,unit = "cm")),
          axis.text.x = element_text( size = 14, margin = margin(t = 0.5, unit = "cm")), 
          axis.title.y = element_text(size = 16, margin = margin(r = 0.5, unit = "cm")),
          plot.margin = margin(1, 1, 1, 1, "cm")) +
    scale_x_discrete()
  ggsave("Mean_of_delay_time_for_each_day_of_week.png", plot = plot_1, width = 10, height = 7)
  
}

plot_of_delays_for_each_day_of_week(result_mean_of_delay_time_for_each_day_of_week)

#######################################

# Najbardziej popularne linie - wykres

plot_of_most_popular_carriers <- function(data){
  sorted_data <- data[order(data$Fraction, decreasing = TRUE), ]
  
  sorted_data$UniqueCarrier <- factor(sorted_data$UniqueCarrier, levels = rev(sorted_data$UniqueCarrier))

  top_10 <- head(sorted_data, 10)
  
  top_10$legend_label <- paste(top_10$UniqueCarrier, "-", sub("^(.*?)\\s*\\(.*\\)$", "\\1", top_10$Description))
  
  top_10$legend_label <- factor(top_10$legend_label, levels = top_10$legend_label)
  
  plot <- ggplot(top_10, aes(x = UniqueCarrier, y = 100 * Fraction, fill = legend_label)) +
    geom_bar(stat = "identity") +
    labs(title = "Top 10 najbardziej popularnych linii lotniczych", x = "Linie lotnicze", y = "Udział w rynku [%]") +
    theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 18), 
          legend.title = element_text(hjust = 0.5, face = "bold", size = 16), 
          axis.text.y = element_text(size = 14, margin = margin(r = 0.5, unit = "cm")),
          legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16, margin = margin(t = 0.5,unit = "cm")),
          axis.text.x = element_text( size = 14, margin = margin(t = 0.5, unit = "cm")), 
          axis.title.y = element_text(size = 16, margin = margin(r = 0.5, unit = "cm")),
          plot.margin = margin(1, 1, 1, 1, "cm"))
  
  plot <- plot + guides(fill = guide_legend(title = "Linie lotnicze", nrow = 10))
  
  ggsave("most_popular_airlines.png", plot, width = 15, height = 5, dpi = 300)
}

plot_of_most_popular_carriers(summarised_popularity_of_airlines_with_full_names)


######################################

# Firmy, które mają najmniej opóźnień - wykres

plot_of_most_reliable_airlines <- function(data){
  sorted_data <- data[order(data$Fraction, decreasing = FALSE), ]
  
  sorted_data$UniqueCarrier <- factor(sorted_data$UniqueCarrier, levels = rev(sorted_data$UniqueCarrier))
  
  top_10 <- head(sorted_data, 10)
  
  top_10$legend_label <- paste(top_10$UniqueCarrier, "-", sub("^(.*?)\\s*\\(.*\\)$", "\\1", top_10$Description))
  
  top_10$legend_label <- factor(top_10$legend_label, levels = top_10$legend_label)
  
  plot <- ggplot(top_10, aes(x = UniqueCarrier, y = 100 * Fraction, fill = legend_label)) +
    geom_bar(stat = "identity") +
    labs(title = "Top 10 linii lotniczych o najmniejszej liczbie opóźnień", x = "Linie lotnicze", y = "Opóźnione loty [%]") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18), 
      legend.title = element_text(hjust = 0.5, face = "bold", size = 16), 
      axis.text.y = element_text(size = 14, margin = margin(r = 0.5, unit = "cm")),
      legend.text = element_text(size = 14),
      axis.title.x = element_text(size = 16, margin = margin(t = 0.5,unit = "cm")),
      axis.text.x = element_text( size = 14, margin = margin(t = 0.5, unit = "cm")), 
      axis.title.y = element_text(size = 16, margin = margin(r = 0.5, unit = "cm")),
      plot.margin = margin(1, 1, 1, 1, "cm"))
  
  plot <- plot + guides(fill = guide_legend(title = "Linie lotnicze", nrow = 10))
  
  ggsave("most_reliable_airlines.png", plot, width = 15, height = 5, dpi = 300)
}

plot_of_most_reliable_airlines(summarised_fraction_of_delays)

###############################################

mean_of_delay_time_for_each_hour <- function(ramki){

  mean_of_delay_time <- function(data){
    data <- data %>%
      filter(!(is.na(ArrDelay))) %>%
      filter(ArrDelay > 0) %>%
      filter(DepTime >= 0 & DepTime <= 2400) %>%
      arrange(DepTime)
    
    breakpoints <- seq(0, 2400, by = 200)
    labels <- paste0(breakpoints[-length(breakpoints)], "-", breakpoints[-1])
    labels[length(labels)] <- paste0(labels[length(labels)], "+")
    
    result <- data %>%
      mutate(Group = cut(DepTime, breaks = breakpoints, include.lowest = TRUE)) %>%
      group_by(Group) %>%
      mutate(
        SummarisedTime = sum(ArrDelay),
        NumberOfDelayedFlights = n()
      ) %>%
      distinct(Group, .keep_all = TRUE)
    return(result[c("Group", "SummarisedTime", "NumberOfDelayedFlights")])
    
  }
  
  mean_of_delay_time_for_every_year <- lapply(ramki, mean_of_delay_time)
  
  merged_mean <- function(list){
    merged_data <- bind_rows(list) %>%
      group_by(Group) %>%
      summarise(across(c(SummarisedTime, NumberOfDelayedFlights), list(sum = sum))) %>%
      mutate(Mean = SummarisedTime_sum / NumberOfDelayedFlights_sum)
    
    return(merged_data[c("Group", "Mean")])
  }
  
  result <- merged_mean(mean_of_delay_time_for_every_year)  
}

result_mean_of_delay_time_for_each_hour <- mean_of_delay_time_for_each_hour(ramki)
write.csv(result_mean_of_delay_time_for_each_hour, "mean_of_delay_time_for_each_hour.csv")

##################################

plot_of_delays_for_each_hour <- function(df){
  df$Group <- factor(df$Group,
                         labels = c("00-02", "02-04", "04-06", "06-08", "08-10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24"))
  
  # Create the line plot
  plot_1 <- ggplot(df, aes(x = Group, y = Mean)) +
    geom_line(aes(group = 1), color = "red", size = 1) +
    geom_point(color = "red", size = 3) +
    labs(x = "Godzina wylotu w czasie lokalnym", y = "Średni czas opóźnienia [min]") +
    ggtitle("Średni czas opóźnienia lotów w zależności od godziny wylotu") + 
    coord_cartesian(ylim = c(0, 100)) + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18), 
          legend.title = element_text(hjust = 0.5, face = "bold", size = 16), 
          axis.text.y = element_text(size = 14, margin = margin(r = 0.5, unit = "cm")),
          legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16, margin = margin(t = 0.5,unit = "cm")),
          axis.text.x = element_text( size = 14, margin = margin(t = 0.5, unit = "cm")), 
          axis.title.y = element_text(size = 16, margin = margin(r = 0.5, unit = "cm")),
          plot.margin = margin(1, 1, 1, 1, "cm")) +
    scale_x_discrete()
  ggsave("Mean_of_delay_time_for_each_hour.png", plot = plot_1, width = 10, height = 7)
  
}

plot_of_delays_for_each_hour(result_mean_of_delay_time_for_each_hour)


###################################

median_of_delay_time_for_each_hour <- function(ramki){
  
  median_of_delay_time <- function(data){
    data <- data %>%
      filter(!(is.na(ArrDelay))) %>%
      filter(ArrDelay > 0) %>%
      filter(DepTime >= 0 & DepTime <= 2400) %>%
      arrange(DepTime)
    
    breakpoints <- seq(0, 2400, by = 200)
    labels <- paste0(breakpoints[-length(breakpoints)], "-", breakpoints[-1])
    labels[length(labels)] <- paste0(labels[length(labels)], "+")
    
    result <- data %>%
      mutate(Group = cut(DepTime, breaks = breakpoints, include.lowest = TRUE)) %>%
      group_by(Group) %>%
      mutate(
        SummarisedTime = median(ArrDelay),
        NumberOfDelayedFlights = n()
      ) %>%
      distinct(Group, .keep_all = TRUE)
    return(result[c("Group", "SummarisedTime", "NumberOfDelayedFlights")])
  }
  
  median_of_delay_time_for_every_year <- lapply(ramki, median_of_delay_time)
  
  merged_median <- function(list){
    merged_data <- bind_rows(list) %>%
      group_by(Group) %>%
      summarize(across(c(SummarisedTime, NumberOfDelayedFlights), list(median = median))) %>%
      mutate(Median = SummarisedTime_median)
    
    return(merged_data[c("Group", "Median")])
  }
  
  result <- merged_median(median_of_delay_time_for_every_year)  
}

result_median_of_delay_time_for_each_hour <- median_of_delay_time_for_each_hour(ramki)


################################################

plot_of_delays_for_each_hour <- function(df){
  df$Group <- factor(df$Group,
                     labels = c("00-02", "02-04", "04-06", "06-08", "08-10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24"))
  
  # Create the line plot
  plot_1 <- ggplot(df, aes(x = Group, y = Median)) +
    geom_line(aes(group = 1), color = "red", size = 1) +
    geom_point(color = "red", size = 3) +
    labs(x = "Godzina wylotu w czasie lokalnym", y = "Mediana czasu opóźnienia [min]") +
    ggtitle("Mediana czasu opóźnienia lotów w zależności od godziny wylotu") + 
    coord_cartesian(ylim = c(0, 50)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18), 
          legend.title = element_text(hjust = 0.5, face = "bold", size = 16), 
          axis.text.y = element_text(size = 14, margin = margin(r = 0.5, unit = "cm")),
          legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16, margin = margin(t = 0.5,unit = "cm")),
          axis.text.x = element_text( size = 14, margin = margin(t = 0.5, unit = "cm")), 
          axis.title.y = element_text(size = 16, margin = margin(r = 0.5, unit = "cm")),
          plot.margin = margin(1, 1, 1, 1, "cm")) +
    scale_x_discrete()
  ggsave("Median_of_delay_time_for_each_hour.png", plot = plot_1, width = 10, height = 7)
  
}

plot_of_delays_for_each_hour(result_median_of_delay_time_for_each_hour)
