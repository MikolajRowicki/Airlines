#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
Mikolaj/DataFrames
setwd("C:/Airline/DataSets")

plot_of_fraction_of_delays_of_most_popular_carriers <- function(df_list, name_1, name_2){
  
  
  filtered_df_list <- lapply(df_list, function(df) {
    df %>%
      select(UniqueCarrier, AllFlights, DelayedFlights) %>%
      filter(UniqueCarrier %in% c(name_1, name_2)) %>%
      arrange(match(UniqueCarrier, c(name_1, name_2)))
  })
  
  check_and_add_rows <- function(df_list, name_1, name_2) {
    for (i in seq_along(df_list)) {
      if (!(name_1 %in% df_list[[i]]$UniqueCarrier)) {
        new_row <- data.frame(UniqueCarrier = name_1, AllFlights = 0, DelayedFlights = 0)
        df_list[[i]] <- rbind(new_row, df_list[[i]])
      }
      
      if (!(name_2 %in% df_list[[i]]$UniqueCarrier)) {
        new_row <- data.frame(UniqueCarrier = name_2, AllFlights = 0, DelayedFlights = 0)
        df_list[[i]] <- rbind(df_list[[i]], new_row)
      }
    }
    
    return(df_list)
  }
  
  filtered_df_list <- check_and_add_rows(filtered_df_list, name_1, name_2)
  
  
  merged_df <- bind_cols(filtered_df_list)
  
  
  for(i in c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57 ,63)){
    column_name <- paste0("Fraction_", i)
    merged_df <- mutate(merged_df, !!column_name :=
                          ifelse(rowSums(merged_df[, c(i - 1, i + 2)]) == 0, 0,
                                 100 * (merged_df[, i] + merged_df[, i + 3]) /
                                   (merged_df[, i - 1] + merged_df[, i + 2])))
    
    
  }
  
  merged_df <- merged_df[, c(1, 67:77)]
  
  colnames(merged_df) <- c("UniqueCarrier", paste0("Fraction_", "87/88"), paste0("Fraction_", "89/90"), paste0("Fraction_", "91/92"), paste0("Fraction_",  "93/94"),
                           paste0("Fraction_",  "95/96"), paste0("Fraction_",  "97/98"), paste0("Fraction_", "99/00"), paste0("Fraction_", "01/02"), paste0("Fraction_", "03/04"), paste0("Fraction_","05/06"), paste0("Fraction_", "07/08"))
  
  merged_df <- merged_df %>%
    pivot_longer(cols = starts_with("Fraction"), names_to = "Year", values_to = "Fraction") %>%
    mutate(Year = substring(Year, nchar(Year) - 4))
  
  
  ggplot(merged_df, aes(x = Year, y = Fraction, fill = UniqueCarrier)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Lata", y = "Procent lotów, które były opóźnione", fill = "Linie lotnicze", title = "Jaka część lotów była opóźniona?") +
    scale_x_discrete(limits = unique(merged_df$Year)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
          plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),  # Powiększ czcionkę etykiety osi X
          axis.title.y = element_text(size = 14, margin(t = 10)),
          legend.text = element_text(size = 14)) 
  
}

plot_of_popularity_of_most_popular_carriers <- function(df_list, name_1, name_2){
  filtered_df_list <- lapply(df_list, function(df) {
    df %>%
      select(UniqueCarrier, Sum) %>%
      filter(UniqueCarrier %in% c(name_1, name_2)) %>%
      arrange(match(UniqueCarrier, c(name_1, name_2)))
  })
  
  check_and_add_rows <- function(df_list, name_1, name_2) {
    for (i in seq_along(df_list)) {
      if (!(name_1 %in% df_list[[i]]$UniqueCarrier)) {
        new_row <- data.frame(UniqueCarrier = name_1, Sum = 0)
        df_list[[i]] <- rbind(new_row, df_list[[i]])
      }
      
      if (!(name_2 %in% df_list[[i]]$UniqueCarrier)) {
        new_row <- data.frame(UniqueCarrier = name_2, Sum = 0)
        df_list[[i]] <- rbind(df_list[[i]], new_row)
        
      }
      
      
    }
    
    return(df_list)
  }
  
  filtered_df_list <- check_and_add_rows(filtered_df_list, name_1, name_2)
  
  merged_df <- bind_cols(filtered_df_list)
  
  for(i in c(2, 6, 10, 14, 18, 22, 26, 30, 34, 38, 42)){
    column_name <- paste0("Sum_", i)
    merged_df <- mutate(merged_df, !!column_name := (merged_df[, i] + merged_df[, i + 2]))
  }
  
  merged_df <- merged_df[, c(1, 45:55)]
  
  colnames(merged_df) <- c("UniqueCarrier", paste0("Fraction_", "87/88"), paste0("Fraction_", "89/90"), paste0("Fraction_", "91/92"), paste0("Fraction_",  "93/94"),
                           paste0("Fraction_",  "95/96"), paste0("Fraction_",  "97/98"), paste0("Fraction_", "99/00"), paste0("Fraction_", "01/02"), paste0("Fraction_", "03/04"), paste0("Fraction_","05/06"), paste0("Fraction_", "07/08"))
  
  merged_df <- merged_df %>%
    pivot_longer(cols = starts_with("Fraction"), names_to = "Year", values_to = "Fraction") %>%
    mutate(Year = substring(Year, nchar(Year) - 4))
  
  
  ggplot(merged_df, aes(x = Year, y = Fraction, fill = UniqueCarrier)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Lata", y = "Liczba lotów", fill = "Linie lotnicze", title = "Popularność linii lotniczych") +
    scale_x_discrete(limits = unique(merged_df$Year)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
          plot.title = element_text(hjust = 0.5), 
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),  # Powiększ czcionkę etykiety osi X
          axis.title.y = element_text(size = 14, margin(t = 10)),
          legend.text = element_text(size = 14)) 
}

result_number_and_fraction_of_flights_and_delays_for_each_year <- read_dataframes_from_csv("number_and_fraction_of_flights_and_delays_for_each_year")

result_number_and_fraction_of_flights_of_each_carrier_for_each_year <- read_dataframes_from_csv("number_and_fraction_of_flights_of_each_carrier_for_each_year")

read_dataframes_from_csv <- function(file_prefix) {
  dataframes <- list()
  for (i in 1:22) {
    filename <- paste0(file_prefix, "/", file_prefix, "_", i + 1986, ".csv")
    df <- read.csv(filename)
    dataframes[[i]] <- df
  }
  return(dataframes)
}

library(shiny)
library(dplyr)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Popularity and reliability of airlines"),

    # Sidebar with a slider input for number of bins 
    ui <- fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput("variable1", "Choose the first airline:", choices = c("9E", "AA", "AQ", "AS", "B6", "CO","DH" ,"DL","EA","EV","F9","FL","HA","HP","ML (1)","MQ","NW","OH","OO","PA (1)","PI","PS","TW","TZ", "US", "UA", "WN", "XE", "YV")),
          selectInput("variable2", "Choose the second airline:", choices = c("9E","AA", "AQ", "AS", "B6", "CO","DH" ,"DL","EA",'EV',"F9","FL","HA","HP","ML (1)","MQ","NW","OH","OO","PA (1)","PI","PS","TW","TZ" ,"US", "UA", "WN", "XE", "YV"))
        ),
        mainPanel(
          plotOutput("plot1"),
          plotOutput("plot2")
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    selected_var1 <- input$variable1
    selected_var2 <- input$variable2
    
    if (!is.null(selected_var1) && !is.null(selected_var2)) {
      plot_of_fraction_of_delays_of_most_popular_carriers(result_number_and_fraction_of_flights_and_delays_for_each_year, selected_var1, selected_var2)
    } else {
      # Render a message if both variables are not selected
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", main = "Select two variables")
    }
  })
  
  output$plot2 <- renderPlot({
    selected_var1 <- input$variable1
    selected_var2 <- input$variable2
    
    if (!is.null(selected_var1) && !is.null(selected_var2)) {
      plot_of_popularity_of_most_popular_carriers(result_number_and_fraction_of_flights_of_each_carrier_for_each_year, selected_var1, selected_var2)
    } else {
      # Render a message if both variables are not selected
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", main = "Select two variables")
    }
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
