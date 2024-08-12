# server.R
library(shiny)
library(ggplot2)
library(DT) 

# Establishing estimates
menest <- c(m60 = "6.95", m200 = "21.77", m400 = "48.86", m600 = "1:21.31", m800 = "1:51.19", m1k = "2:28.74", mmile = "4:08.35", m3k = "8:11.33", m5k = "14:25.13", m60h = "8.03", mhj = "2.11", mpv = "4.88", mlj = "7.17", mtj = "14.22", msp = "16.32", mwt = "18.53", mhep = "4882.85")  
womenest <-c(w60 = "7.65", w200 = "25.32", w400 = "56.48", w600 = "1:36.31", w800 = "2:14.31", w1k = "2:56.19", wmile = "4:58.64", w3k = "9:39.92", w5k = "16:49.06", w60h = "8.68", whj = "1.74", wpv = "3.87", wlj = "5.83", wtj = "11.94", wsp = "14.09", wwt = "18.63", wpent = "3526.456")

# Loading in men and women datasets 
mendata <- readRDS("/Users/camdenheafitz/Desktop/Cheafitz25.github.io/men_2023_12_07.Rds")
womendata <-readRDS("/Users/camdenheafitz/Desktop/Cheafitz25.github.io/women_2023_12_07.Rds")

# Formats time correctly
formatTime <- function(time) {
  time <- as.numeric(time)
  minutes <- floor(time / 60)
  seconds <- time %% 60
  formatted_time <- ifelse(minutes == 0, sprintf("%.2f", seconds), sprintf("%d:%05.2f", minutes, seconds))
  return(formatted_time)
}

shinyServer(function(input, output) {
  
  # About Tab
  output$about_text <- renderText({
    "This Shiny app provides insights and estimates based on collegiate track and field results from 2009-2023. It includes two main tabs: Track and Field Insights and 2024 Estimates. Explore historical performance data by selecting gender, event, and place. Get estimated winning performances for the 2024 New England Championship meet by selecting gender and event. Feel free to explore and analyze track and field data with this app! Here is a link to the TFFRS homepage: https://www.tfrrs.org/"
  })
  
  # Creating dataset based on user input
  selected_data <- reactive({
    if (input$insights_sex == "Men") {
      dataset <- mendata
    } else {
      dataset <- womendata
    }
    
    event <- input$insights_event
    place <- input$insights_place
    
    # Filter data based on event
    if (event %in% c("60", "200", "400", "600", "800", "1k", "Mile", "3k", "5k", "60 Hurdles")) {
      dataset <- dataset[dataset$EVENT == event, ]
    } else if (event %in% c("High Jump", "Pole Vault", "Triple Jump", "Long Jump", "Shot Put", "Weight Throw")) {
      dataset <- dataset[dataset$EVENT == event, ]
    } else if (event %in% c("Heptathlon", "Pentathlon")) {
      dataset <- dataset[dataset$EVENT == event, ]
    }
    
    # Filter data Based on place
    if (!is.null(place) && place != "place") {
      dataset <- dataset[dataset$PL == place, ]
    }
    
    return(dataset)
  })
  
  #Insight tab
  output$insights_trackFieldPlot <- renderPlot({
    formatted_data <- selected_data()
    
    if (!is.null(formatted_data)) { # Tells which unit should be on the y-axis
      event <- input$insights_event
      performance <- switch(
        event,
        "60" = "TIME",
        "200" = "TIME",
        "400" = "TIME",
        "600" = "TIME",
        "800" = "TIME",
        "1k" = "TIME",
        "Mile" = "TIME",
        "3k" = "TIME",
        "5k" = "TIME",
        "60 Hurdles" = "TIME",
        "High Jump" = "MARK",
        "Pole Vault" = "MARK",
        "Triple Jump" = "MARK",
        "Long Jump" = "MARK",
        "Shot Put" = "MARK",
        "Weight Throw" = "MARK",
        "Heptathlon" = "POINTS",
        "Pentathlon" = "POINTS",
        "default" = "TIME"
      )
      # Graph
      ggplot(formatted_data, aes(x = YEAR, y = get(performance))) +
        geom_point(position = position_jitter(width = 0.2), size = 2, color = ifelse(formatted_data$PL == 1, "darkgoldenrod2", "black")) +
        labs(title = paste("Track and Field Data for", input$insights_sex, "-", input$insights_event, "-", "Place", input$insights_place),
             x = "Year",
             y = performance) +
        geom_smooth() +
        scale_x_continuous(breaks = unique(formatted_data$YEAR))
    }
  })
  
  #Data table output
  output$insights_dataTable <- renderDT({
    formatted_data <- selected_data()
    
    if (!is.null(formatted_data)) {
      event <- input$insights_event
      
      # Define columns to display based on the selected event
      display_columns <- switch(
        event,
        "60" = c("NAME", "YEAR", "TEAM", "EVENT", "TIME", "PL"),
        "200" = c("NAME", "YEAR", "TEAM", "EVENT", "TIME", "PL"),
        "400" = c("NAME", "YEAR", "TEAM", "EVENT", "TIME", "PL"),
        "600" = c("NAME", "YEAR", "TEAM", "EVENT", "TIME", "PL"),
        "800" = c("NAME", "YEAR", "TEAM", "EVENT", "TIME", "PL"),
        "1k" = c("NAME", "YEAR", "TEAM", "EVENT", "TIME", "PL"),
        "Mile" = c("NAME", "YEAR", "TEAM", "EVENT", "TIME", "PL"),
        "3k" = c("NAME", "YEAR", "TEAM", "EVENT", "TIME", "PL"),
        "5k" = c("NAME", "YEAR", "TEAM", "EVENT", "TIME", "PL"),
        "60 Hurdles" = c("NAME", "YEAR", "TEAM", "EVENT", "TIME", "PL"),
        "High Jump" = c("NAME", "YEAR", "TEAM", "EVENT", "MARK", "PL"),
        "Pole Vault" = c("NAME", "YEAR", "TEAM", "EVENT", "MARK", "PL"),
        "Triple Jump" = c("NAME", "YEAR", "TEAM", "EVENT", "MARK", "PL"),
        "Long Jump" = c("NAME", "YEAR", "TEAM", "EVENT", "MARK", "PL"),
        "Shot Put" = c("NAME", "YEAR", "TEAM", "EVENT", "MARK", "PL"),
        "Weight Throw" = c("NAME", "YEAR", "TEAM", "EVENT", "MARK", "PL"),
        "Heptathlon" = c("NAME", "YEAR", "TEAM", "EVENT", "POINTS", "PL"),
        "Pentathlon" = c("NAME", "YEAR", "TEAM", "EVENT", "POINTS", "PL")
      )
      
      # Subset the data frame to include only the selected columns
      formatted_data <- formatted_data[, display_columns, drop = FALSE]
      
      # Format time values in 'TIME' column
      if ("TIME" %in% colnames(formatted_data)) {
        formatted_data$TIME <- sapply(formatted_data$TIME, formatTime)
      }
    }
    
    datatable(formatted_data, options = list(pageLength = 15))
  })
  
  # 2024 Estimates tab, hardcoding estimates 
  output$estimates_estimationText <- renderText({
    if (input$estimates_sex == "Men" && input$estimates_event == "60") {
      estimate_value <- menest["m60"]
      estimate_message <- paste("I estimate the winning men's 60m performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "200") {
      estimate_value <- menest["m200"]
      estimate_message <- paste("I estimate the winning men's 200m performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "400") {
      estimate_value <- menest["m400"]
      estimate_message <- paste("I estimate the winning men's 400m performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "600") {
      estimate_value <- menest["m600"]
      estimate_message <- paste("I estimate the winning men's 600m performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "800") {
      estimate_value <- menest["m800"]
      estimate_message <- paste("I estimate the winning men's 800 performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "1k") {
      estimate_value <- menest["m1k"]
      estimate_message <- paste("I estimate the winning men's 1k performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "Mile") {
      estimate_value <- menest["mmile"]
      estimate_message <- paste("I estimate the winning men's mile performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "3k") {
      estimate_value <- menest["m3k"]
      estimate_message <- paste("I estimate the winning men's 3k performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "5k") {
      estimate_value <- menest["m5k"]
      estimate_message <- paste("I estimate the winning men's 5k performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "60 Hurdles") {
      estimate_value <- menest["m60h"]
      estimate_message <- paste("I estimate the winning men's 60 hurdles performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "High Jump") {
      estimate_value <- menest["mhj"]
      estimate_message <- paste("I estimate the winning men's high jump performance to be", estimate_value, "meters.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "Pole Vault") {
      estimate_value <- menest["mpv"]
      estimate_message <- paste("I estimate the winning men's pole vault performance to be", estimate_value, "meters.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "Long Jump") {
      estimate_value <- menest["mlj"]
      estimate_message <- paste("I estimate the winning men's long jump performance to be", estimate_value, "meterss.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "Triple Jump") {
      estimate_value <- menest["mtj"]
      estimate_message <- paste("I estimate the winning men's triple jump performance to be", estimate_value, "meters.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "Shot Put") {
      estimate_value <- menest["mwt"]
      estimate_message <- paste("I estimate the winning men's shot put performance to be", estimate_value, "meters.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "Weight Throw") {
      estimate_value <- menest["msp"]
      estimate_message <- paste("I estimate the winning men's weight throw performance to be", estimate_value, "meters.")
    } else if (input$estimates_sex == "Men" && input$estimates_event == "Heptathlon") {
      estimate_value <- menest["mhep"]
      estimate_message <- paste("I estimate the winning men's heptathlon performance to be", estimate_value, "points.")
      
      # Women
    }  else if(input$estimates_sex == "Women" && input$estimates_event == "60"){
      estimate_value <- womenest["w60"]
      estimate_message <- paste("I estimate the winning women's 60m performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "200") {
      estimate_value <- womenest["w200"]
      estimate_message <- paste("I estimate the winning women's 200m performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "400") {
      estimate_value <- womenest["w400"]
      estimate_message <- paste("I estimate the winning women's 600m performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "600") {
      estimate_value <- womenest["w600"]
      estimate_message <- paste("I estimate the winning women's 800m performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "800") {
      estimate_value <- womenest["w800"]
      estimate_message <- paste("I estimate the winning women's 1k performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "1k") {
      estimate_value <- womenest["w1k"]
      estimate_message <- paste("I estimate the winning women's 200m performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "Mile") {
      estimate_value <- womenest["wmile"]
      estimate_message <- paste("I estimate the winning women's mile performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "3k") {
      estimate_value <- womenest["w3k"]
      estimate_message <- paste("I estimate the winning women's 3k performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "5k") {
      estimate_value <- womenest["w5k"]
      estimate_message <- paste("I estimate the winning women's 5k performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "60 Hurdles") {
      estimate_value <- womenest["w60h"]
      estimate_message <- paste("I estimate the winning women's 60 hurdles performance to be", estimate_value, "seconds.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "High Jump") {
      estimate_value <- womenest["whj"]
      estimate_message <- paste("I estimate the winning women's high jump performance to be", estimate_value, "meters.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "Pole Vault") {
      estimate_value <- womenest["wpv"]
      estimate_message <- paste("I estimate the winning women's pole vault performance to be", estimate_value, "meters.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "Long Jump") {
      estimate_value <- womenest["wlj"]
      estimate_message <- paste("I estimate the winning women's long jump performance to be", estimate_value, "meterss.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "Triple Jump") {
      estimate_value <- womenest["wtj"]
      estimate_message <- paste("I estimate the winning women's triple jump performance to be", estimate_value, "meters.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "Shot Put") {
      estimate_value <- womenest["wwt"]
      estimate_message <- paste("I estimate the winning women's shot put performance to be", estimate_value, "meters.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "Weight Throw") {
      estimate_value <- womenest["wsp"]
      estimate_message <- paste("I estimate the winning women's weight throw performance to be", estimate_value, "meters.")
    } else if (input$estimates_sex == "Women" && input$estimates_event == "Pentathlon") {
      estimate_value <- womenest["wpent"]
      estimate_message <- paste("I estimate the winning women's pentathlon performance to be", estimate_value, "points.")
    } else {
      estimate_message <- "No estimation available for this combination."
    }
    
    return(estimate_message)
  })
})