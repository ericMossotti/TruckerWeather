# Load necessary libraries
library(DBI)          # For database connectivity
library(ggplot2)      # For creating plots
library(scales)       # For scaling and formatting axes
library(openair)      # For specialized plots like wind roses
source("./scripts/Output/Plots/plot_themer.R")  # Custom theme for ggplot

# Helper function to execute a query and return the result
execute_query <- function(con, query) {
     dbGetQuery(con, query)  # Execute the SQL query and return the result
}


plot_temperature_trend <- function(con, freezing_threshold = 32) {
     # Query to fetch temperature data for the day
     query <- "
    SELECT
      temperature_2m,
      time_only,
      common_date,
      month_day
    FROM
      forecast_data
    WHERE
      latitude = 38.748;
  "
     
     data <- execute_query(con, query)  # Execute the query and get the data
     
     # Calculate bar width based on time intervals
     if (nrow(data) > 1) {
          time_diff <- as.numeric(difftime(data$common_date[2], data$common_date[1], units = "secs"))
     } else {
          time_diff <- 3600  # Default to 1 hour if only one data point
     }
     half_width <- time_diff / 2
     
     # Prepare data for rectangular columns
     data <- data %>%
          arrange(common_date) %>%
          mutate(
               xmin = common_date - half_width,
               xmax = common_date + half_width,
               fill_group = ifelse(temperature_2m > freezing_threshold, "above freezing", "below freezing"),
               ymin = ifelse(temperature_2m > freezing_threshold, freezing_threshold, temperature_2m),
               ymax = ifelse(temperature_2m > freezing_threshold, temperature_2m, freezing_threshold)
          )
     
     # Create a ggplot object for temperature trend
     rPlot <- ggplot(data, aes(x = common_date, y = temperature_2m)) +
          geom_rect(
               aes(
                    xmin = xmin,
                    xmax = xmax,
                    ymin = ymin,
                    ymax = ymax,
                    fill = fill_group
               ),
               color = 'black',
               alpha = 0.5
          ) +  # Column rectangles
     #     geom_line(color = "black", size = 0.5) +  # Line plot for temperature
          geom_hline(
               yintercept = freezing_threshold,
               linetype = "dashed",
               color = "lightblue",
               linewidth = 0.4
          ) +  # Horizontal line for freezing threshold
          labs(
               title = "Temperature Forecast",
               x = "",
               y = "° F"
          ) +  # Labels for the plot
          scale_x_datetime(
               labels = label_date("%l %p"),
               breaks = "6 hours",
               minor_breaks = "2 hours",
               guide = guide_axis(n.dodge = 1)
          ) +  # Format x-axis for time
          scale_y_continuous(sec.axis = dup_axis(name = "")) +  # Secondary y-axis
          scale_fill_manual(
               name = "Freezing Indicators",
               values = c(
                    "above freezing" = "green",
                    "below freezing" = "lightblue"
               )
          ) +  # Manual color scale
          facet_grid(~ month_day) +  # Facet by month_day
          ggplot_theming()  # Apply custom theme
     
     # Save the plot as a PNG file
     base_path <- "data/plots/"
     plot_path <- paste0(base_path, "ggTemperature.png")
     ggsave(plot_path, plot = rPlot, scale = 1.5)
     
     # Read the PNG file and display it
     img <- readPNG(plot_path)
     grid::grid.raster(img)
}

# Precipitation and Probability ----
plot_precipitation <- function(con) {
     # Query to fetch precipitation data
     query <- "
    SELECT
      precipitation_probability,
      precipitation,
      rain,
      snowfall,
      time_only,
      common_date,
      month_day
    FROM
      forecast_data
    WHERE
      latitude = 38.748;
  "
     
     data <- execute_query(con, query)  # Execute the query and get the data
     
     # Calculate scale factor for secondary y-axis
     scale_factor <- max(data$precipitation_probability, 
                         na.rm = TRUE) / max(data$rain, 
                                             data$snowfall, na.rm = TRUE)
     
     # Create a ggplot object for precipitation
     rPlot <- ggplot(data, aes(x = as.POSIXct(common_date))) +
          geom_area(
               aes(y = precipitation_probability, fill = "Precipitation Probability"),
               #position = "jitter"
               linewidth = 0.2
          ) +  # Area plot for precipitation probability
          geom_col(
               aes(y = rain * scale_factor, fill = "Rain (in.)"),
               #size = 1,
               alpha = 0.3,
               position = "stack",
               #linetype = "dashed"
          ) +  # Line plot for rain
          geom_col(
               aes(y = snowfall * scale_factor, fill = "Snowfall (in.)"),
               #size = 1,
               alpha = 0.3,
               position = "stack",
               #linetype = "dotted"
          ) +  # Line plot for snowfall
          scale_y_continuous(
               name = "Precipitation Probability (%)",
               sec.axis = sec_axis( ~ . / ifelse(
                    is.infinite(scale_factor), 1000, scale_factor
               ), name = "Rain / Snowfall (inches)")
          ) +  # Dual y-axes
          scale_x_datetime(
               labels = scales::date_format("%H:%M"),
               breaks = "6 hours",
               minor_breaks = "2 hour",
               guide = guide_axis(n.dodge = 1)
          ) +  # Format x-axis for time
          scale_fill_manual(
               name = "Weather Condition",
               values = c(
                    "Rain (in.)" = "skyblue",
                    "Snowfall (in.)" = "snow"
               )
          ) +  # Manual color scale for weather conditions
          scale_fill_manual(
               name = "Precipitation\n and Probability",  # Single legend title
               values = c(
                    "Rain (in.)" = "skyblue", 
                    "Snowfall (in.)" = "snow", 
                    "Precipitation Probability" = "gray20"
               )) +
               labs(title = "Precipitation Forecast", 
               x = "Time of Day", 
               y = "Precipitation Probability (%)") +  # Labels for the plot
          facet_grid(~ month_day) +  # Facet by month_day
          ggplot_theming(legend.position = "bottom", 
                         legend.text = element_text(size = rel(0.5)),
                         legend.title = element_text(size = rel(0.7)))  # Apply custom theme
     
     # Save the plot as a PNG file
     base_path <- "data/plots/"
     plot_path <- paste0(base_path, "ggPrecipitation.png")
     ggsave(plot_path, plot = rPlot, scale = 1.5)
     
     # Read the PNG file and display it
     img <- readPNG(plot_path)
     grid::grid.raster(img)
     
}

# OpenAir Wind Rose ----
plot_wind_rose <- function(con) {
     # Query to fetch wind data
     query <- "
    SELECT
      wind_speed_10m,
      wind_direction_10m,
      time_only,
      common_date,
      month_day
    FROM
      forecast_data
    WHERE
      latitude = 38.748;
  "
     
     data <- execute_query(con, query)  # Execute the query and get the data
     
     # Create a wind rose plot using the openair package
     windRose(
          data,
          ws = "wind_speed_10m",
          wd = "wind_direction_10m",
          breaks = 5,
          paddle = TRUE,
          cols = paletteer_d("ggsci::springfield_simpsons", n = 3),
          key.position = "left"
     )
}

# ggplot wind rose ----
plot_wind_rose_ggplot <- function(con) {
     # Query to fetch wind data
     query <- "
       SELECT
         wind_direction_10m,
         speed_bin,
         wind_direction_cardinal,
         direction_angle,
         time_only,
         month_day
       FROM forecast_data
       WHERE
         latitude = 38.748;
     "
     
     data <- execute_query(con, query)  # Execute the query and get the data
     
     # Summarize data for plotting
     plot_data <- data |>
          group_by(wind_direction_10m, speed_bin, month_day, time_only) |>
          summarise(count = n(), .groups = "drop")
     
     # Get unique days
     days <- unique(plot_data$month_day)
     
     walk(days, ~ {
          # Filter data for the current day
          day_data <- filter(plot_data, month_day == .x)
          
          # Create the wind rose plot for the current day
          day_plot <- ggplot(day_data,
                             aes(
                                  x = wind_direction_10m, y = count, fill = speed_bin
                             )) +
               geom_col(width = 15,
                        color = "black",
                        linewidth = 0.1) +
               coord_polar(start = 2 * pi) +
               scale_x_continuous(
                    limits = c(0, 360),
                    breaks = seq(22.5, 360, by = 22.5),
                    labels = c(' ', 'NE', ' ', 'E', ' ', 'SE', ' ', 'S', ' ','SW', ' ', 'W', ' ', 'NW', ' ', 'N')  # Cardinal labels
               ) +
               scale_fill_paletteer_d('ggprism::viridis') +
               labs(
                    title = paste("Wind Rose -", .x),
                    x = "Wind Direction (°)",
                    y = "",
                    fill = "Wind Speed (m/s)"
               ) +
               facet_wrap( ~ time_only) +  # Facet by hour
               ggplot_theming(
                    text = element_text(size = 8),
                    axis.text = element_text(
                         color = 'gray',
                         margin = margin(5, 5, 5, 5, "pt"),
                         size = rel(.8)
                    ),
                    axis.text.y = element_blank(),
                    strip.background = element_rect(fill = 'gray20'),
                    #strip.background.y = element_rect('#39D94E'),
                    strip.text = element_text(size = rel(0.8), 
                                              margin = margin(0, 0, 0, 0, "pt"),
                                              color = 'cornsilk'),
                    
               )
          
          # Save the plot for the current day
          ggsave(
               stringr::str_remove(paste0("data/plots/wind_rose_", .x, ".png"), " "),
               day_plot,
               #width = 24,
               #height = 20,
               scale = 2
          )
     })
     
     }


# Visibility geom_line ----
plot_visibility_line <- function(con) {
     # Query to fetch visibility data
     query <- "
    SELECT
      visibility,
      common_date,
      month_day
    FROM
      forecast_data
    WHERE
      latitude = 38.748;
  "
     
     data <- execute_query(con, query)  # Execute the query and get the data
     
     # Create a ggplot object for visibility trend
     rPlot <- ggplot(data, aes(x = common_date, y = visibility / 10 ^ 3)) +
          geom_line(color = "white", size = 0.5) +  # Line plot for visibility
          geom_point(color = "gray", alpha = 1) +  # Points for visibility
          labs(title = "Visibility Map", x = "Date", y = "Visibility (km)") +  # Labels for the plot
          scale_x_datetime(
               labels = scales::date_format("%H:%M"),
               breaks = "6 hours",
               minor_breaks = "2 hour",
               guide = guide_axis(n.dodge = 1)
          ) +  # Format x-axis for time
          facet_grid(~ month_day) +  # Facet by month_day
          ggplot_theming()  # Apply custom theme
     
     # Save the plot as a PNG file
     base_path <- "data/plots/"
     plot_path <- paste0(base_path, "ggVisibilityLine.png")
     ggsave(plot_path, plot = rPlot, scale = 1.5)
     
     # Read the PNG file and display it
     img <- readPNG(plot_path)
     grid::grid.raster(img)
     
}

# Visibility Non-Categorical Heat ----
plot_visibility_heat <- function(con) {
     # Query to fetch visibility data
     query <- "
    SELECT
      visibility,
      common_date,
      time_only,
      month_day
    FROM
      forecast_data
    WHERE
      latitude = 38.748;
  "
     
     data <- execute_query(con, query)  # Execute the query and get the data
     data$time_only <- as.POSIXct(data$time_only, format = "%H:%M:%S")
     
     # Create a ggplot object for visibility heatmap
     rPlot <- ggplot(data, aes(
          x = month_day,
          y = time_only,
          fill = visibility / 10 ^ 3
     )) +
          geom_tile() +  # Tile plot for visibility
          scale_fill_viridis_c(option = "magma") +  # Color scale for visibility
          labs(
               title = "Visibility (km)",
               x = "Time of Day",
               y = "Date",
               fill = "Visibility (km)"
          ) +  # Labels for the plot
          scale_y_datetime(
               date_labels = "%H:%M",
               date_breaks = "2 hours",
               sec.axis = dup_axis(name = "")
          ) +  # Format x-axis for time
          facet_grid(~ month_day, scales = "free") +
          ggplot_theming(legend.position = "right")  # Apply custom theme
     
     # Save the plot as a PNG file
     base_path <- "data/plots/"
     plot_path <- paste0(base_path, "ggVisibilityHeat.png")
     ggsave(plot_path, plot = rPlot, scale = 1.5)
     
     # Read the PNG file and display it
     img <- readPNG(plot_path)
     grid::grid.raster(img)
}

# Visibility Categorical Heat ----
plot_visibility_categorical_heat <- function(con) {
     # Query to fetch visibility data
     query <- "
    SELECT
      visibility,
      visibility_category,
      common_date,
      time_only,
      month_day
    FROM
      forecast_data
    WHERE
      latitude = 38.748;
  "
     
     data <- execute_query(con, query)  # Execute the query and get the data

          # Create a ggplot object for categorical visibility heatmap
     # Convert time_only to POSIXct for plotting
     data$time_only <- as.POSIXct(data$time_only, format = "%H:%M:%S")
     
     # Create a ggplot object for weather codes
     rPlot <- ggplot(data, aes(x = month_day, y = time_only, fill = visibility_category)) +
          geom_tile() +  # Tile plot for visibility categories
          scale_fill_manual(
               values = c(
                    "Clearest (>30 km)" = "green",
                    "Excellent (10-30 km)" = "darkgreen",
                    "Good (5-10 km)" = "yellow",
                    "Moderate (2-5 km)" = "orange",
                    "Low (1-2 km)" = "red",
                    "Fog/Haze (<1 km)" = "purple"
               )
          ) +  # Manual color scale for visibility categories
          labs(
               title = "Visibility Category Map",
               x = "Date",
               y = "Time of Day",
               fill = "Visibility Level"
          ) +  # Labels for the plot
          scale_y_datetime(
               date_labels = "%H:%M",
               date_breaks = "2 hours",
               sec.axis = dup_axis(name = "")
          ) +  # Format y-axis for time
          facet_grid(~ month_day, scales = "free") +
          ggplot_theming(legend.position = "right")  # Apply custom theme
     
     # Save the plot as a PNG file
     base_path <- "data/plots/"
     plot_path <- paste0(base_path, "ggVisibilityCat.png")
     ggsave(plot_path, plot = rPlot, scale = 1.5)
     
     # Read the PNG file and display it
     img <- readPNG(plot_path)
     grid::grid.raster(img)
}

# Weather Codes ----
plot_weather_codes <- function(con) {
     # Query to fetch weather codes and descriptions
     query <- "
    SELECT
      fd.weather_code,
      wc.Description AS description,
      fd.time_only,
      fd.month_day
    FROM
      forecast_data fd
    LEFT JOIN weather_codes wc ON wc.weather_code == fd.weather_code
    WHERE
      latitude = 38.748;
  "
     
     data <- execute_query(con, query)  # Execute the query and get the data
     
     # Convert time_only to POSIXct for plotting
     data$time_only <- as.POSIXct(data$time_only, format = "%H:%M:%S")
     
     # Create a ggplot object for weather codes
     rPlot <- ggplot(
          data, aes(x = month_day, y = time_only, fill = description)) +
          geom_tile(alpha = 0.5) +  # Tile plot for weather codes
          scale_fill_paletteer_d("khroma::land") +  # Color scale for weather codes
          scale_y_datetime(
               date_labels = "%H:%M",
               date_breaks = "2 hours",
               sec.axis = dup_axis(name = "")
          ) +  # Format y-axis for time
          labs(
               title = "Weather Code Map",
               x = "Day",
               y = "Time of Day",
               fill = "Weather Code"
          ) +  # Labels for the plot
          facet_grid(~ month_day, scales = "free") +
          ggplot_theming(legend.position = "right")  # Apply custom theme
     
     # Save the plot as a PNG file
     base_path <- "data/plots/"
     plot_path <- paste0(base_path, "ggWeatherCodes.png")
     ggsave(plot_path, plot = rPlot, scale = 1.5)
     
     # Read the PNG file and display it
     img <- readPNG(plot_path)
     grid::grid.raster(img)
}

display_a_plot <- function(plot_path) {
     # Read the PNG file and display it
     img <- readPNG(plot_path)
     grid::grid.raster(img)     
}
