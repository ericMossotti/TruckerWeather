


# Load necessary libraries
library(DBI)
library(ggplot2)
library(scales)
library(paletteer)
library(openair)
source("./scripts/Output/Plots/plot_output.R")
source("./scripts/Setup/Enum/create_enum_and_associate.R")


# Helper function to execute a query and return the result
execute_query <- function(con, query) {
     dbGetQuery(con, query)
}

# 1 Day Temperature Trend ----
plot_temperature_trend <- function(con, freezing_threshold = 32) {
     query <- "
    SELECT
      temperature_2m,
      strftime(date, '%H:%M:%S') AS time_only,
      strptime('1970-01-01 ' || strftime(date, '%H:%M:%S'), '%Y-%m-%d %H:%M:%S') AS common_date,
      strftime(date, '%b %d') AS day
    FROM
      hourly;
  "
     
     data <- execute_query(con, query)
     
     rPlot <- ggplot(data, aes(x = common_date, y = temperature_2m)) +
          geom_line(color = "black", size = 0.5) +
          geom_hline(
               yintercept = freezing_threshold,
               linetype = "dashed",
               color = "lightblue",
               linewidth = 0.4
          ) +
          geom_ribbon(
               aes(
                    ymin = freezing_threshold,
                    ymax = ifelse(
                         temperature_2m > freezing_threshold,
                         temperature_2m,
                         freezing_threshold
                    )
               ),
               fill = "green",
               alpha = 0.5,
               na.rm = TRUE
          ) +
          geom_ribbon(
               aes(
                    ymin = ifelse(
                         temperature_2m < freezing_threshold,
                         temperature_2m,
                         freezing_threshold
                    ),
                    ymax = freezing_threshold
               ),
               fill = "lightblue",
               alpha = 0.5,
               na.rm = TRUE
          ) +
          labs(
               title = "Temperature Forecast",
               x = "",
               y = "° F",
               caption = "Green fill indicates temperatures above freezing; light blue fill indicates below freezing."
          ) +
          scale_x_datetime(
               labels = label_date("%l %p"),
               breaks = "7 day",
               minor_breaks = "1 hour",
               guide = guide_axis(n.dodge = 2)
          ) +
          facet_grid(. ~ day) +
          ggplot_theming()
     
     base_path <- "data/plots/"
     plot_path <- paste0(base_path, "ggTemperature.png")
     ggsave(plot_path, plot = rPlot, scale = 1.5)
     
     # Read the PNG file
     img <- readPNG(plot_path)
     # Display the image
     grid::grid.raster(img)
}

# Precipitation and Probability ----
plot_precipitation <- function(con) {
     query <- "
    SELECT
      precipitation_probability,
      precipitation,
      rain,
      snowfall,
      strftime(date, '%H:%M:%S') AS time_only,
      strptime('1970-01-01 ' || strftime(date, '%H:%M:%S'), '%Y-%m-%d %H:%M:%S') AS common_date,
      strftime(date, '%b %d') AS day
    FROM
      hourly;
  "
     
     data <- execute_query(con, query)
     
     scale_factor <- max(data$precipitation_probability, na.rm = TRUE) / max(data$rain, data$snowfall, na.rm = TRUE)
     
     rPlot <- ggplot(data, aes(x = as.POSIXct(common_date))) +
          geom_area(
               aes(y = precipitation_probability, fill = "Precipitation Probability"),
               linewidth = 0.2
          ) +
          geom_line(
               aes(y = rain * scale_factor, color = "Rain (inches)"),
               size = 1,
               linetype = "dashed"
          ) +
          geom_line(
               aes(y = snowfall * scale_factor, color = "Snowfall (inches)"),
               size = 1,
               linetype = "dotted"
          ) +
          scale_y_continuous(
               name = "Precipitation Probability (%)",
               sec.axis = sec_axis( ~ . / ifelse(
                    is.infinite(scale_factor), 1000, scale_factor
               ), name = "Rain / Snowfall (inches)")
          ) +
          scale_x_datetime(
               labels = scales::date_format("%H:%M"),
               breaks = "6 hours",
               minor_breaks = "2 hour",
               guide = guide_axis(n.dodge = 1)
          ) +
          scale_color_manual(
               name = "Weather Condition",
               values = c(
                    "Rain (inches)" = "skyblue",
                    "Snowfall (inches)" = "snow"
               )
          ) +
          scale_fill_manual(name = "Chance of %",
                            values = c("Precipitation Probability" = "gray20")) +
          labs(title = "Precipitation Forecast", x = "Time of Day", y = "Precipitation Probability (%)") +
          facet_grid(~ day) +
          ggplot_theming()
     
     base_path <- "data/plots/"
     plot_path <- paste0(base_path, "ggPrecipitation.png")
     ggsave(plot_path, plot = rPlot, scale = 1.5)
     
     # Read the PNG file
     #readPNG(plot_path) |>
          # Display the image
      #    grid::grid.raster()
     
     # Read the PNG file
     img <- readPNG(plot_path)
     # Display the image
     grid::grid.raster(img)
     
}

# OpenAir Wind Rose ----
plot_wind_rose <- function(con) {
     query <- "
    SELECT
      wind_speed_10m,
      wind_direction_10m,
      strftime(date, '%H:%M:%S') AS time_only,
      strptime('1970-01-01 ' || strftime(date, '%H:%M:%S'), '%Y-%m-%d %H:%M:%S') AS common_date,
      strftime(date, '%b %d') AS day
    FROM
      hourly;
  "
     
     data <- execute_query(con, query)
     
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

# ggplot Wind Rose ----
plot_wind_rose_ggplot <- function(con) {
     query <- "
    SELECT
      wind_speed_10m,
      wind_direction_10m,
      strftime(date, '%H:%M:%S') AS time_only,
      strptime('1970-01-01 ' || strftime(date, '%H:%M:%S'), '%Y-%m-%d %H:%M:%S') AS common_date,
      strftime(date, '%b %d') AS day
    FROM
      hourly;
  "
     
     data <- execute_query(con, query)
     
     data <- data |>
          mutate(speed_bin = cut(
               wind_speed_10m,
               breaks = c(0, 2, 4, 6, 8, 10, Inf),
               labels = c("0-2", "2-4", "4-6", "6-8", "8-10", "10+")
          ))
     
     rPlot <- ggplot(data, aes(x = wind_direction_10m, fill = speed_bin)) +
          geom_histogram(binwidth = 10,
                         color = "black",
                         position = "stack") +
          coord_polar(start = 2 * pi) +
          scale_x_continuous(limits = c(0, 360),
                             breaks = seq(0, 360, by = 45)) +
          labs(
               title = "Wind Rose",
               x = "Wind Direction (°)",
               y = "Frequency",
               fill = "Wind Speed (m/s)"
          ) +
          ggplot_theming()
     
     base_path <- "data/plots/"
     plot_path <- paste0(base_path, "ggWindRose.png")
     ggsave(plot_path, plot = rPlot, scale = 1.5)
     
     # Read the PNG file
     img <- readPNG(plot_path)
     # Display the image
     grid::grid.raster(img)
     
}

# Visibility geom_line ----
plot_visibility_line <- function(con) {
     query <- "
    SELECT
      visibility,
      strptime('1970-01-01 ' || strftime(date, '%H:%M:%S'), '%Y-%m-%d %H:%M:%S') AS common_date,
      strftime(date, '%b %d') AS day
    FROM
      hourly;
  "
     
     data <- execute_query(con, query)
     
     rPlot <- ggplot(data, aes(x = common_date, y = visibility / 10 ^ 3)) +
          geom_line(color = "white", size = 0.5) +
          geom_point(color = "gray", alpha = 1) +
          labs(title = "Visibility Map", x = "Date", y = "Visibility (km)") +
          scale_x_datetime(
               labels = scales::date_format("%H:%M"),
               breaks = "6 hours",
               minor_breaks = "2 hour",
               guide = guide_axis(n.dodge = 1)
          ) +
          facet_grid(~ day) +
          ggplot_theming()
     
     base_path <- "data/plots/"
     plot_path <- paste0(base_path, "ggVisibilityLine.png")
     ggsave(plot_path, plot = rPlot, scale = 1.5)
     
     # Read the PNG file
     img <- readPNG(plot_path)
     # Display the image
     grid::grid.raster(img)
     
}

# Visibility Non-Categorical Heat ----
plot_visibility_heat <- function(con) {
     query <- "
    SELECT
      visibility,
      strptime('1970-01-01 ' || strftime(date, '%H:%M:%S'), '%Y-%m-%d %H:%M:%S') AS common_date,
      strftime(date, '%b %d') AS day
    FROM
      hourly;
  "
     
     data <- execute_query(con, query)
     
     rPlot <- ggplot(data, aes(
          x = common_date,
          y = day,
          fill = visibility / 10 ^ 3
     )) +
          geom_tile() +
          scale_fill_viridis_c(option = "magma") +
          labs(
               title = "Visibility (km)",
               x = "Time of Day",
               y = "Date",
               fill = "Visibility (km)"
          ) +
          scale_x_datetime(
               labels = scales::date_format("%H:%M"),
               breaks = "6 hours",
               minor_breaks = "2 hour",
               guide = guide_axis(n.dodge = 1)
          ) +
          ggplot_theming()
     
     base_path <- "data/plots/"
     plot_path <- paste0(base_path, "ggVisibilityHeat.png")
     ggsave(plot_path, plot = rPlot, scale = 1.5)
     
     # Read the PNG file
     img <- readPNG(plot_path)
     # Display the image
     grid::grid.raster(img)
     
}

# Visibility Categorical Heat ----
plot_visibility_categorical_heat <- function(con) {
     query <- "
    SELECT
      visibility,
      strptime('1970-01-01 ' || strftime(date, '%H:%M:%S'), '%Y-%m-%d %H:%M:%S') AS common_date,
      strftime(date, '%b %d') AS day
    FROM
      hourly;
  "
     
     data <- execute_query(con, query)
     
     data <- data |>
          mutate(
               visibility_category = case_when(
                    visibility > 30 * 10 ^ 3 ~ "Clearest (>30 km)",
                    visibility > 10 * 10 ^ 3 ~ "Excellent (10-30 km)",
                    visibility > 5 * 10 ^ 3 ~ "Good (5-10 km)",
                    visibility > 2 * 10 ^ 3 ~ "Moderate (2-5 km)",
                    visibility > 1 * 10 ^ 3 ~ "Low (1-2 km)",
                    TRUE ~ "Fog/Haze (<1 km)"
               )
          )
     
     rPlot <- ggplot(data,
                     aes(x = common_date, y = day, fill = visibility_category)) +
          geom_tile() +
          scale_fill_manual(
               values = c(
                    "Clearest (>30 km)" = "green",
                    "Excellent (10-30 km)" = "darkgreen",
                    "Good (5-10 km)" = "yellow",
                    "Moderate (2-5 km)" = "orange",
                    "Low (1-2 km)" = "red",
                    "Fog/Haze (<1 km)" = "purple"
               )
          ) +
          labs(
               title = "Visibility Category Map",
               x = "Time of Day",
               y = "Date",
               fill = "Visibility Level"
          ) +
          scale_x_datetime(
               labels = scales::date_format("%H:%M"),
               breaks = "6 hours",
               minor_breaks = "2 hour",
               guide = guide_axis(n.dodge = 1)
          ) +
          ggplot_theming()
     
     base_path <- "data/plots/"
     plot_path <- paste0(base_path, "ggVisibilityCat.png")
     ggsave(plot_path, plot = rPlot, scale = 1.5)
     
     # Read the PNG file
     img <- readPNG(plot_path)
     # Display the image
     grid::grid.raster(img)
     
}

# Weather Codes ----
plot_weather_codes <- function(con) {
     codes <- c(
          '0',
          '1',
          '2',
          '3',
          '45',
          '48',
          '51',
          '53',
          '55',
          '56',
          '57',
          '61',
          '63',
          '65',
          '66',
          '67',
          '71',
          '73',
          '75',
          '77',
          '80',
          '81',
          '82',
          '85',
          '86',
          '95',
          '96',
          '99'
     )
     descriptions <- c(
          'Clear sky',
          'Mainly clear',
          'Partly cloudy',
          'Overcast',
          'Fog',
          'Depositing rime fog',
          'Drizzle: light',
          'Drizzle: moderate',
          'Drizzle: dense',
          'Freezing drizzle: light',
          'Freezing drizzle: dense',
          'Rain: slight',
          'Rain: moderate',
          'Rain: heavy',
          'Freezing rain: light',
          'Freezing rain: heavy',
          'Snow fall: slight',
          'Snow fall: moderate',
          'Snow fall: heavy',
          'Snow grains',
          'Rain showers: slight',
          'Rain showers: moderate',
          'Rain showers: violent',
          'Snow showers: slight',
          'Snow showers: heavy',
          'Thunderstorm: slight or moderate',
          'Thunderstorm with slight hail',
          'Thunderstorm with heavy hail'
     )
     
     result <- create_enum_and_associate(con, "WeatherCode", codes, descriptions)
     
     query <- "
    SELECT
      hr.weather_code::INTEGER::TEXT::WeatherCode AS weather_code,
      wc.Description AS description,
      strftime(hr.date, '%H:%M:%S') AS time_only,
      strftime(hr.date, '%b %d') AS day
    FROM
      hourly hr
    LEFT JOIN WeatherCode wc ON wc.Code == hr.weather_code;
  "
     
     data <- execute_query(con, query)
     
     data$time_only <- as.POSIXct(data$time_only, format = "%H:%M:%S")
     
     rPlot <- ggplot(data, aes(x = day, y = time_only, fill = description)) +
          geom_tile(alpha = 0.3) +
          scale_fill_paletteer_d("khroma::highcontrast") +
          scale_y_datetime(
               date_labels = "%H:%M",
               date_breaks = "2 hours",
               sec.axis = dup_axis(name = "")
          ) +
          labs(
               title = "Weather Code Map",
               x = "Day",
               y = "Time of Day",
               fill = "Weather Code"
          ) +
          ggplot_theming()
     
     base_path <- "data/plots/"
     plot_path <- paste0(base_path, "ggWeatherCodes.png")
     ggsave(plot_path, plot = rPlot, scale = 1.5)
     
     # Read the PNG file
     img <- readPNG(plot_path)
     # Display the image
     grid::grid.raster(img)
     
}
