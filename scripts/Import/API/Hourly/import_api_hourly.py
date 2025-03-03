import polars as pl  # For data manipulation and DataFrame creation
import pandas as pd  # For generating the date range
import requests_cache  # For caching API requests to reduce load and improve performance
from retry_requests import retry  # For retrying failed API requests
import openmeteo_requests  # For interacting with the Open-Meteo API
from datetime import datetime, timezone  # For handling date and time

def import_api_hourly(latitude: float, longitude: float) -> pl.DataFrame:
    """
    Fetches hourly weather data from the Open-Meteo API for the given latitude and longitude.

    Parameters:
        latitude (float): The latitude of the location for which weather data is requested.
        longitude (float): The longitude of the location for which weather data is requested.

    Returns:
        pl.DataFrame: A Polars DataFrame containing hourly weather data for the specified location.
    """
    
    # Setup the Open-Meteo API client with cache and retry on error
    # Caching reduces the number of API calls by storing responses for 1 hour (3600 seconds)
    cache_session = requests_cache.CachedSession('.cache', expire_after = 3600)
    
    # Retry mechanism: retry up to 5 times with exponential backoff if the request fails
    retry_session = retry(cache_session, retries = 5, backoff_factor = 0.2)
    
    # Initialize the Open-Meteo API client with the cached and retry-enabled session
    openmeteo = openmeteo_requests.Client(session = retry_session)
    
    # Define the API endpoint and parameters for the weather data request
    url = "https://api.open-meteo.com/v1/forecast"
    params = {
        "latitude": latitude,  # Latitude of the location
        "longitude": longitude,  # Longitude of the location
        "hourly": [  # List of hourly weather variables to fetch
            "temperature_2m",  # Temperature at 2 meters above ground
            "precipitation_probability",  # Probability of precipitation
            "precipitation",  # Total precipitation
            "rain",  # Rain amount
            "showers",  # Showers amount
            "snowfall",  # Snowfall amount
            "snow_depth",  # Snow depth
            "weather_code",  # Weather condition code
            "visibility",  # Visibility
            "wind_speed_10m",  # Wind speed at 10 meters above ground
            "wind_direction_10m"  # Wind direction at 10 meters above ground
        ],
        "temperature_unit": "fahrenheit",  # Temperature unit (Fahrenheit)
        "wind_speed_unit": "mph",  # Wind speed unit (miles per hour)
        "precipitation_unit": "inch",  # Precipitation unit (inches)
        "timezone": "America/Chicago",  # Timezone for the data
        "forecast_days": 1,  # Number of forecast days (1 day)
        "past_hours": 6,  # Include past 6 hours of data
        "forecast_hours": 24,  # Include next 24 hours of forecast
        "models": "best_match"  # Use the best matching weather model
    }
    
    # Make the API request to fetch weather data
    responses = openmeteo.weather_api(url, params = params)
    
    # Process the first location in the response (only one location is requested)
    response = responses[0]
    
    # Print location and timezone information for debugging
    print(f"Coordinates {response.Latitude()}°N {response.Longitude()}°E")
    print(f"Elevation {response.Elevation()} m asl")
    print(f"Timezone {response.Timezone()} {response.TimezoneAbbreviation()}")
    print(f"Timezone difference to GMT+0 {response.UtcOffsetSeconds()} s")
    
    # Process hourly data from the API response
    hourly = response.Hourly()
    
    # Extract each hourly weather variable from the response
    hourly_temperature_2m = hourly.Variables(0).ValuesAsNumpy()  # Temperature at 2m
    hourly_precipitation_probability = hourly.Variables(1).ValuesAsNumpy()  # Precipitation probability
    hourly_precipitation = hourly.Variables(2).ValuesAsNumpy()  # Total precipitation
    hourly_rain = hourly.Variables(3).ValuesAsNumpy()  # Rain amount
    hourly_showers = hourly.Variables(4).ValuesAsNumpy()  # Showers amount
    hourly_snowfall = hourly.Variables(5).ValuesAsNumpy()  # Snowfall amount
    hourly_snow_depth = hourly.Variables(6).ValuesAsNumpy()  # Snow depth
    hourly_weather_code = hourly.Variables(7).ValuesAsNumpy()  # Weather condition code
    hourly_visibility = hourly.Variables(8).ValuesAsNumpy()  # Visibility
    hourly_wind_speed_10m = hourly.Variables(9).ValuesAsNumpy()  # Wind speed at 10m
    hourly_wind_direction_10m = hourly.Variables(10).ValuesAsNumpy()  # Wind direction at 10m
    
    # Create a time range for the hourly data using Pandas (supports hourly intervals)
    start_time = datetime.fromtimestamp(hourly.Time(), tz = timezone.utc)  # Start time of the data
    end_time = datetime.fromtimestamp(hourly.TimeEnd(), tz = timezone.utc)  # End time of the data
    interval = pd.Timedelta(seconds=hourly.Interval())  # Time interval between data points
    
    # Generate the date range using Pandas
    date_range = pd.date_range(
        start = start_time,
        end = end_time - interval,  # Subtract interval to match the API's inclusive="left" behavior
        freq = interval
    )
    
    # Create a dictionary to store the hourly weather data
    hourly_data = {
        "date": date_range,
        "temperature_2m": hourly_temperature_2m,
        "precipitation_probability": hourly_precipitation_probability,
        "precipitation": hourly_precipitation,
        "rain": hourly_rain,
        "showers": hourly_showers,
        "snowfall": hourly_snowfall,
        "snow_depth": hourly_snow_depth,
        "weather_code": hourly_weather_code,
        "visibility": hourly_visibility,
        "wind_speed_10m": hourly_wind_speed_10m,
        "wind_direction_10m": hourly_wind_direction_10m
    }
    
    # Convert the dictionary to a Polars DataFrame and return it
    return pl.DataFrame(hourly_data)
