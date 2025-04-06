import pandas as pd  # For generating the date range
import requests_cache  # For caching API requests to reduce load and improve performance
from retry_requests import retry  # For retrying failed API requests
import openmeteo_requests  # For interacting with the Open-Meteo API
from datetime import datetime, timezone  # For handling date and time

def import_api_hourly(latitude: float, longitude: float) -> pd.DataFrame:
     """
     Fetches hourly weather data from the Open-Meteo API for the given latitude and longitude.
     
     Parameters:
        latitude (float): The latitude of the location for which weather data is requested.
        longitude (float): The longitude of the location for which weather data is requested.
     
     Returns:
        pd.DataFrame: A Pandas DataFrame containing hourly weather data for the specified location.
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
        #"forecast_days": 1,  # Number of forecast days (1 day)
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
     
     # Process hourly data. The order of variables needs to be the same as requested.
     hourly = response.Hourly()
     hourly_temperature_2m = hourly.Variables(0).ValuesAsNumpy()
     hourly_precipitation_probability = hourly.Variables(1).ValuesAsNumpy()
     hourly_precipitation = hourly.Variables(2).ValuesAsNumpy()
     hourly_rain = hourly.Variables(3).ValuesAsNumpy()
     hourly_showers = hourly.Variables(4).ValuesAsNumpy()
     hourly_snowfall = hourly.Variables(5).ValuesAsNumpy()
     hourly_snow_depth = hourly.Variables(6).ValuesAsNumpy()
     hourly_weather_code = hourly.Variables(7).ValuesAsNumpy()
     hourly_visibility = hourly.Variables(8).ValuesAsNumpy()
     hourly_wind_speed_10m = hourly.Variables(9).ValuesAsNumpy()
     hourly_wind_direction_10m = hourly.Variables(10).ValuesAsNumpy()
     
     hourly_data = {"date": pd.date_range(
     	start = pd.to_datetime(hourly.Time(), unit = "s", utc = True),
     	end = pd.to_datetime(hourly.TimeEnd(), unit = "s", utc = True),
     	freq = pd.Timedelta(seconds = hourly.Interval()),
     	inclusive = "left"
     )}
     
     hourly_data["latitude"] = latitude
     hourly_data["longitude"] = longitude
     hourly_data["temperature_2m"] = hourly_temperature_2m
     hourly_data["precipitation_probability"] = hourly_precipitation_probability
     hourly_data["precipitation"] = hourly_precipitation
     hourly_data["rain"] = hourly_rain
     hourly_data["showers"] = hourly_showers
     hourly_data["snowfall"] = hourly_snowfall
     hourly_data["snow_depth"] = hourly_snow_depth
     hourly_data["weather_code"] = hourly_weather_code
     hourly_data["visibility"] = hourly_visibility
     hourly_data["wind_speed_10m"] = hourly_wind_speed_10m
     hourly_data["wind_direction_10m"] = hourly_wind_direction_10m
     
     #data = pd.DataFrame(data = hourly_data)
     
     return(pd.DataFrame(data = hourly_data))
