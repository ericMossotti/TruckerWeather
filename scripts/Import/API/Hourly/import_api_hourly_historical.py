import openmeteo_requests
import requests_cache
import pandas as pd
import polars as pl
from retry_requests import retry

def import_api_hourly_historical(latitude: float, longitude: float) -> pl.DataFrame:
     # Setup the Open-Meteo API client with cache and retry on error
     cache_session = requests_cache.CachedSession('.cache', expire_after = -1)
     retry_session = retry(cache_session, retries = 5, backoff_factor = 0.2)
     openmeteo = openmeteo_requests.Client(session = retry_session)
     
     # Make sure all required weather variables are listed here
     # The order of variables in hourly or daily is important to assign them correctly below
     url = "https://archive-api.open-meteo.com/v1/archive"
     params = {
     	"latitude": 38.748,
     	"longitude": -90.439,
     	"start_date": "1974-01-01",
     	"end_date": "2024-12-31",
     	"hourly": [
     	     "temperature_2m", 
     	     "precipitation", 
     	     "rain", 
     	     "snowfall", 
     	     "snow_depth", 
     	     "visibility",
     	     "weather_code", 
     	     "wind_speed_10m", 
     	     "wind_direction_10m"],
     	"temperature_unit": "fahrenheit",
     	"wind_speed_unit": "mph",
     	"precipitation_unit": "inch",
     	"timezone": "America/Chicago",
     	"models": "best_match"
     }
     
     responses = openmeteo.weather_api(url, params = params)
     
     # Process first location. Add a for-loop for multiple locations or weather models
     response = responses[0]
     print(f"Coordinates {response.Latitude()}°N {response.Longitude()}°E")
     print(f"Elevation {response.Elevation()} m asl")
     print(f"Timezone {response.Timezone()} {response.TimezoneAbbreviation()}")
     print(f"Timezone difference to GMT+0 {response.UtcOffsetSeconds()} s")
     
     # Process hourly data. The order of variables needs to be the same as requested.
     hourly = response.Hourly()
     hourly_temperature_2m = hourly.Variables(0).ValuesAsNumpy()
     hourly_precipitation = hourly.Variables(1).ValuesAsNumpy()
     hourly_rain = hourly.Variables(2).ValuesAsNumpy()
     hourly_snowfall = hourly.Variables(3).ValuesAsNumpy()
     hourly_snow_depth = hourly.Variables(4).ValuesAsNumpy()
     hourly_visibility = hourly.Variables(5).ValuesAsNumpy()
     hourly_weather_code = hourly.Variables(6).ValuesAsNumpy()
     hourly_wind_speed_10m = hourly.Variables(7).ValuesAsNumpy()
     hourly_wind_direction_10m = hourly.Variables(8).ValuesAsNumpy()


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


     hourly_data = {
     "date": date_range,
     "temperature_2m": hourly_temperature_2m,
     "precipitation": hourly_precipitation,
     "rain": hourly_rain,
    # "showers": hourly_showers,
     "snowfall": hourly_snowfall,
     "snow_depth": hourly_snow_depth,
     "weather_code": hourly_weather_code,
     "visibility": hourly_visibility,
     "wind_speed_10m": hourly_wind_speed_10m,
     "wind_direction_10m": hourly_wind_direction_10m
     }

     return pl.DataFrame(hourly_data)
