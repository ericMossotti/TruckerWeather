import openmeteo_requests
import requests_cache
import pandas as pd
import polars as pl
from retry_requests import retry

def import_api_hourly_historical(
     latitude: float, longitude: float, 
     startDate: str, # e.g. "1974-01-01"
     endDate: str # e.g. "2024-12-31"
     ) -> pl.DataFrame:
     # Setup the Open-Meteo API client with cache and retry on error
     cache_session = requests_cache.CachedSession('.cache', expire_after = -1)
     retry_session = retry(cache_session, retries = 5, backoff_factor = 0.2)
     openmeteo = openmeteo_requests.Client(session = retry_session)
     
     # Make sure all required weather variables are listed here
     # The order of variables in hourly or daily is important to assign them correctly below
     url = "https://archive-api.open-meteo.com/v1/archive"
     params = {
     	"latitude": latitude,
     	"longitude": longitude,
     	"start_date": startDate,
     	"end_date": endDate,
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

     hourly_data = {"date": pd.date_range(
     	start = pd.to_datetime(hourly.Time(), unit = "s", utc = True),
     	end = pd.to_datetime(hourly.TimeEnd(), unit = "s", utc = True),
     	freq = pd.Timedelta(seconds = hourly.Interval()),
     	inclusive = "left"
     )}
     
     hourly_data["latitude"] = latitude
     hourly_data["longitude"] = longitude
     hourly_data["temperature_2m"] = hourly_temperature_2m
     hourly_data["precipitation"] = hourly_precipitation
     hourly_data["rain"] = hourly_rain
     hourly_data["snowfall"] = hourly_snowfall
     hourly_data["snow_depth"] = hourly_snow_depth
     hourly_data["visibility"] = hourly_visibility
     hourly_data["weather_code"] = hourly_weather_code
     hourly_data["wind_speed_10m"] = hourly_wind_speed_10m
     hourly_data["wind_direction_10m"] = hourly_wind_direction_10m
     
     return(pd.DataFrame(data = hourly_data))
