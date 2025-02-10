def daily_api():
     
     # Setup the Open-Meteo API client with cache and retry on error
     cache_session = requests_cache.CachedSession('.cache', expire_after = 3600)
     retry_session = retry(cache_session, retries = 5, backoff_factor = 0.2)
     openmeteo = openmeteo_requests.Client(session = retry_session)
     
     # Make sure all required weather variables are listed here
     # The order of variables in hourly or daily is important to assign them correctly below
     url = "https://api.open-meteo.com/v1/forecast"
     params = {
     	"latitude": 38.748,
     	"longitude": -90.439,
     	"daily": 
     	     ["weather_code", 
     	     "daylight_duration", 
     	     "precipitation_sum", 
     	     "rain_sum", 
     	     "showers_sum", 
     	     "snowfall_sum", 
     	     "precipitation_hours", 
     	     "precipitation_probability_max", 
     	     "wind_speed_10m_max", 
     	     "wind_gusts_10m_max", 
     	     "wind_direction_10m_dominant"],
     	"temperature_unit": "fahrenheit",
     	"wind_speed_unit": "mph",
     	"precipitation_unit": "inch",
     	"timezone": "America/Chicago",
     	"models": "best_match"
     }
     responses = openmeteo.weather_api(url, params=params)
     
     # Process first location. Add a for-loop for multiple locations or weather models
     response = responses[0]
     print(f"Coordinates {response.Latitude()}°N {response.Longitude()}°E")
     print(f"Elevation {response.Elevation()} m asl")
     print(f"Timezone {response.Timezone()} {response.TimezoneAbbreviation()}")
     print(f"Timezone difference to GMT+0 {response.UtcOffsetSeconds()} s")
     
     # Process daily data. The order of variables needs to be the same as requested.
     daily = response.Daily()
     daily_weather_code = daily.Variables(0).ValuesAsNumpy()
     daily_daylight_duration = daily.Variables(1).ValuesAsNumpy()
     daily_precipitation_sum = daily.Variables(2).ValuesAsNumpy()
     daily_rain_sum = daily.Variables(3).ValuesAsNumpy()
     daily_showers_sum = daily.Variables(4).ValuesAsNumpy()
     daily_snowfall_sum = daily.Variables(5).ValuesAsNumpy()
     daily_precipitation_hours = daily.Variables(6).ValuesAsNumpy()
     daily_precipitation_probability_max = daily.Variables(7).ValuesAsNumpy()
     daily_wind_speed_10m_max = daily.Variables(8).ValuesAsNumpy()
     daily_wind_gusts_10m_max = daily.Variables(9).ValuesAsNumpy()
     daily_wind_direction_10m_dominant = daily.Variables(10).ValuesAsNumpy()
     
     daily_data = {"date": pd.date_range(
     	start = pd.to_datetime(daily.Time(), unit = "s", utc = True),
     	end = pd.to_datetime(daily.TimeEnd(), unit = "s", utc = True),
     	freq = pd.Timedelta(seconds = daily.Interval()),
     	inclusive = "left"
     )}
     
     daily_data["weather_code"] = daily_weather_code
     daily_data["daylight_duration"] = daily_daylight_duration
     daily_data["precipitation_sum"] = daily_precipitation_sum
     daily_data["rain_sum"] = daily_rain_sum
     daily_data["showers_sum"] = daily_showers_sum
     daily_data["snowfall_sum"] = daily_snowfall_sum
     daily_data["precipitation_hours"] = daily_precipitation_hours
     daily_data["precipitation_probability_max"] = daily_precipitation_probability_max
     daily_data["wind_speed_10m_max"] = daily_wind_speed_10m_max
     daily_data["wind_gusts_10m_max"] = daily_wind_gusts_10m_max
     daily_data["wind_direction_10m_dominant"] = daily_wind_direction_10m_dominant
     
     daily_dataframe = pd.DataFrame(data = daily_data)
     
     return(daily_dataframe)
