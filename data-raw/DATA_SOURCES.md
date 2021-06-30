# Sources of Data
## Sea Level Rise in providence, Maine

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

**8418150_meantrend.csv**  
Data downloaded directly from NOAA Tides and Currents (Using "Export to CSV"
button).
(https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=8454000) 
March 31, 2021 by Curtis C. Bohlen

D
**providence_tides_monthly.csv**  
Data downloaded directly from NOAA API using a simple python script
[(here)](providence_tide_gage_monthly.py).
Details on the API are available from the
[NOAA web page](https://tidesandcurrents.noaa.gov/api/).

Data is highly correlated, but not identical with the previous data set. After
review, this data was not used in analysis or preparation of graphics.

**providence_tides_hourly.csv**  
Data downloaded directly from NOAA API using a simple python script
[(here)](providence_tide_gage_hourly.py).
Details on the API are available from the
[NOAA web page](https://tidesandcurrents.noaa.gov/api/).

This data was used to generate daily counts of exceedences above flood levels,
where flood levels are defined as observations above providence's highest 
Astronomical Tide (HAT), at 6.52 ft MLLW, or 1.987 m MLLW.

**providence_tides_hourly_predicts.csv**  
Data downloaded directly from NOAA API using a simple python script
[(here)](providence_tide_gage_hourly_predicts.py).
Details on the API are available from the
[NOAA web page](https://tidesandcurrents.noaa.gov/api/).


