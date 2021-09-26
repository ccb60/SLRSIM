# SLRSIM
A lightweight package to access and analyze data on changes in sea level.

The package includes functions to:

1.  Access metadata for NOAA water level station, through NOAA APIs. 
Metadata functions provide access selected information about specified stations
including:

       - station datums, 
   
       - the station's time offset from UCT,
   
       - the tidal epoch,
   
       - the span of available water level data (first and last years),
   
       - NOAA's estimates of long-term sea level change.

2.  Access data from NOAA's APIs to facilitate analysis of sea level rise,
including hourly observed and predicted water levels. To keep the package light,
data access functions gather only hourly data.

3. Analyze historical rates of sea level rise.  Functions include tools to 
calculate a long-term rate of change of sea level (mimicking NOAA's analysis, 
in part), and to examine whether the rate of sea level change is itself 
changing.

4. Examine historical frequency of days that exhibit water levels that exceed a 
flood threshold ("Flood events".) Although sea level rise at many NOAA stations 
over the  past 100 years or so has been moderate (~ 8 inches), even those 
relatively mild changes in sea level have led to some startling changes in 
"nuisance" flood events.

5.  Forecast how frequency of flood events may change under specified sea level 
rise scenarios. 
