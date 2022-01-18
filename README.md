# SLRSIM
A lightweight package to access and analyze data on changes in sea level

## Outline of Contents
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

This Package is in draft form, and in (slow) active development. We will be
periodically updating the repository, as we add features and catch bugs.

## Installing `SLRSIM`
### Install `devtools`
If you have not already installed the `devtools`package, you will need to do so.

On the R Command Line, enter

`install.packages("devtools")`

Alternatively, you can use the GUI menus in RStudio:

`Tools -> Install Packages...`

Then select the `devtools` package from  the enormous list 
of packages available on CRAN, and the package will be 
installed.

### Install Package
To install a package of interest from GitHub, you need to specify both the
"Author" and the "Package".  If you have a URL to the GitHub Repo, the form of
that URL is `https://github.com/<Author>/<Package>`, so you have all the
information you need.

```	
library(devtools)
install_github("ccb60/SLRSIM")
```

Or, alternatively, if you want to avoid polluting your search path with an
unnecessary environment associated with `devtools`, you can accomplish the same
thing with the following.

```
devtools::install_github("ccb60/SLRSIM")
```

## Casco Bay Estaury Partnership
This package was developed for the Casco Bay Estuary Partnership, as an offshoot
of developing our most recent "State of Casco Bay" report, where we conducted
analyses of sea level rise for Portland, Maine. 

For more on the Casco Bay Estuary Partnership, visit our 
[webpage](https://cascobayestuary.org).  The State of Casco Bay Report
itself can be found 
[here](https://www.cascobayestuary.org/strategic-planning/state-of-casco-bay/).

After a couple of requests to conduct similar analyses for other National
Estuary Programs, we decided to prepare a package to simplify those analyses.

