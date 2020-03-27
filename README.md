# wmh-analysis

## Rationale

This repository contains R code for the paper:

> Vansteelant WMG, Klaassen R, Strandberg R, Janssens K, T´Jollyn F, Bouten W, Koks BJ & Anselin A (In press) Western Marsh Harriers _Circus aeruginosus_ from nearby breeding areas migrate along comparable loops but on contrasting schedules in the West African-Eurasian flyway. Journal of Ornithology.

To study their migration patterns, the paper makes use of GPS tracking data of 3 breeding populations of _Circus aeruginosus_:

1. Near the Belgian-Dutch border in the provinces East Flanders (Belgium) and Zeeland (the Netherlands): 4 tracked birds, 
data collected by [INBO](www.inbo.be/en), data available at [Anselin et al. (2019)](http://doi.org/10.5281/zenodo.3532941).
2. The province of Groningen (the Netherlands): 2 tracked birds, data collected by the 
[Dutch Montagu’s Harrier Foundation](http://werkgroepgrauwekiekendief.nl/), data available at [Koks et al. (2019)](http://doi.org/10.5281/zenodo.3552508).
3. Southern Sweden: 4 tracked birds, data collected by [Lund University](https://www.lunduniversity.lu.se/).

## Repo structure

```
|-- data                <- Data used in this analysis
    |--maps             <- Maps downloaded by the scripts, to be ignored by git
|-- reports             <- Output generated by the scripts, to be ignored by git
    |-- figures         <- Figures generated by the scripts, to be ignored by git
    |-- tables          <- Tables (.csv) generated by the scripts, to be ignored by git
|-- src                 <- Scripts generated for this analysis
|-- gitignore           <- Files and directories to be ignored by git
|-- LICENSE             <- Repository license
|-- README.md           <- Description of this repository
|-- wmh-analysis.Rproj  <- R project file for this repository
```

## Getting started

### Packages

In order to run the scripts properly, run [`src/p1_loadPackages.R`](src/p1_loadPackages.R) to install and load all required packages.

### Data

#### Project data (accessible in this repo)

- `annotation.csv`: annotated version of the Belgian and Dutch dataset. Wouter Vansteelant determined the start and end dates of migratory and non-migratory periods through expert interpretation of tracks in Google Earth.
    - Bird name [`name`]
	- Migratory cycle [`cycle`] (winter, spring, summer, autumn + year)
	- Starting date [`date`]
    - Behaviour type [`type`] (breeding, dead, migration, move, pre-migration, winter.stage1, winter.stage2, winter.stage3)
    - Criteria for selection in analysis ([`complete`], [`use.date`])
- `annotation-swedes.csv`: annotated version of the Swedish dataset. Wouter Vansteelant determined the start and end dates of migratory and non-migratory periods through expert interpretation of tracks in Google Earth.
    - Bird name [`name`]
	- Migratory cycle [`cycle`] (winter, spring, summer, autumn + year)
	- Starting date [`date`]
    - Behaviour type [`type`] (breeding, dead, migration, move, pre-migration, winter.stage1, winter.stage2)
    - Criteria for selection in analysis ([`complete`], [`use.date`])
- `MH-resampled-23092017.csv`: Belgian and Dutch tracking data downloaded from the UvA-BiTS database on September 23rd, 2017
	- Device and individual code ([`dev`], [`name`]
	- Coordinates ([`lat`], [`lon`], [`alt`])
	- Date and time
		- Date time [`dt`]
		- Year [`yr`]
		- [`season`] (spring/autumn)
		- Julian time [`julian`]
		- day of year [`yday`]
	- Data provider [`origin`]
	- Derived codes ([`indday`] (name_julian), [`migr`] (name_yr_season))
	- Calculated parameters using functions in script [`pt2pt_fxns`]
		- Distance [`dist`]
		- Duration [`dur`]
		- Speed ([`spd`] = [`dist`]/[`dur`])		 
- `swedes.csv`: Swedish tracking data
- `weather.csv`: daily tailwind conditions calculated using the method of [Klaassen et al. (2010)](https://doi.org/10.1111/j.1600-048X.2010.05058.x). u- and v-wind components were obtained from the NOAA NCEP global atmospheric reanalysis model.

#### External GIS data (available through web links)

- `ne_50m_admin_0_countries`: shapefile containing country boundaries, source: Natural Earth ( https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/)
- `alt_30s_bil`: raster file containing a DEM of the world, source: worldclim, version 1.4 (http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/alt_30s_bil.zip)

## Scripts

### Main scripts

The scripts are supposed to be run consecutively.

- `p2a_analyses_read_processed_data.R`: read data files
- `p2b_analyses_annotate&clean_processed_data.R`: clean data
- `p3_Fig1A_Lat-vs-yday.R`: make figure 1A of the paper
- `p3a_Fig1B&C.R`: make figures 1B and 1C
- `p3b_FigS2_Wintering-fin.R`: make figure S2
- `p4_wind-fin.R`: read wind data and calculate mean daily wind components
- `p5_CompileData-PerTrip-PerID_TableS1&S9.R`: make table S1 and S9
- `p6_Figure2&TablesS5-S6.R`: make table S5 and S6
- `p6_TableS2.R`: make table S2
- `p6a_TableS3_Long-diff-pops.R`: make table S3
- `p6b_TablesS7&8_WindSelectivity_glmms&posthoc.R`: make table S7 and S8
- `pt2pt_fxns.R`: functions to compute distance, duration and speed

### Side scripts

These scripts are helper scripts automatically loaded when running the main scripts.

- `sidescript_readSwedes.R`: read Swedish dataset, make selection and manipulate data to match with mh-resampled-23092017
- `sidescript_CalcDailyStats.R`: compute daily statistics
- `sidescript_CalcStopovers.R`: compute coordinates and duration of stopovers
- `sidescript_readDEM.R`: read and manipulate DEM for making maps

## Acknowledgements

This work makes use of data and/or infrastructure provided by INBO and funded by Research Foundation - Flanders (FWO) as part of the Belgian contribution to [LifeWatch](https://lifewatch.be).
