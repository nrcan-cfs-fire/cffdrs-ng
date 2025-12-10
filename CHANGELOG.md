# Changelog

All notable changes to this project will be documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/). Changes under "to be released" are intended for the next update. Programming languages affected are specified when applicable and omitted when changes affect the GitHub repository or all three languages at once (*C*, *Python*, and *R*).

## 2025-12-10

### Added
- Option when running `hFWI()` to have a transition from matted to standing grassland fuel (default, for regions with winter snowfall), or stick with standing instead (*Python* and *R*)
- Option when running `hFWI()` to not split by year (for regions with seasonal data that includes Dec 31-Jan 1) and run each station's data as one continuous block (*Python* and *R*)
- Standard datasets for PRF 2007
    - Standard hourly weather data input for C (**PRF2007_hourly_wx_C-format.csv**)
    - Standard `hFWI()` outputs (**PRF2007_standard_hourly_FWI.csv**)
    - Standard `generate_daily_summaries()` outputs (**PRF2007_standard_daily_summaries.csv**)
- New print message when running hourly FWI that shows startup values

### Changed
- Leap years are properly accounted for in the transition between matted and standing GFMC, default `percent_cured` values, and in `get_sunlight()`
- Transition between matted and standing GFMC and start of grassland fuel green up now defined by calendar date instead of Julian (ordinal) date

| Transition                          | Old Value (Julian date) | New Value (Calendar date) |
| ----------------------------------- | ----------------------- | ------------------------- |
| End of matted grassland fuel        | 180                     | June 30th                 |
| Start of standing grassland fuel    | 181                     | July 1st                  |
| Start of green up (`percent_cured`) | 71 (implicit)           | March 12th                |

- Reorganized **NG_FWI** variable definitions in header (*Python* and *R*)
- Most startup arguments when running **NG_FWI_main.c** or **daily_summaries.c** by command line are optional (and reorganized) matching R version (*C*)
- DMC and DC functions take and output moisture content (%) instead of moisture codes (*C*)
- Updated moisture code to moisture content (and vice versa) conversion functions (*C*)

### Removed
- Deprecated `is_sequential()` function (*R*)
- Unused `dmc_before_rain` and `dc_before_rain` arguments for **NG_FWI** (*C*)
- Requirement that the `timezone` argument be between -2 and -9 (*C*)

### Fixed
- All outputs in C version (NGFWI, daily summaries, make hourly, make minmax) now match Python and R versions (*C*)
- `is_sequential_hours()` and `is_sequential_days()` differentiates by their respective time units (*R*)
- `daily_summaries()` accounts for leap years properly (pseudo date differentiates leap days) (*Python* and *R*)
- `grass_fuel_load` actually uses the default 0.35 kg/m^2 if not provided (*C*)

## 2025-10-31 🎃

### Changed
- Updated **NG_FWI** and **daily_summaries** output column order (*C*)
- Updated **NG_FWI** and **daily_summaries** to round outputs to 4 decimal places (*C*)

### Fixed
- GFMC, DMC, and DC calculations to match other versions (*C*)
- Unallocated memory access in **daily_summaries** if a day does not have enough hours (*C*)

### Removed
- Some print statements for debugging (*C*)

## 2025-10-20

### Added
- `timezone` argument in `minmax_to_hourly()` function defaults to using a column in input, or can still be specified (*Python* and *R*)
- `round_out` argument in `minmax_to_hourly()` and `daily_to_minmax()` functions defaults to rounding weather variables to 4 decimal places (*Python* and *R*)
- `daily_to_minmax()` and `minmax_to_hourly()` check for required input columns (*Python* and *R*)

### Changed
- `minmax_to_hourly()` can accept one row (one day) inputs (*Python* and *R*)
- `daily_to_minmax()` and `minmax_to_hourly()` outputs preserve data frame/data table class from input (*R*)

### Removed
- Requirement that the `timezone` argument in `minmax_to_hourly()` be between -2 and -9 (*Python*)

### Fixed
- Running **NG_FWI**, **daily_summaries**, **make_hourly**, and **make_minmax** by command line properly converts `timezone`, `round_out`, and `reset_hr` arguments to corresponding number types (*Python* and *R*)
- `minmax_to_hourly()` and `get_sunlight()` calculations allow for different data frame column name cases (*Python* and *R*)
- `make_minmax` by command line doesn't use removed `save_csv()` function (*Python* and *R*)
- `is_sequential_days()` and `is_sequential_hours()` works on a data frame copy and allows for different data frame column name cases (*Python* and *R*)
- `hFWI()` output preserves data table class if input was a data table (*R*)

## 2025-10-10

### Fixed
- Print statement in `generate_daily_summaries()` can handle numeric IDs (*Python*)
- Generating `date` column in `hFWI()` converts input values to integer first (*Python*)

## 2025-10-08

### Added
- Restored `timezone` argument in `hFWI()` to create/override `timezone` column in input data frame. Default is to use a provided `timezone` column (*Python* and *R*)

### Changed
- Updated daytime check in DMC and DC calculation for northern regions with midnight sun (*Python* and *R*)
- Calculated `solrad` values less than 1e-4 are always automatically set to 0 (*Python* and *R*)

## 2025-10-02

### Added
- Preliminary calculation variables for **NG_FWI.c** are included in output and inputs for "live hourly runs" (continuation runs) (*C*)
- **daily_summaries.c** file to generate daily summary reports similar to Python and R (*C*)
- **NG_FWI_main.c** file to hold the `main()` function for **NG_FWI.c** (*C*)
- **NG_FWI_main_historical.c** file to keep the old usage and arguments for **NG_FWI.c** (*C*)

### Changed
- `solrad` and `percent_cured` columns are calculated if not provided (*C*)
- Updated daytime check in DMC and DC calculation for northern regions with midnight sun (*C*)
- Solar radiation calculation in **util.c** to only need each hour's weather data instead of a whole day (*C*)
- Parameters used to create typical diurnal curve for `make_hourly()` (see similar 2025-09-26 changes for parameter values) (*C*)
- Various changes to header checking for compatibility between files (*C*)

## 2025-09-26

### Changed
- Parameters used to create typical diurnal curve for `make_hourly()` (*Python* and *R*)

| Parameter     | Old Value | New Value |
| ------------- | --------- | --------- |
| Temp $\alpha$ | 0.2       | 0.0       |
| Temp $\beta$  | 2.0       | 2.75      |
| Temp $\gamma$ | -2.9      | -1.9      |
| RH $\alpha$   | 0.4       | 0.25      |
| RH $\beta$    | 1.9       | 2.75      |
| RH $\gamma$   | -2.9      | -2.0      |
| WS $\alpha$   | 1.2       | 1.0       |
| WS $\beta$    | 1.7       | 1.5       |
| WS $\gamma$   | -1.5      | -1.3      |

### Fixed
- Data inputs to `hFWI()` that have multiple years for the same ID (station) are properly split by year (*R*)

## 2025-09-10

### Added
- Preliminary calculation variables for `hFWI()` function are included in output and are optional inputs for "live hourly runs" (*Python* and *R*)
- `generate_daily_summaries()` function has an optional `reset_hr` argument that can be used to change the 24 hour window boundary from 05:00 to another hour. This is intended for input datasets that use a non-local time (*Python* and *R*)
- `hFWI()` and `generate_daily_summaries()` functions now automatically round output values to 4 decimal places, which can be changed via an optional `round_out` argument (*Python* and *R*)
- `hFWI()` and `generate_daily_summaries()` pass through extra columns in input data frame (*Python* and *R*)
- Help information for running **hFWI.py** by command line (*Python*)
- **SECURITY.md** file
### Changed
- GitHub repository organized into folders (for structure overview see README)
- DMC calculation in `hFWI()` to better respond to long and heavy rain events (*Python* and *R*)
- Solar radiation calculation in `getSunlight()` to only need each hour's weather data instead of a whole day (*Python* and *R*)
- `timezone` input in `hFWI()` moved from function argument to column in hourly data frame (*Python* and *R*)
- Output data frame column name changes for `generate_daily_summaries()` (*Python* and *R*): 

| Old Column Name       | New Column Name |
| --------------------- | --------------- |
| `peak_time`           | `peak_hr`       |
| `wind_speed_smoothed` | `ws_smooth`     |
| `peak_isi_smoothed`   | `isi_smooth`    |
| `peak_gsi_smoothed`   | `gsi_smooth`    |

- Function name changes in **util**:

| Old Function Name     | New Function Name       | Programming Language    |
| --------------------- | ----------------------- | ----------------------- |
| `getSunlight()`       | `get_sunlight()`        | *Python* and *R*        |
| `isSequential()`      | `is_sequential()`       | *R*                     |
| `isSequentialDays()`  | `is_sequential_days()`  | *R* (to match *Python*) |
| `isSequentialHours()` | `is_sequential_hours()` | *R* (to match *Python*) |
| `findQ()`             | `find_q()`              | *R* (to match *Python*) |
| `findrh()`            | `find_rh()`             | *R* (to match *Python*) |

- Preprocessing and data checking of input data frame for `hFWI()` function standardized between programming languages (*Python* and *R*)
- Function to convert between DMC and duff moisture content changed to be exact inverses without rounding (*Python* and *R*)
- All conversions between corresponding moisture codes and moisture contents have a function (*Python* and *R*)
- Standardized print statements when running `hFWI()` and `generate_daily_summaries()` with an optional `silent` argument (*Python* and *R*)
- Running **NG_FWI** and **daily_summaries** by command line updated to match new `hFWI()` and `generate_daily_summaries()` arguments and options (*Python* and *R*)
- Preliminary calculation variables for `hFWI()` function condensed to `mcgfmc_matted`, `mcgfmc_standing`, `dmc_before_rain`, `dc_before_rain`, `prec_cumulative`, and `canopy_drying` (*Python* and *R*)
### Removed
- Alternative data frame column names for `hFWI()` function. Year, month, wind speed, and precipitation now must be `yr`, `mon`, `ws`, or `prec` respectively (*Python* and *R*)
- Unused functions in **util** (*Python* and *R*)
- **make_inputs**, **make_daily**, and **old_cffdrs** code files
- **test.sh** file
- **test_hffmc.csv** and **wx_hourly.csv** data files. The remaining PRF dataset is used in a tutorial found on the website
- **CMakeLists.txt** file
### Fixed
- `generate_daily_summaries()` retrieves `isi_smooth` and `gsi_smooth` at the proper (peak) hour (*Python* and *R*)
- `generate_daily_summaries()` calculates `gsi_smooth` using mcgfmc instead of GFMC (*Python* and *R*)
- `getSunlight()` function doesn't create a new `solrad` column when the `get_solrad` argument and calculation is turned off (*Python* and *R*)

## 2025-08-22
### Added
- Starting up changelog file, leaving out past changes
### Changed
- DMC and DC calculation functions only include relevant inputs (*Python* and *R*)
- DMC and DC wetting functions and function variables' names (*Python* and *R*)
### Deprecated
- **make_inputs** as it is exceeded by `hFWI()` which performs the same jobs and will continue to be updated (*Python* and *R*)
### Removed
- Debugging print statements in `standing_grass_spread_ROS()` function (*R*)
- Incorrect and unused `dmc_to_moisture_percent()` function in **util.r** (*R*)
