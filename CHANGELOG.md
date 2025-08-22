# Changelog

All notable changes to this project will be documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/). Changes under "unreleased" are intended for the next update. Programming languages affected are specified when applicable and omitted when changes affect the GitHub repository or all three languages at once (*C*, *Python*, and *R*).

## [To be released] - yyyy-mm-dd

### Added
- Preliminary calculation variables for `hFWI()` function are included in output and are optional inputs for "live hourly runs" (*Python* and *R*)
- `generate_daily_summaries()` function has an optional `reset_hr` argument that can be used to change the 24 hour window boundary from 05:00 to another hour. This is intended for input datasets that use a non-local time (*Python* and *R*)
- `hFWI()` and `generate_daily_summaries()` functions now automatically round output values to 4 decimal places, which can be changed via an optional `round_out` argument (*Python* and *R*)
- `hFWI()` and `generate_daily_summaries()` functions pass through extra columns in input data frame (*Python* and *R*)
- Help information for running **hFWI.py** by command line with `-h` flag (*Python*)
### Changed
- DMC calculation in `hFWI()` function to better respond to long and heavy rain events (*Python* and *R*)
- Solar radiation calculation in `getSunlight()` function to only need each hour's weather data instead of a whole day (*Python* and *R*)
- Output data frame column name changes for `generate_daily_summaries()` function (daily report):

| Old                   | New          |
| --------------------- | ------------ |
| `peak_time`           | `peak_hr`    |
| `wind_speed_smoothed` | `ws_smooth`  |
| `peak_isi_smoothed`   | `isi_smooth` |
| `peak_gsi_smoothed`   | `gsi_smooth` |

- Preprocessing and data checking of input data frame for `hFWI()` function standardized between programming languages (*Python* and *R*)
- Function to convert between DMC and duff moisture content changed to be exact inverses without rounding (*Python* and *R*)
- All conversions between corresponding moisture codes and moisture contents have a function (*Python* and *R*)
- Standardized print statements when running `hFWI()` and `generate_daily_summaries()` functions with an optional `silent` argument (*Python* and *R*)
- Running **NG_FWI** and **daily_summaries** by command line updated to match new `hFWI()` and `generate_daily_summaries()` functions' arguments and options (*Python* and *R*)
- Preliminary calculation variables for `hFWI()` function condensed to `mcgfmc_matted`, `mcgfmc_standing`, `dmc_before_rain`, `dc_before_rain`, `prec_cumulative`, and `canopy_drying` (*Python* and *R*)
### Removed
- Alternative data frame column names for `hFWI()` function. Year, month, wind speed, and precipitation now must be `yr`, `mon`, `ws`, or `prec` respectively (*Python* and *R*)
### Fixed
- `generate_daily_summaries()` function retrieves `isi_smooth` and `gsi_smooth` at the proper (peak) hour (*Python* and *R*)
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