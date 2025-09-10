# Changelog

All notable changes to this project will be documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/). Changes under "unreleased" are intended for the next update. Programming languages affected are specified when applicable and omitted when changes affect the GitHub repository or all three languages at once (*C*, *Python*, and *R*).

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
