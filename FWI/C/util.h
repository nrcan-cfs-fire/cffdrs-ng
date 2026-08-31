/*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/.
*/


/*
Utility functions for FWI2025.
*/


/*** Include guard and import libraries **************************************/

#ifndef _UTIL_H
#define _UTIL_H

#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <time.h>


/*** Constants ***************************************************************/

#ifndef M_PI
#define M_PI 3.1415926535897932384626433
#endif


/*** Structures **************************************************************/

/**
 * Store a row of hourly weather data from the input file
 */
struct wx_hr
{
    // Location. "long" is reserved in C, unlike in Python and R.
    double lat;
    double lon;
    // Time. "timezone" is type double to allow for fractional UTC offsets.
    double timezone;
    int year;
    int mon;
    int day;
    int hour;
    struct tm timestamp;
    // Hourly weather.
    double temp;
    double rh;
    double ws;
    double rain;
    // Grassland parameters (optional inputs).
    double percent_cured;
    double grass_fuel_load;
    double solrad;
    // Drying day start and end times (calculated).
    double sunrise;
    double sunset;
};

/**
 * Store a row of daily weather data from the input file
 */
struct wx_day
{
    // Location. "long" is reserved in C, unlike in Python and R.
    double lat;
    double lon;
    // Time.
    int year;
    int mon;
    int day;
    // Daily weather.
    double temp;
    double rh;
    double ws;
    double rain;
};

/**
 * Store a row of daily minmax weather data from the input file
 */
struct wx_minmax
{
    // Location. "long" is reserved in C, unlike in Python and R.
    double lat;
    double lon;
    // Time. "timezone" is type double to allow for fractional UTC offsets.
    double timezone;
    int year;
    int mon;
    int day;
    struct tm date;
    // Daily minmax weather.
    double temp_min;
    double temp_max;
    double rh_min;
    double rh_max;
    double ws_min;
    double ws_max;
    double rain;
    // Drying day start and end times (calculated).
    double sunrise;
    double sunset;
};

/**
 * Flags for if an optional input is not provided and needs to be calculated
 */
struct need_optionals
{
    bool need_grass_fuel_load;
    bool need_percent_cured;
    bool need_solrad;
};

/**
 * Cumulative precipitation values
 */
struct rain_intercept
{
    double rain_total_prev;
    int drying_since_intercept;
};


/*** Forward declarations ****************************************************/

/**
* FWI2025 version (date) following changelog defined for print statements
*
* @return       String of date (YYYY-MM-DD)
*/
char* version();

/**
 * Check that a file header (columns) matches the given string or else exit
 * with extra consideration for three optional columns
 *
 * @param    input     Input file to check for string
 * @param    header    String to match
 * @param    f         flags struct for missing optional columns:
 *                         [grass_fuel_load, percent_cured, solrad]
 */
void check_header_FWI(FILE *input, const char *header,
                      struct need_optionals *f);

/**
 * Check that the file header (columns) matches the given string or else exit
 *
 * @param    input     Input file to check for string
 * @param    header    String to match
 */
void check_header_match(FILE *input, const char *header);

/**
 * Check that weather values are valid or else exit
 *
 * @param    temp    Temperature [°C]
 * @param    rh      Relative humidity [%]
 * @param    wind    Wind speed [km/h]
 * @param    rain    Precipitation [mm]
 */
void check_weather(double temp, double rh, double wind, double rain);

/**
 * Check that FWI input parameters are valid
 *
 * @param    temp               Temperature [°C]
 * @param    rh                 Relative humidity [%]
 * @param    wind               Wind speed [km/h]
 * @param    rain               Precipitation [mm]
 * @param    grass_fuel_load    Grass fuel load [kg/m^2]
 * @param    percent_cured      Grass curing [%]
 * @param    solrad             Solar radiation [kW/m^2]
 */
void check_inputs(double temp, double rh, double wind, double rain,
                  double grass_fuel_load, double percent_cured, double solrad);

/**
 * Read a row from an hourly weather CSV file
 */
int read_row_inputs(FILE *inp, struct wx_hr *r, struct need_optionals *f,
                    float def_grass_fuel_load, int def_mon_curing,
                    int def_day_curing);

/**
 * Read a row from a daily weather CSV file
 */
int read_row_daily(FILE *inp, struct wx_day *r);

/**
 * Read a row from a daily minmax weather CSV file
 */
int read_row_minmax(FILE *inp, struct wx_minmax *r);

/**
 * Maximum of two doubles
 * 
 * @param    x    First value
 * @param    y    Second value
 * @return        The larger value of the two
 */
double _max(double x, double y);

/**
 * Minimum of two doubles
 * 
 * @param    x    First value
 * @param    y    Second value
 * @return        The smaller value of the two
 */
double _min(double x, double y);

/**
 * Determine if a year is a leap year or not (common year)
 * 
 * @param    yr    Year
 * @return         Whether the year is a leap year
 */
bool is_leap(int yr);

/**
 * Find ordinal date of year, accounts for leap years
 *
 * @param    yr     Year
 * @param    mon    Month of year
 * @param    day    Day of month
 * @return          Day of year
 */
int julian(int yr, int mon, int day);

/**
 * Calculate hourly solar radiation using vapour pressure deficit
 *
 * @param    r    Hourly weather data structure
 * @return        Hourly solar radiation [kW/m^2]
 */
double solar_radiation(struct wx_hr *r);

// Alias for solar_radiation().
#define single_hour_solrad_estimation solar_radiation

/**
 * Calculate start and end times of the drying day based on location and date.
 * Based on generalized solar position equations
 *
 * @param    lat          Latitude [DD]
 * @param    lon          Longitude [DD]
 * @param    timezone     UTC offset
 * @param    timestamp    tm structure for year and yday (julian)
 * @param    suntime      double array [2] to output sunrise and sunset
 */
void sun_times(double lat, double lon, double timezone, struct tm timestamp,
               double *suntime);

// Alias for sun_times().
#define sunrise_sunset sun_times

/**
* Calculate default grassland curing values. Based on typical 10 day variations
* of grassland curing based on NDVI in the Boreal Plains region.
*
* @param    yr           Year
* @param    mon          Month of year
* @param    day          Day of month
* @param    start_mon    Start month of grassland fuel green-up (Default 3)
* @param    start_day    Start day of grassland fuel green-up (Default 12)
* @return                Grassland curing percentage [%]
*/
double grassland_curing(int yr, int mon, int day,
                        int start_mon, int start_day);

// Aliases for solar_radiation().
#define seasonal_curing grassland_curing
#define grass_curing grassland_curing

/**
 * Write an output row to a CSV file after rounding values
 */
int save_rounded(FILE *file, const char *fmt, const double value);

/**
 * Write an output row to a CSV file, similar to functionality in Python and R
 */
void save_csv(FILE *file, const char *fmt_all, ...);

#endif
