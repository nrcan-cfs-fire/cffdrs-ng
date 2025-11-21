#ifndef _UTIL_H
#define _UTIL_H
#include <math.h>
#include <stdio.h>
#include <stdbool.h>
#include <time.h>

#ifndef M_PI
#define M_PI 3.1415926535897932384626433
#endif

// Start of grassland fuels curing (default March 12th)
static const int MON_CURING = 3;
static const int DAY_CURING = 12;

/**
 * A row from the input file for an hourly weather stream
 */
struct row
{
  double lat, lon, timezone;
  int year, mon, day, hour;
  double temp, rh, ws, rain;
  /* Either need solar radiation to be included, or calculated */
  double solrad;
  /* derived from lat/lon/date/timezone */
  double sunrise, sunset;
  /* grass curing (%) [0-100]*/
  double percent_cured;
  /* grass fuel load (kg/m^2) */
  double grass_fuel_load;
  // timestamp (including julian/yday)
  struct tm timestamp;
};

/**
 * A row from the input file for a daily weather stream
 */
struct row_daily
{
  double lat, lon;
  int year, mon, day;
  double temp, rh, wind, rain;
};

/**
 * A row from the input file for a min/max weather stream
 */
struct row_minmax
{
  double lat, lon;
  int year, mon, day;
  double temp_min, temp_max, rh_min, rh_max, ws_min, ws_max, rain;
};

struct flags{
  bool solrad_flag;
  bool percent_cured_flag;
};

/**
 * Read a row from an hourly weather stream file
 */
int read_row(FILE *inp, struct row *r);

/**
 * Read a row from an hourly fwi inputs stream file
 */
int read_row_inputs(FILE *inp, struct row *r, struct flags *f);

/**
 * Read a row from a daily weather stream file
 */
int read_row_daily(FILE *inp, struct row_daily *r);

/**
 * Read a row from a min/max weather stream file
 */
int read_row_minmax(FILE *inp, struct row_minmax *r);

/**
 * Find specific humidity
 *
 * @param temp        Temperature (Celcius)
 * @param rh          Relative humidity (percent, 0-100)
 * @return            Specific humidity (g/kg)
 */
double findQ(double temp, double rh);

/**
 * Find relative humidity
 *
 * @param q           Specific humidity (g/kg)
 * @param temp        Temperature (Celcius)
 * @return            Relative humidity (percent, 0-100)
 */
double findrh(double q, double temp);

/**
 * Find if a year is a leap year or not
 * 
 * @param yr    Year
 * @return      Boolean whether year is a leap year or not
 */
bool is_leap(int yr);

/**
 * Find day of year
 *
 * @param yr          Year
 * @param mon         Month of year
 * @param day         Day of month
 * @return            Day of year
 */
int julian(int yr, int mon, int day);

/**
 * Calculate hourly surface open-site shortwave radiation in kW/m^2.
 *
 * @param r                 Structure of data row (required columns:
 *                            lat, lon, timezone, yr, hr, timestamp, temp, rh)
 * @return                  Hourly solar radiation (kW/m^2)
 */
double single_hour_solrad_estimation(struct row *r);

/**
 * Find sunrise and sunset for a given date and location.
 *
 * @param r             Structure of data row (required columns:
 *                        lat, lon, timezone, yr, timestamp)
 * @return              Updates .sunrise and .sunset members of input structure
 */
void sunrise_sunset(struct row *r);

/**
 * Check that the file stream matches the given string and exit if not
 *
 * @param input       Input file to check for string
 * @param header      String to match
 */
void check_header(FILE *input, const char *header, struct flags *f);

void check_header_legacy(FILE *input, const char *header);

/**
 * Check that weather parameters are valid
 *
 * @param temp        Temperature (Celcius)
 * @param rh          Relative humidity (percent, 0-100)
 * @param wind        Wind speed (km/h)
 * @param rain        Rain (mm)
 */
void check_weather(double temp, double rh, double wind, double rain);

/**
 * Check that FWI input parameters are valid
 *
 * @param temp            Temperature (Celcius)
 * @param rh              Relative humidity (percent, 0-100)
 * @param wind            Wind speed (km/h)
 * @param rain            Rain (mm)
 * @param grass_fuel_load Grass fuel load ((kg/m^2))
 * @param percent_cured   Grass curing (percent, 0-100)
 * @param solrad          Solar radiation (kW/m^2)
 */
void check_inputs(double temp, double rh, double wind, double rain, double grass_fuel_load, double percent_cured, double solrad);

/**
* Set default percent_cured values based off annual variation in Boreal Plains region
*
* @param yr             Year
* @param mon            Month of year
* @param day            Day of month
* @param start_mon      Month of grassland fuel green up start (Boreal Plains Mar 12)
* @param start_day      Day of grassland fuel green up start (Boreal Plains Mar 12)
* @return               percent_cured [%], percent of grassland fuel that is cured
*/
double seasonal_curing(int yr, int mon, int day, int start_mon, int start_day);

/* C90 max() also causing problems */
double _max(double x, double y);

/* C90 min() also causing problems */
double _min(double x, double y);

void save_csv(FILE *file, const char *fmt_all, ...);

/* HACK: format and then output to prevent -0.0 */
int save_rounded(FILE *file, const char *fmt, const double value);
#endif
