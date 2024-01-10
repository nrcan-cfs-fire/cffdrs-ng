#ifndef _UTIL_H
#define _UTIL_H
#include <math.h>
#include <stdio.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/**
 * A row from the input file for an hourly weather stream
 */
struct row
{
  double lat, lon;
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
/**
 * Read a row from an hourly weather stream file
 */
int read_row(FILE* inp, struct row* r);
/**
 * Read a row from a daily weather stream file
 */
int read_row_daily(FILE* inp, struct row_daily* r);
/**
 * Read a row from a min/max weather stream file
 */
int read_row_minmax(FILE* inp, struct row_minmax* r);
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
 * Find solar radiation at a give time and place
 *
 * @param lat               Latitude (degrees)
 * @param lon               Longitude (degrees)
 * @param mon               Month
 * @param day               Day of month
 * @param hour              Hour of day
 * @param timezone          Offset from GMT in hours
 * @param[out] sunrise      Sunrise in decimal hours (in the local time zone specified)
 * @param[out] sunset       Sunset in decimal hours (in the local time zone specified)
 * @return                  Solar radiation (kW/m^2)
 */
double sun(double lat, double lon, int mon, int day, int hour, double timezone, double* sunrise, double* sunset);
/**
 * Find solar radiation at a give time and place
 *
 * @param lat               Latitude (degrees)
 * @param lon               Longitude (degrees)
 * @param jd                Day of year
 * @param hour              Hour of day
 * @param timezone          Offset from GMT in hours
 * @param[out] sunrise      Sunrise in decimal hours (in the local time zone specified)
 * @param[out] sunset       Sunset in decimal hours (in the local time zone specified)
 * @return                  Solar radiation (kW/m^2)
 */
double sun_julian(double lat, double lon, int jd, int hour, double timezone, double* sunrise, double* sunset);
/**
 * Find day of year. Does not properly deal with leap years.
 *
 * @param mon         Month
 * @param day         Day of month
 * @return            Day of year
 */
int julian(int mon, int day);
/**
 * Check that the file stream matches the given string and exit if not
 *
 * @param input       Input file to check for string
 * @param header      String to match
 */
void check_header(FILE* input, const char* header);
/**
 * Check that weather parameters are valid
 *
 * @param temp        Temperature (Celcius)
 * @param rh          Relative humidity (percent, 0-100)
 * @param wind        Wind speed (km/h)
 * @param rain        Rain (mm)
 */
void check_inputs(double temp, double rh, double wind, double rain);

double seasonal_curing(int julian_date);

/* C90 round() only rounds to int but want digits */
double _round(double x, int digits);

/* C90 max() also causing problems */
double _max(double x, double y);

/* C90 min() also causing problems */
double _min(double x, double y);
#endif
