#ifndef _UTIL_H
#define _UTIL_H
#include <stdio.h>
#include <math.h>
#include <float.h>
#include <stdlib.h>
#include <string.h>

/**
 * Mathematical constant PI
 */
const float pi;

/**
 * Find specific humidity
 *
 * @param temp        Temperature (Celcius)
 * @param rh          Relative humidity (percent, 0-100)
 * @return            Specific humidity (g/kg)
 */
float findQ(float temp, float rh);
/**
 * Find relative humidity
 *
 * @param q           Specific humidity (g/kg)
 * @param temp        Temperature (Celcius)
 * @return            Relative humidity (percent, 0-100)
 */
float findrh(float q, float temp);
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
float sun(float lat, float lon, int mon, int day, int hour, int timezone, float *sunrise, float *sunset);
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
float sun_julian(float lat, float lon, int jd, int hour, int timezone, float *sunrise, float *sunset);
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
#endif
