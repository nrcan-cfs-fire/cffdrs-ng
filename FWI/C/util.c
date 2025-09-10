#include "util.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>

/* abs() isn't working either */
double _abs(double x)
{
  /* work around negative 0 as well */
  return (0 == x) ? 0.0 : ((x < 0) ? -x : x);
}

/* copysign() doesn't seem to work for some reason */
double _copysign(double x, double y)
{
  /* >=0 to account for negative zero */
  return _abs(x) * ((y >= 0) ? 1 : -1);
}

double _max(double x, double y)
{
  return (x > y ? x : y);
}

double _min(double x, double y)
{
  return (x < y ? x : y);
}

double findQ(double temp, double rh)
{
  /* find absolute humidity */
  double svp = 6.108 * exp(17.27 * temp / (temp + 237.3));
  double vp = svp * rh / 100.0;
  return (217 * vp / (273.17 + temp));
}

double findrh(double q, double temp)
{
  double cur_vp = (273.17 + temp) * q / 217;
  return (100 * cur_vp / (6.108 * exp(17.27 * temp / (temp + 237.3))));
}

/*
 * Calculate Hargreaves hourly surface open-site shortwave radiation in kW/m^2.
 */
void solar_radiation(double lat, double lon, int mon, int day, double timezone, double temp_range, struct double_24hr *solrad)
{
  int jd = julian(mon, day);
  solar_radiation_julian(lat, lon, jd, timezone, temp_range, solrad);
}

void solar_radiation_julian(double lat, double lon, int jd, double timezone, double temp_range, struct double_24hr *solrad)
{
  /*
    Completely disconnect from sunrise/sunset code so nothing changes there
     if we change how solar radiation is calculated
  */
  double dechour = 12.0;
  double fracyear = 2.0 * M_PI / 365.0 * ((float)(jd)-1.0 + ((float)(dechour)-12.0) / 24.0);
  double eqtime = 229.18 * (0.000075 + 0.001868 * cos(fracyear) - 0.032077 * sin(fracyear) - 0.014615 * cos(2.0 * fracyear) - 0.040849 * sin(2.0 * fracyear));
  double decl = 0.006918 - 0.399912 * cos(fracyear) + 0.070257 * sin(fracyear) - 0.006758 * cos(fracyear * 2.0) + 0.000907 * sin(2.0 * fracyear) - 0.002697 * cos(3.0 * fracyear) + 0.00148 * sin(3.0 * fracyear);
  double timeoffset = eqtime + 4 * lon - 60 * timezone;
  struct double_24hr cos_zenith;
  double sum_24hr_solrad_ext = 0;
  double sum_24hr_cos_zenith = 0;
  /*
    calculating solar radiation using Hargraeves model suggested at:
    (https://github.com/derekvanderkampcfs/open_solar_model/tree/V1#conclusions)
  */
  for (int h = 0; h < 24; ++h)
  {
    double tst = (float)h * 60.0 + timeoffset;
    double hourangle = tst / 4 - 180;
    /* Extraterrestrial solar radiation in kW/m^2 */
    cos_zenith.hour[h] = cos(_min(M_PI / 2,
                                  acos(sin(lat * M_PI / 180) * sin(decl) + cos(lat * M_PI / 180) * cos(decl) * cos(hourangle * M_PI / 180))));
    sum_24hr_cos_zenith += cos_zenith.hour[h];
    double solrad_ext = 1.367 * cos_zenith.hour[h];
    /* Daily total of Extra. Solar Rad in kJ/m^2/day */
    sum_24hr_solrad_ext += (solrad_ext * 3600);
  }
  /* Daily surface Solar Rad in kJ/m^2/day */
  double sum_24hr_solrad = 0.11 * sum_24hr_solrad_ext * (pow(temp_range, 0.59));
  /* Hargreaves hourly surface solar rad in kW/m^2 */
  for (int h = 0; h < 24; ++h)
  {
    solrad->hour[h] = _max(
        0,
        cos_zenith.hour[h] / sum_24hr_cos_zenith * sum_24hr_solrad / 3600);
  }
}

/*
 * Find sunrise and sunset for a given date and location.
 */
void sunrise_sunset(double lat, double lon, int mon, int day, double timezone, double *sunrise, double *sunset)
{
  int jd = julian(mon, day);
  sunrise_sunset_julian(lat, lon, jd, timezone, sunrise, sunset);
}

void sunrise_sunset_julian(double lat, double lon, int jd, double timezone, double *sunrise, double *sunset)
{
  /*
  this routine approximately calcualtes sunrise and sunset and daylength
  Really any routine like this could be used,  some are more precise than others.

  bmw
  */
  double dechour = 12.0;
  double fracyear = 2.0 * M_PI / 365.0 * ((float)(jd)-1.0 + (dechour - 12.0) / 24.0);
  double eqtime = 229.18 * (0.000075 + 0.001868 * cos(fracyear) - 0.032077 * sin(fracyear) - 0.014615 * cos(2.0 * fracyear) - 0.040849 * sin(2.0 * fracyear));
  double decl = 0.006918 - 0.399912 * cos(fracyear) + 0.070257 * sin(fracyear) - 0.006758 * cos(fracyear * 2.0) + 0.000907 * sin(2.0 * fracyear) - 0.002697 * cos(3.0 * fracyear) + 0.00148 * sin(3.0 * fracyear);
  double timeoffset = eqtime + 4 * lon - 60 * timezone;
  double zenith = 90.833 * M_PI / 180.0;
  /*
   * FIX: is this some kind of approximation that can be wrong?
   *       breaks with (67.1520291504819, -132.37538245496188)
   */
  double x_tmp = cos(zenith) / (cos(lat * M_PI / 180.0) * cos(decl)) - tan(lat * M_PI / 180.0) * tan(decl);
  /* HACK: keep in range */
  x_tmp = _max(-1, _min(1, x_tmp));
  double halfday = 180.0 / M_PI * acos(x_tmp);
  *sunrise = (720.0 - 4.0 * (lon + halfday) - eqtime) / 60 + timezone;
  *sunset = (720.0 - 4.0 * (lon - halfday) - eqtime) / 60 + timezone;
}

int julian(int mon, int day)
{
  static const int month[13] = {0, 31, 59, 90, 120, 151, 181, 212, 242, 273, 304, 334, 365};
  return month[mon - 1] + day;
}

void check_header(FILE *input, const char *header)
{
  /* printf("Checking header matches:\n\t%s\n", header); */
  /* check that the header matches what is expected */
  char a[1];
  const int n = strlen(header);
  int i;
  /* do this one character at a time because unsure how long line would be if we used %s */
  for (i = 0; i < n + 1; ++i)
  {
    int err = fscanf(input, "%c", a);
    if (0 == err)
    {
      printf("Error reading file\n");
      exit(1);
    }
    /* need a newline at end or else it's not really a match */
    if ((i == n && '\n' != a[0]) || (i < n && a[0] != header[i]))
    {
      printf("Expected columns to be '%s'\n", header);
      exit(1);
    }
  }
}

void check_weather(double temp, double rh, double wind, double rain)
{
  /* just do basic checks, but use this so we can expand checks if desired */
  if (rh < 0 || rh > 100)
  {
    printf("RH must be 0-100, but got %f\n", rh);
    exit(1);
  }
  if (wind < 0)
  {
    printf("Wind must be positive, but got %f\n", wind);
    exit(1);
  }
  if (rain < 0)
  {
    printf("Rain must be positive, but got %f\n", rain);
    exit(1);
  }
}
void check_inputs(double temp, double rh, double wind, double rain, double solrad, double percent_cured, double grass_fuel_load)
{
  check_weather(temp, rh, wind, rain);
  /* just do basic checks, but use this so we can expand checks if desired */
  if (solrad < 0)
  {
    printf("Solar radiation must be positive, but got %f\n", rh);
    exit(1);
  }
  if (percent_cured < 0 || percent_cured > 100)
  {
    printf("Percent cured must be 0-100, but got %f\n", rh);
    exit(1);
  }
  if (grass_fuel_load < 0)
  {
    printf("Grass fuel load must be positive, but got %f\n", wind);
    exit(1);
  }
}

int read_row(FILE *inp, struct row *r)
{
  /* this is declared as an array just to make it a pointer ...for reading commas easily*/
  char a[1];
  int err = fscanf(inp,
                   "%lf%c%lf%c%d%c%d%c%d%c%d%c%lf%c%lf%c%lf%c%lf",
                   &r->lat,
                   a,
                   &r->lon,
                   a,
                   &r->year,
                   a,
                   &r->mon,
                   a,
                   &r->day,
                   a,
                   &r->hour,
                   a,
                   &r->temp,
                   a,
                   &r->rh,
                   a,
                   &r->ws,
                   a,
                   &r->rain);
  if (err > 0)
  {
    check_weather(r->temp, r->rh, r->ws, r->rain);
  }
  return err;
}

int read_row_inputs(FILE *inp, struct row *r)
{
  /* this is declared as an array just to make it a pointer ...for reading commas easily*/
  char a[1];
  int err = fscanf(inp,
                   "%lf%c%lf%c%d%c%d%c%d%c%d%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf",
                   &r->lat,
                   a,
                   &r->lon,
                   a,
                   &r->year,
                   a,
                   &r->mon,
                   a,
                   &r->day,
                   a,
                   &r->hour,
                   a,
                   &r->temp,
                   a,
                   &r->rh,
                   a,
                   &r->ws,
                   a,
                   &r->rain,
                   a,
                   &r->solrad,
                   a,
                   &r->percent_cured,
                   a,
                   &r->grass_fuel_load);
  if (err > 0)
  {
    check_inputs(r->temp, r->rh, r->ws, r->rain, r->solrad, r->percent_cured, r->grass_fuel_load);
  }
  return err;
}

int read_row_daily(FILE *inp, struct row_daily *r)
{
  /* this is declared as an array just to make it a pointer ...for reading commas easily*/
  char a[1];
  int err = fscanf(inp,
                   "%lf%c%lf%c%d%c%d%c%d%c%lf%c%lf%c%lf%c%lf",
                   &r->lat,
                   a,
                   &r->lon,
                   a,
                   &r->year,
                   a,
                   &r->mon,
                   a,
                   &r->day,
                   a,
                   &r->temp,
                   a,
                   &r->rh,
                   a,
                   &r->wind,
                   a,
                   &r->rain);
  if (err > 0)
  {
    check_weather(r->temp, r->rh, r->wind, r->rain);
  }
  return err;
}

int read_row_minmax(FILE *inp, struct row_minmax *r)
{
  /* this is declared as an array just to make it a pointer ...for reading commas easily*/
  char a[1];
  int err = fscanf(inp,
                   "%lf%c%lf%c%d%c%d%c%d%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf",
                   &r->lat,
                   a,
                   &r->lon,
                   a,
                   &r->year,
                   a,
                   &r->mon,
                   a,
                   &r->day,
                   a,
                   &r->temp_min,
                   a,
                   &r->temp_max,
                   a,
                   &r->rh_min,
                   a,
                   &r->rh_max,
                   a,
                   &r->ws_min,
                   a,
                   &r->ws_max,
                   a,
                   &r->rain);
  if (err > 0)
  {
    check_weather(r->temp_min, r->rh_min, r->ws_min, r->rain);
    check_weather(r->temp_max, r->rh_max, r->ws_max, r->rain);
  }
  return err;
}

/* this is a simple piecewise tabular summary (10day) if DKT's NDVI based cure state analysis (smoothed)
It is from central canada Boreal Plains

It will be a DEFAULT greenness given NO other information ....Users should be encouraged to make ...
these local observations each year themselves as such obs will be far superior to this average
*/
double seasonal_curing(int julian_date)
{
  static double PERCENT_CURED[38] = {96.0, 96.0, 96.0, 96.0, 96.0, 96.0, 96.0, 96.0, 95.0, 93.0, 92.0, 90.5, 88.4, 84.4, 78.1, 68.7, 50.3, 32.9, 23.0, 22.0, 21.0, 20.0, 25.7, 35.0, 43.0, 49.8, 60.0, 68.0, 72.0, 75.0, 78.9, 86.0, 96.0, 96.0, 96.0, 96.0, 96.0, 96.0};
  /* these are data from DanT's 10 day average of curing for Boreal Plains ...they have been smoothed however
  and the winter has been added at the max curing observed
  the DATE array values is the first julian date (doy) in the 10 day window....its unnedded the way i did this now
  A date past the end of year has been added to make the array search easier.

  input julian date should be between 1 and 366
  */
  /* truncating the date divide by 10 to get in right range */
  const int jd_class = julian_date / 10;
  const double first = PERCENT_CURED[jd_class];
  const double last = PERCENT_CURED[jd_class + 1];
  /* should be the fractional position in the 10 day period  */
  const double period_frac = (julian_date % 10) / 10.0;
  return (first + (last - first) * period_frac);
}

// // HACK: use sprintf and then copy so we can replace -0.0 without screwing things up with bad rounding
// int _fprintf(FILE *__restrict __stream, const char *__restrict __fmt, ...)
// {
//   char buffer[4096];
//   va_list args;
//   va_start(args, __fmt);
//   const size_t len = vsnprintf(
//       buffer,
//       sizeof(buffer),
//       __fmt,
//       args);
//   if (len >= sizeof(buffer))
//   {
//     printf("**** ERROR: could not write because buffer is too small for %s:\n\t", buffer);
//     exit(-1);
//   }
//   va_end(args);
//   // now replace -0.0 anywhere
// }

int save_rounded(FILE *file, const char *fmt, const double value)
{
  char buffer[4096];
  const size_t len = snprintf(
      buffer,
      sizeof(buffer),
      fmt,
      value);
  if (len >= sizeof(buffer))
  {
    printf("**** ERROR: could not write because buffer is too small for %s:\n\t", buffer);
    exit(-1);
  }
  if (0 == strcmp(buffer, "-0.0"))
  {
#ifndef NDEBUG
    printf("Converting %s with %f to 0.0\n", fmt, value);
#endif
    fprintf(file, "0.0");
  }
  else
  {
#ifndef NDEBUG
    printf("Converting %s with %f to %s\n", fmt, value, buffer);
#endif
    fprintf(file, "%s", buffer);
  }
  return len;
}

// HACK: act like other languages for printing based on columns so results are the same
void save_csv(FILE *file, const char *fmt_all, ...)
{
  char buffer[strlen(fmt_all) + 1];
  strcpy(buffer, fmt_all);
  va_list args;
#ifndef NDEBUG
  va_start(args, fmt_all);
  // can't reuse args after vprintf()
  printf("Called save_csv() with %s\n", fmt_all);
  vprintf(fmt_all, args);
  va_end(args);
#endif
  va_start(args, fmt_all);
  // int done = 0;
  // int i = 0;
  int j = 0;
  char delim_buffer[2];
  delim_buffer[1] = '\0';
  while (buffer[j] != '\0')
  {
    // if (done > n)
    // {
    //   printf("***** ERROR: more values than format string defines for %s\n:", fmt_all);
    //   // should crash here?
    //   vprintf(fmt_all, args);
    //   exit(-1);
    // }
    int k = j;
    // find next delimeter or end of format
    while (buffer[k] != '\n' && buffer[k] != '\0' && buffer[k] != ',')
    {
      ++k;
    }
    delim_buffer[0] = fmt_all[k];
    // just replace with '\0' and then undo so we can use printf on substring
    buffer[k] = '\0';
    if (k == j)
    {
      // nothing there so must be done?
      break;
    }
    // all format strings should end in a letter?
#ifndef NDEBUG
    printf("Checking for %s\n", &(buffer[j]));
#endif
    switch (buffer[k - 1])
    {
    case 'd':
      int value_int = va_arg(args, int);
      // no need to guard against "-0.0"
#ifndef NDEBUG
      printf("Converting %s with %d to ", &(buffer[j]), value_int);
      printf(&(buffer[j]), value_int);
      printf("\n");
#endif
      fprintf(file, &(buffer[j]), value_int);
      break;
    case 'f':
    // fall through
    case 'g':
      double value_double = va_arg(args, double);
#ifndef NDEBUG
      printf("formatting %s with %f\n", &(buffer[j]), value_double);
#endif
      save_rounded(file, &(buffer[j]), value_double);
      break;
    default:
      printf("***** ERROR: invalid format string %s", buffer);
      exit(-1);
    }
    buffer[k] = delim_buffer[0];
    if (delim_buffer[0] != '\0')
    {
      fprintf(file, "%s", delim_buffer);
    }
    j = k + 1;
    // ++i;
    // ++done;
  }
  va_end(args);
}
