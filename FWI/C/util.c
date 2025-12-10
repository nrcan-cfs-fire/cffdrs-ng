#include "util.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>
#include <time.h>
#define NDEBUG



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

double seasonal_curing(int yr, int mon, int day, int start_mon, int start_day)
{
  static double PERCENT_CURED[] = {
    96.0,  // "winter" cured value
    95.0,
    93.0,
    92.0,
    90.5,
    88.4,
    84.4,
    78.1,
    68.7,
    50.3,
    32.9,
    23.0,
    22.0,
    21.0,
    20.0,
    25.7,
    35.0,
    43.0,
    49.8,
    60.0,
    68.0,
    72.0,
    75.0,
    78.9,
    86.0,
    96.0  // "winter" cured value for rest of year
  };

  // find previous green up start date (year - 1 or year)
  struct tm date = {
    .tm_year = yr - 1900,
    .tm_mon = mon - 1,
    .tm_mday = day,
    .tm_isdst = 0};
  struct tm greenup = {
    .tm_year = yr - 1900,
    .tm_mon = start_mon - 1,
    .tm_mday = start_day,
    .tm_isdst = 0};
  
  int shift = difftime(mktime(&date), mktime(&greenup)) / 86400;  // 86400s / day
  if (shift < 0) {
    greenup.tm_year = yr - 1 - 1900;
    shift = difftime(mktime(&date), mktime(&greenup)) / 86400;
  }

  int days_in = shift + 1;  // start date is first non-winter value (not 0th)

  // check if date is in green phase or winter (cured) phase
  if (days_in < (sizeof(PERCENT_CURED) / sizeof(PERCENT_CURED[0]) - 1) * 10) {
    // linear interpolation between every 10-day value
    double per_cur0 = PERCENT_CURED[days_in / 10];
    double per_cur1 = PERCENT_CURED[days_in / 10 + 1];
    double period_frac = (days_in % 10) / 10.0;
    double result = per_cur0 + (per_cur1 - per_cur0) * period_frac;
    return result;
  } else {
    return PERCENT_CURED[0];
  }
}

bool is_leap(int yr) {
  return (yr % 4 == 0 && yr % 100 != 0 || yr % 400 == 0);
}

int julian(int yr, int mon, int day)
{
    if (mon == 1) {  // January
        return day;
    } else if (mon == 2) {  // February
        return day + 31;
    } else {  // March-December depends on leap year, use algorithm
        int i;
        if (is_leap(yr)) {
            i = 2;
        } else {
            i = 3;
        }
        return (30 * (mon - 1) + floor(0.6 * (mon + 1)) - i + day);
    }
}

double single_hour_solrad_estimation(struct row *r)
{
  double dechour = 12.0;
  // .tm_yday is already 0-indexed (i.e. Jan 1st = 0)
  double fracyear = 2.0 * M_PI * (r->timestamp.tm_yday + (dechour - 12.0) / 24.0);
  if (is_leap(r->year)) {
    fracyear = fracyear / 366.0;
  } else {
    fracyear = fracyear / 365.0;
  }
  double eqtime = 229.18 * (0.000075 +
    0.001868 * cos(fracyear) - 0.032077 * sin(fracyear) -
    0.014615 * cos(2.0 * fracyear) - 0.040849 * sin(2.0 * fracyear));
  double decl = 0.006918 -
    0.399912 * cos(fracyear) + 0.070257 * sin(fracyear) -
    0.006758 * cos(fracyear * 2.0) + 0.000907 * sin(2.0 * fracyear) -
    0.002697 * cos(3.0 * fracyear) + 0.00148 * sin(3.0 * fracyear);
  double timeoffset = eqtime + 4.0 * r->lon - 60.0 * r->timezone;
  double tst = ((double)r->hour) * 60.0 + timeoffset;
  double hourangle = tst / 4.0 - 180.0;
  double zenith = acos(sin(r->lat * M_PI / 180.0) * sin(decl) +
    cos(r->lat * M_PI / 180.0) * cos(decl) * cos(hourangle * M_PI / 180.0));
  zenith = _min(M_PI / 2.0, zenith);
  double cos_zenith = cos(zenith);
  double vpd = 6.11 * (1.0 - r->rh / 100.0) *
    exp(17.29 * r->temp / (r->temp + 237.3));
  
  double solrad = cos_zenith * 0.92 * (1.0 - exp(-0.22 * vpd));
  if (solrad < 1e-4) {
    solrad = 0.0;
  }
  return solrad;
}

void sunrise_sunset(double lat, double lon, double timezone,
  struct tm date, double *suntime)
{
  double dechour = 12.0;
  // .tm_yday is already 0-indexed (i.e. Jan 1st = 0)
  double fracyear = 2.0 * M_PI * (date.tm_yday + (dechour - 12.0) / 24.0);
  if (is_leap(date.tm_year)) {
    fracyear = fracyear / 366.0;
  } else {
    fracyear = fracyear / 365.0;
  }
  double eqtime = 229.18 * (0.000075 +
    0.001868 * cos(fracyear) - 0.032077 * sin(fracyear) - 
    0.014615 * cos(2.0 * fracyear) - 0.040849 * sin(2.0 * fracyear));
  double decl = 0.006918 -
    0.399912 * cos(fracyear) + 0.070257 * sin(fracyear) -
    0.006758 * cos(fracyear * 2.0) + 0.000907 * sin(2.0 * fracyear) -
    0.002697 * cos(3.0 * fracyear) + 0.00148 * sin(3.0 * fracyear);
  double timeoffset = eqtime + 4 * lon - 60 * timezone;
  double zenith = 90.833 * M_PI / 180.0;
  double x_tmp = cos(zenith) / (cos(lat * M_PI / 180.0) * cos(decl)) -
    tan(lat * M_PI / 180.0) * tan(decl);
  x_tmp = _max(-1.0, _min(1.0, x_tmp));
  double halfday = 180.0 * acos(x_tmp) / M_PI;

  suntime[0] = (720.0 - 4.0 * (lon + halfday) - eqtime) / 60.0 + timezone;
  suntime[1] = (720.0 - 4.0 * (lon - halfday) - eqtime) / 60.0 + timezone;
}

void check_header_FWI(FILE *input, const char *header_req, struct flags *f) {

  char header_full[200];

  // add optional parameters for a full header
  strcpy(header_full, header_req);
  strcat(header_full, ",grass_fuel_load,percent_cured,solrad");

  char in_buffer[200];  // limit input header to 200 characters
  int in_buffer_len;

  // read first line of file
  fscanf(input, "%200s", in_buffer);  // limit input header to 200 characters
  in_buffer_len = strlen(in_buffer);

  if (in_buffer_len == 200) {  // limit input header to 200 characters
    puts("Input header has 200 or more characters, remove columns or increase limit");
    exit(1);
  }

  if (strncmp(header_req, in_buffer, strlen(header_req)) != 0) {
    printf("Error: Missing required columns or didn't start in this order\n%s\n",
      header_req);
    exit(1);
  }

  if (in_buffer_len > strlen(header_full)) {
    printf("Error: Input file header too long. Max possible columns are\n%s\n",
      header_full);
    exit(1);
  }

  // create all combinations of optional grass_fuel_load, percent_cured, solrad columns
  char header_no_s[200];
  strcpy(header_no_s, header_req);
  strcat(header_no_s, ",grass_fuel_load,percent_cured");

  char header_no_p[200];
  strcpy(header_no_p, header_req);
  strcat(header_no_p, ",grass_fuel_load,solrad");

  char header_no_g[200];
  strcpy(header_no_g, header_req);
  strcat(header_no_g, ",percent_cured,solrad");

  char header_no_ps[200];
  strcpy(header_no_ps, header_req);
  strcat(header_no_ps, ",grass_fuel_load");

  char header_no_gs[200];
  strcpy(header_no_gs, header_req);
  strcat(header_no_gs, ",percent_cured");

  char header_no_gp[200];
  strcpy(header_no_gp, header_req);
  strcat(header_no_gp, ",solrad");

  // check if in_buffer matches any combination of optional headers
  if (strcmp(header_full, in_buffer) == 0) {  // match
    // leave flags to default false (meaning no need to calculate)
  } else if (strcmp(header_no_s, in_buffer) == 0) {
    f->solrad_flag = true;
  } else if (strcmp(header_no_p, in_buffer) == 0) {
    f->percent_cured_flag = true;
  } else if (strcmp(header_no_g, in_buffer) == 0) {
    f->grass_fuel_load_flag = true;
  } else if (strcmp(header_no_ps, in_buffer) == 0) {
    f->percent_cured_flag = true;
    f->solrad_flag = true;
  } else if (strcmp(header_no_gs, in_buffer) == 0) {
    f->grass_fuel_load_flag = true;
    f->solrad_flag = true;
  } else if (strcmp(header_no_gp, in_buffer) == 0) {
    f->grass_fuel_load_flag = true;
    f->percent_cured_flag = true;
  } else if (strcmp(header_req, in_buffer) == 0) {
    f->grass_fuel_load_flag = true;
    f->percent_cured_flag = true;
    f->solrad_flag = true;
  } else {
    printf("Error: Optional columns need to be ordered\n%s\n", header_full);
    exit(1);
  }

}

void check_header_match(FILE *input, const char *header)
{
  /* check that the first line in input matches header */
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

void check_inputs(double temp, double rh, double wind, double rain,
  double grass_fuel_load, double percent_cured, double solrad)
{
  check_weather(temp, rh, wind, rain);
  /* just do basic checks, but use this so we can expand checks if desired */
  if (solrad < 0)
  {
    printf("Solar radiation must be positive, but got %f\n", solrad);
    exit(1);
  }
  if (percent_cured < 0 || percent_cured > 100)
  {
    printf("Percent cured must be 0-100, but got %f\n", percent_cured);
    exit(1);
  }
  if (grass_fuel_load < 0)
  {
    printf("Grass fuel load must be positive, but got %f\n", grass_fuel_load);
    exit(1);
  }
}

int read_row_inputs(FILE *inp, struct row *r, struct flags *f,
  float def_grass_fuel_load, int def_mon_curing, int def_day_curing) {
  char line[500];  // limit a row of data to 500 characters
  int err;

  // read in the next line from the input csv
  err = fscanf(inp, "%500s", line);  // limit a row of data to 500 characters
  
  if (strlen(line) == 500) {  // limit a row of data to 500 characters
    puts("Input data line has 500 or more characters, remove columns or increase limit");
    exit(1);
  }

  if (err == 0) {  // fscanf didn't find another line
    return err;
  }

  // this section uses (requires) the fact that required inputs are ordered and complete
  r->lat = atof(strtok(line, ","));
  r->lon = atof(strtok(NULL, ","));
  r->year = atoi(strtok(NULL, ","));
  r->mon = atoi(strtok(NULL, ","));
  r->day = atoi(strtok(NULL, ","));
  r->hour = atoi(strtok(NULL, ","));
  r->temp = atof(strtok(NULL, ","));
  r->rh = atof(strtok(NULL, ","));
  r->ws = atof(strtok(NULL, ","));
  r->rain = atof(strtok(NULL, ","));

  // make timestamp and calculate julian (yday)
  struct tm ts = {
    .tm_year = r->year - 1900,  // years since 1900
    .tm_mon = r->mon - 1,  // 0-indexed month (i.e. Jan = 0)
    .tm_mday = r->day,
    .tm_hour = r->hour,
    .tm_isdst = 0  // modify command line timezone input only for daylight time data
  };
  mktime(&ts);

  r->timestamp = ts;

  // optional inputs provided or calculated
  if (f->grass_fuel_load_flag) {  // true means not provided, needs to be calculated
    r->grass_fuel_load = def_grass_fuel_load;
  } else {
    r->grass_fuel_load = atof(strtok(NULL, ","));
  }
  
  if (f->percent_cured_flag) {
    r->percent_cured = seasonal_curing(r->year, r->mon, r->day,
      def_mon_curing, def_day_curing);
  } else {
    r->percent_cured = atof(strtok(NULL, ","));
  }

  if (f->solrad_flag) {
    r->solrad = single_hour_solrad_estimation(r);
  } else {
    r->solrad = atof(strtok(NULL, ","));
  }

  check_inputs(r->temp, r->rh, r->ws, r->rain,
    r->grass_fuel_load, r->percent_cured, r->solrad);

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
    "%lf%c%lf%c"
    "%d%c%d%c%d%c"
    "%lf%c%lf%c"
    "%lf%c%lf%c"
    "%lf%c%lf%c%lf",
    &r->lat, a, &r->lon, a,
    &r->year, a, &r->mon, a, &r->day, a,
    &r->temp_min, a, &r->temp_max, a,
    &r->rh_min, a, &r->rh_max, a,
    &r->ws_min, a, &r->ws_max, a, &r->rain);
  if (err > 0)
  {
    check_weather(r->temp_min, r->rh_min, r->ws_min, r->rain);
    check_weather(r->temp_max, r->rh_max, r->ws_max, r->rain);
  }

  struct tm date = {
    .tm_year = r->year - 1900,  // years since 1900
    .tm_mon = r->mon - 1,  // 0-indexed month (i.e. Jan = 0)
    .tm_mday = r->day,
    .tm_isdst = 0  // modify command line timezone input only for daylight time data
  };
  mktime(&date);
  r->date = date;

  return err;
}

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

// act like other languages for printing based on columns so results are the same
void save_csv(FILE *file, const char *fmt_all, ...)
{
  char *buffer = (char *)malloc(sizeof(char) * (strlen(fmt_all) + 1));
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
    case 'd': ;
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
    case 'g': ;
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
  free(buffer);
}
