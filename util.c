#include "util.h"

float findQ(float temp, float rh)
{
  /* find absolute humidity */
  float svp = 6.108 * exp(17.27 * temp / (temp + 237.3));
  float vp = svp * rh / 100.0;
  return (217 * vp / (273.17 + temp));
}

float findrh(float q, float temp)
{
  float cur_vp = (273.17 + temp) * q / 217;
  return (100 * cur_vp / (6.108 * exp(17.27 * temp / (temp + 237.3))));
}

float sun(float lat, float lon, int mon, int day, int hour, int timezone, float* sunrise, float* sunset)
{
  int jd = julian(mon, day);
  return sun_julian(lat, lon, jd, hour, timezone, sunrise, sunset);
}

float sun_julian(float lat, float lon, int jd, int hour, int timezone, float* sunrise, float* sunset)
{
  /*

  this routine approximately calcualtes sunrise and sunset and daylength
  Really any routine like this could be used,  some are more precise than others.

  It takes in:
  latitude:   in degrees north -  positive number
  longtitude: in degress EAST(standard)  - WEST hemisphere is a negative
  month:
  day:
  adjust:  hours off of Greenich mean time (for EST = -5  (EDT=-4)   CST=-6 MST=-7  PST=-8)

  It returns (as pass by reference from the funciton call line)
  SUNRISE in decimal hours  (in the local time zone specified)
  SUNSET in decimal hours  (in the local time zone specified)

  and the function itself returns
  DAYLENGTH (in hours)


  bmw
  */
  float dechour = 12.0;
  float fracyear = 2.0 * M_PI / 365.0 * ((float)(jd)-1.0 + ((float)(dechour)-12.0) / 24.0);
  float eqtime = 229.18 * (0.000075 + 0.001868 * cos(fracyear) - 0.032077 * sin(fracyear) - 0.014615 * cos(2.0 * fracyear) - 0.040849 * sin(2.0 * fracyear));
  float decl = 0.006918 - 0.399912 * cos(fracyear) + 0.070257 * sin(fracyear)
             - 0.006758 * cos(fracyear * 2.0) + 0.000907 * sin(2.0 * fracyear)
             - 0.002697 * cos(3.0 * fracyear) + 0.00148 * sin(3.0 * fracyear);
  float timeoffset = eqtime + 4 * lon - 60 * timezone;
  float tst = (float)hour * 60.0 + timeoffset;
  float hourangle = tst / 4 - 180;
  float zenith = acos(sin(lat * M_PI / 180) * sin(decl)
                      + cos(lat * M_PI / 180) * cos(decl) * cos(hourangle * M_PI / 180));
  float solrad = 0.95 * cos(zenith);
  if (solrad < 0)
  {
    solrad = 0.0;
  }
  printf(" SOLAR: %d  %d fracyear=%f dec=%f  toff=%f  tst=%fha=%f zen=%f  solrad=%f\n",
         jd,
         hour,
         fracyear,
         decl,
         timeoffset,
         tst,
         hourangle,
         zenith,
         solrad);
  zenith = 90.833 * M_PI / 180.0;
  /*
   * FIX: is this some kind of approximation that can be wrong?
   *       breaks with (67.1520291504819, -132.37538245496188)
   */
  float x_tmp = cos(zenith) / (cos(lat * M_PI / 180.0) * cos(decl)) - tan(lat * M_PI / 180.0) * tan(decl);
  /* HACK: keep in range */
  if (x_tmp < -1)
  {
    x_tmp = -1;
  }
  else if (x_tmp > 1)
  {
    x_tmp = 1;
  }
  float halfday = 180.0 / M_PI * acos(x_tmp);
  *sunrise = (720.0 - 4.0 * (lon + halfday) - eqtime) / 60 + timezone;
  *sunset = (720.0 - 4.0 * (lon - halfday) - eqtime) / 60 + timezone;
  return solrad;
}

int julian(int mon, int day)
{
  static const int month[13] = {0, 31, 59, 90, 120, 151, 181, 212, 242, 273, 304, 334, 365};
  return month[mon - 1] + day;
}

void check_header(FILE* input, const char* header)
{
  /* check that the header matches what is expected */
  char a[1];
  const int header_len = strlen(header);
  int i;
  /* do this one character at a time because unsure how long line would be if we used %s */
  for (i = 0; i < header_len; ++i)
  {
    int err = fscanf(input, "%c", a);
    if (0 == err)
    {
      printf("Error reading file\n");
      exit(1);
    }
    if (a[0] != header[i])
    {
      printf("Expected columns to be '%s'\n", header);
      exit(1);
    }
  }
}

void check_inputs(float temp, float rh, float wind, float rain)
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

int read_row(FILE* inp, struct row* r)
{
  /* this is declared as an array just to make it a pointer ...for reading commas easily*/
  char a[1];
  int err = fscanf(inp, "%f%c%f%c%d%c%d%c%d%c%d%c%f%c%f%c%f%c%f", &r->lat, a, &r->lon, a, &r->year, a, &r->mon, a, &r->day, a, &r->hour, a, &r->temp, a, &r->rh, a, &r->wind, a, &r->rain);
  if (err > 0)
  {
    check_inputs(r->temp, r->rh, r->wind, r->rain);
  }
  return err;
}

int read_row_minmax(FILE* inp, struct row_minmax* r)
{
  /* this is declared as an array just to make it a pointer ...for reading commas easily*/
  char a[1];
  int err = fscanf(inp, "%f%c%f%c%d%c%d%c%d%c%d%c%f%c%f%c%f%c%f%c%f%c%f%c%f", &r->lat, a, &r->lon, a, &r->year, a, &r->mon, a, &r->day, a, &r->hour, a, &r->temp_min, a, &r->temp_max, a, &r->rh_min, a, &r->rh_max, a, &r->wind_min, a, &r->wind_max, a, &r->rain);
  if (err > 0)
  {
    check_inputs(r->temp_min, r->rh_min, r->wind_min, r->rain);
    check_inputs(r->temp_max, r->rh_max, r->wind_max, r->rain);
  }
  return err;
}

/* this is a simple piecewise tabular summary (10day) if DKT's NDVI based cure state analysis (smoothed)
It is from central canada Boreal Plains

It will be a DEFAULT greenness given NO other information ....Users should be encouraged to make ...
these local observations each year themselves as such obs will be far superior to this average
*/
float seasonal_curing(int julian_date)
{
  static float PERCENT_CURED[38] = {96.0, 96.0, 96.0, 96.0, 96.0, 96.0, 96.0, 96.0, 95.0, 93.0, 92.0, 90.5, 88.4, 84.4, 78.1, 68.7, 50.3, 32.9, 23.0, 22.0, 21.0, 20.0, 25.7, 35.0, 43.0, 49.8, 60.0, 68.0, 72.0, 75.0, 78.9, 86.0, 96.0, 96.0, 96.0, 96.0, 96.0, 96.0};
  /* these are data from DanT's 10 day average of curing for Boreal Plains ...they have been smoothed however
  and the winter has been added at the max curing observed
  the DATE array values is the first julian date (doy) in the 10 day window....its unnedded the way i did this now
  A date past the end of year has been added to make the array search easier.

  input julian date should be between 1 and 366
  */
  /* truncating the date divide by 10 to get in right range */
  int jd_class = (int)(julian_date / 10.0);
  /* should be the fractional position in the 10 day period  */
  float period_frac = (julian_date - (jd_class * 10.0)) / 10.0;
  float difference = PERCENT_CURED[jd_class + 1] - PERCENT_CURED[jd_class];
  float cure = PERCENT_CURED[jd_class] + difference * period_frac;
  return cure;
}
