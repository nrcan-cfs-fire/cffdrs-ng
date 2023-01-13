#include "util.h"

float findQ(float temp, float rh)
{
  /* find absolute humidity */
  float svp = 6.108 * exp(17.27 * temp / (temp + 237.3));
  float vp = svp * rh / 100.0;
  return(217 * vp / (273.17 + temp));
}

float findrh(float q, float temp)
{
  float cur_vp = (273.17 + temp) * q / 217;
  return(100 * cur_vp / (6.108 * exp(17.27 * temp / (temp + 237.3))));
}

float sun(float lat, float lon, int mon, int day, int hour, int timezone, float *sunrise, float *sunset)
{
  int jd = julian(mon, day);
  return sun_julian(lat, lon, jd, hour, timezone, sunrise, sunset);
}

float sun_julian(float lat, float lon, int jd, int hour, int timezone, float *sunrise, float *sunset)
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
  float fracyear = 2.0 * M_PI / 365.0 * ((float)(jd) - 1.0 + ((float)(dechour) - 12.0) / 24.0);
  float eqtime = 229.18 * (0.000075 + 0.001868 * cos(fracyear) - 0.032077 * sin (fracyear)
                  - 0.014615 * cos(2.0 * fracyear) - 0.040849 * sin(2.0 * fracyear));
  float decl = 0.006918 - 0.399912 * cos(fracyear) + 0.070257 * sin(fracyear)
                - 0.006758 * cos(fracyear * 2.0) + 0.000907 * sin(2.0 * fracyear)
                - 0.002697 * cos(3.0 * fracyear) + 0.00148 * sin(3.0 * fracyear);
  float timeoffset = eqtime + 4 * lon - 60 * timezone;
  float tst = (float)hour * 60.0 + timeoffset;
  float hourangle = tst / 4 - 180;
  float zenith = acos(sin(lat * M_PI / 180) * sin(decl)
                  + cos(lat * M_PI / 180) * cos(decl) * cos(hourangle * M_PI / 180));
  float solrad = 0.95 * cos(zenith);
  if(solrad < 0) { solrad = 0.0; }
  printf(" SOLAR: %d  %d fracyear=%f dec=%f  toff=%f  tst=%fha=%f zen=%f  solrad=%f\n",
         jd, hour, fracyear, decl, timeoffset, tst, hourangle, zenith, solrad);
  zenith = 90.833 * M_PI / 180.0;
  float halfday = 180.0 / M_PI *
            acos(cos(zenith) / (cos(lat * M_PI / 180.0) * cos(decl))
                  - tan(lat * M_PI / 180.0) * tan(decl));
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
   for(i = 0; i < header_len; ++i)
   {
     fscanf(input, "%c", a);
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
  if (rh < 0 || rh > 100) {
    printf("RH must be 0-100, but got %f\n", rh);
    exit(1);
  }
  if (wind < 0) {
    printf("Wind must be positive, but got %f\n", wind);
    exit(1);
  }
  if (rain < 0) {
    printf("Rain must be positive, but got %f\n", rain);
    exit(1);
  }
}

int read_row(FILE *inp, struct row* r)
{
  char a[1]; /* this is declared as an array just to make it a pointer ...for reading commas easily*/
  int err = fscanf(inp,"%f%c%f%c%d%c%d%c%d%c%d%c%f%c%f%c%f%c%f",
  &r->lat,a,&r->lon,a,&r->year,a,&r->mon,a,&r->day,a,&r->hour,a,&r->temp,a,&r->rh,a,&r->wind,a,&r->rain);
  if (err > 0)
  {
    check_inputs(r->temp, r->rh, r->wind, r->rain);
  }
  return err;
}

int read_row_minmax(FILE *inp, struct row_minmax* r)
{
  char a[1]; /* this is declared as an array just to make it a pointer ...for reading commas easily*/
  int err = fscanf(inp,"%f%c%f%c%d%c%d%c%d%c%d%c%f%c%f%c%f%c%f%c%f%c%f%c%f",
  &r->lat,a,&r->lon,a,&r->year,a,&r->mon,a,&r->day,a,&r->hour,a,&r->temp_min,a,&r->temp_max,a,&r->rh_min,a,&r->rh_max,a,&r->wind_min,a,&r->wind_max,a,&r->rain);
  if (err > 0)
  {
    check_inputs(r->temp_min, r->rh_min, r->wind_min, r->rain);
    check_inputs(r->temp_max, r->rh_max, r->wind_max, r->rain);
  }
  return err;
}
