/*
 * Create a file that has all the inputs that are required for the NG_FWI
 * from a file that only has hourly weather data.
 */
#include "util.h"
#include <stdlib.h>

/* Fuel Load (kg/m^2) */
static const double DEFAULT_GRASS_FUEL_LOAD = 0.35;

/* FIX: figure out what this should be */
static const double DEFAULT_LATITUDE = 55.0;
static const double DEFAULT_LONGITUDE = -120.0;

int populate_row(FILE *inp, struct row *cur, double TZadjust)
{
  int err = read_row(inp, cur);
  double julian_day = julian(cur->mon, cur->day);
  /*
    FIX: Allow using solrad, percent_cured, or grass_fuel_load columns if they exist already
  */
  /*
    assuming we want curing to change based on current day and not remain
    the same across entire period based on start date
  */
  cur->percent_cured = seasonal_curing(julian_day);
  /* FIX: use a constant grass fuel load for now */
  cur->grass_fuel_load = DEFAULT_GRASS_FUEL_LOAD;
  return err;
}

int main(int argc, char *argv[])
{
  /*  CSV headers */
  static const char *header = "lat,long,yr,mon,day,hr,temp,rh,ws,prec";
  static const char *header_out = "lat,long,yr,mon,day,hr,temp,rh,ws,prec,solrad,percent_cured,grass_fuel_load";
  if (4 != argc)
  {
    printf("Command line:   %s <local GMToffset> <input file> <output file>\n\n", argv[0]);
    printf("<local GMToffset> is the off of Greenich mean time (for Eastern = -5  Central=-6   MT=-7  PT=-8 )  \n");
    printf("All times should be local standard time\n");
    printf("INPUT FILE format must be HOURLY weather data, comma seperated and take the form\n");
    printf("%s\n\n", header);
    exit(1);
  }

  FILE *inp = fopen(argv[2], "r");
  printf("Opening input file >>> %s   \n", argv[2]);
  if (inp == NULL)
  {
    printf("\n\n ***** FILE  %s  does not exist\n", argv[2]);
    exit(1);
  }
  int TZadjust = atoi(argv[1]);
  if (TZadjust < -9 || TZadjust > -2)
  {
    printf("/n *****   Local time zone adjustment must be vaguely in Canada so between -9 and -2 \n");
    exit(1);
  }
  /* check that the header matches what is expected */
  check_header(inp, header);
  struct row cur;
  int err = populate_row(inp, &cur, TZadjust);
  struct row for_day[24];
  struct row old = cur;
  FILE *out = fopen(argv[3], "w");
  fprintf(out, "%s\n", header_out);
  while (err > 0)
  {
    /* don't assume all 24 hours are there for every day */
    int hour_min = 25;
    int hour_max = -1;
    /* read until end of day */
    /* either haven't assigned anything yet, or we hit hour 0 of next day */
    while ((err > 0) && ((-1 == hour_max) || (cur.hour > 0)))
    {
      hour_min = _min(hour_min, cur.hour);
      hour_max = _max(hour_max, cur.hour);
      for_day[cur.hour] = cur;
      old = cur;
      err = populate_row(inp, &cur, TZadjust);
      if (err > 0 && (old.lon != cur.lon || old.lat != cur.lat))
      {
        printf("Latitude and Longitude must be constant\n");
        exit(1);
      }
      /* FIX: doesn't check that days are sequential too */
      if (err > 0 && (1 != (cur.hour - old.hour) && !(23 == old.hour && 0 == cur.hour)))
      {
        printf("Hours must be sequential but went from %d to %d\n",
               old.hour,
               cur.hour);
        exit(1);
      }
    }
    if (-1 == hour_max)
    {
      /* didn't actually read anything, so do nothing */
      break;
    }
    /* calculate temperature range for the hours that are there */
    double temp_min = __DBL_MAX__;
    double temp_max = __DBL_MIN__;
    for (int h = hour_min; h <= hour_max; ++h)
    {
      temp_min = _min(temp_min, for_day[h].temp);
      temp_max = _max(temp_max, for_day[h].temp);
    }
    double temp_range = temp_max - temp_min;
    struct double_24hr solrad;
    /*
      Regardless of how many hours were read, need to calculate all 24 hours of
      solar radiation because they rely on the sum for the day.
     */
    solar_radiation(for_day[hour_min].lat,
                    for_day[hour_min].lon,
                    for_day[hour_min].mon,
                    for_day[hour_min].day,
                    TZadjust,
                    temp_range,
                    &solrad);
    /* output lines for rows that existed in input */
    for (int h = hour_min; h <= hour_max; ++h)
    {
      for_day[h].solrad = solrad.hour[h];
      save_csv(out,
               "%.4f,%.4f,%4d,%02d,%02d,%02d,%.1f,%.0f,%.1f,%.2f,%.4f,%.1f,%.2f\n",
               for_day[h].lat,
               for_day[h].lon,
               for_day[h].year,
               for_day[h].mon,
               for_day[h].day,
               for_day[h].hour,
               for_day[h].temp,
               for_day[h].rh,
               for_day[h].ws,
               for_day[h].rain,
               for_day[h].solrad,
               for_day[h].percent_cured,
               for_day[h].grass_fuel_load);
    }
  } /* end the main while(err>0)  */

  /* printf("output has been written to>>> %s\n",argv[6]); */
  fclose(inp);
  fclose(out);

  return 0;
}
