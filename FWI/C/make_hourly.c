/*
inputs min/max weather stream
outputs hourly weather stream
*/

#include <stdlib.h>
#include <math.h>
#include "util.h"

/* Alpha/Beta/Gamma coefficients for Temperature, RH, and Wind Speed */
const double C_TEMP[3] = {0.0, 2.75, -1.9};
const double C_RH[3] = {0.25, 2.75, -2.0};
const double C_WIND[3] = {1.0, 1.5, -1.3};

double diurnal(double v_min, double v_max, double tv_min, double yv_sunset,
  double sunrise, double sunset,
  double solarnoon_yest, double sunset_yest, double sunrise_tom,
  const double *abg, double *hourly);

int main(int argc, char *argv[])
{
  if (argc != 4)
  {
    printf("\n########\nhelp/usage:\n%s input output timezone\n\n", argv[0]);
    // Help
    printf("positional arguments:\n"
      "input                 Input csv data file\n"
      "output                Output csv file name and location\n"
      "timezone              UTC offset\n########\n\n");
    exit(1);
  }
  
  /*  CSV headers */
  static const char *header = "lat,long,yr,mon,day,"
    "temp_min,temp_max,rh_min,rh_max,ws_min,ws_max,prec";
  static const char *header_out = "lat,long,yr,mon,day,hr,temp,rh,ws,prec";

  FILE *inp, *out;
  int err;
  double TZadjust;

  // open input file
  inp = fopen(argv[1], "r");
  printf("Opening input file >>> %s   \n", argv[2]);
  if (inp == NULL)
  {
    printf("\n\n ***** FILE  %s  does not exist\n", argv[2]);
    exit(1);
  }

  // load required timezone argument
  TZadjust = atoi(argv[3]);

  check_header_match(inp, header);
  
  // open output file
  out = fopen(argv[2], "w");
  if (out == NULL) {
    printf("\n\n***** FILE %s can not be opened\n", argv[2]);
    exit(1);
  }
  printf("Saving outputs to file >>> %s\n", argv[2]);
  fprintf(out, "%s\n", header_out);

  // start calculation
  struct row_minmax yest, cur, tom;
  err = read_row_minmax(inp, &cur);
  if (!(err > 0))
  {
    printf("Unable to read first row of indices\n");
    exit(1);
  }
  cur.timezone = TZadjust;

  /* use today's values for yesterday because we need to do something */
  yest = cur;
  double solarnoon_yest = 0.0;
  double sunset_yest = 0.0;
  double ytemp_sunset = 0.0;
  double yrh_sunset = 0.0;
  double ywind_sunset = 0.0;
  int done_first = 0;
  while (err > 0)
  {
    if (done_first)
    {
      err = read_row_minmax(inp, &tom);
    }
    else
    {
      yest = cur;
      tom = cur;
    }
    if (!(err > 0))
    {
      /* error but we still want to figure out today's values */
      tom = cur;
    }
    static const int LST_NOON = 12;

    // calculate sunrise and sunset for current day and tomorrow
    double suntime[2];
    sunrise_sunset(cur.lat, cur.lon, cur.timezone, cur.date, suntime);
    cur.sunrise = suntime[0];
    cur.sunset = suntime[1];
    double solarnoon = (cur.sunset - cur.sunrise) / 2.0 + cur.sunrise;

    tom.date.tm_mday++;  // increase day by one (mktime handles month transitions)
    mktime(&tom.date);  // update tm structure
    sunrise_sunset(tom.lat, tom.lon, tom.timezone, tom.date, suntime);
    tom.sunrise = suntime[0];
    tom.sunset = suntime[1];

    double atemp[24], arh[24], aws[24], arain[24];
    int h;
    for (h = 0; h < 24; ++h)
    {
      atemp[h] = 0.0;
      arh[h] = 0.0;
      aws[h] = 0.0;
      arain[h] = 0.0;
    }
    ytemp_sunset = diurnal(cur.temp_min, cur.temp_max, tom.temp_min, ytemp_sunset,
      cur.sunrise, cur.sunset, solarnoon_yest, sunset_yest, tom.sunrise, C_TEMP, atemp);
    yrh_sunset = diurnal(1.0 - cur.rh_max / 100.0, 1.0 - cur.rh_min / 100.0,
      1.0 - tom.rh_max / 100.0, yrh_sunset,
      cur.sunrise, cur.sunset, solarnoon_yest, sunset_yest, tom.sunrise, C_RH, arh);
    ywind_sunset = diurnal(cur.ws_min, cur.ws_max, tom.ws_min, ywind_sunset,
      cur.sunrise, cur.sunset, solarnoon_yest, sunset_yest, tom.sunrise, C_WIND, aws);
    arain[7] = cur.rain;
    if (done_first)
    {
      for (h = 0; h < 24; ++h)
      {
        arh[h] = 100 * (1.0 - arh[h]);
        save_csv(out,
                 "%.4f,%.4f,%d,%d,%d,%d,"
                 "%.4f,%.4f,%.4f,%f\n",
                 cur.lat, cur.lon, cur.year, cur.mon, cur.day, h,
                 atemp[h], arh[h], aws[h], arain[h]);
      }
    }
    else
    {
      done_first = 1;
    }
    /* move today to yesterday's variables */
    yest = cur;
    solarnoon_yest = solarnoon;
    sunset_yest = cur.sunset;
    /* advance to the next day */
    cur = tom;
  } /* end the main while(err>0)  */

  /* printf("output has been written to>>> %s\n",argv[6]); */
  fclose(inp);
  fclose(out);

  return 0;
}

/**
 * Determines values based on a diurnal curve
 *
 * @param v_min               minimum value today
 * @param v_max               maximum value today
 * @param tv_min              tomorrow's minimum value
 * @param yv_sunset           yesterday's value at sunset
 * @param sunrise             time of sunrise today
 * @param sunset              time of sunset today
 * @param solarnoon_yest      time of solar noon yesterday
 * @param sunset_yest         time of sunset yesterday
 * @param sunrise_tom         time of sunrise tomorrow
 * @param abg[3]              alpha, beta, gamma values to use
 * @param hourly[24]          array of calculated output values by hour for today
 * @return                    value at sunset today
 */
double diurnal(double v_min, double v_max, double tv_min,
  double yv_sunset, double sunrise, double sunset,
  double solarnoon_yest, double sunset_yest, double sunrise_tom,
  const double *abg, double *hourly)
{
  const double c_alpha = abg[0];
  const double c_beta = abg[1];
  const double c_gamma = abg[2];
  const double solarnoon = (sunset - sunrise) / 2.0 + sunrise;
  const double t_min = sunrise + c_alpha;
  const double t_max = solarnoon + c_beta;
  const double v_sunset = v_min + (v_max - v_min) *
    sin((M_PI / 2.0) * ((sunset - t_min) / (t_max - t_min)));
  const double yt_min = solarnoon_yest + c_beta;
  const double time_min_tom = sunrise_tom + c_alpha;
  int h;
  for (h = 0; h < 24; ++h)
  {
    if (h <= t_min)
    {
      hourly[h] = v_min + (yv_sunset - v_min) *
        exp(c_gamma * ((h + 24 - sunset_yest) / (24 - sunset_yest + t_min)));
    }
    else if (h > t_min && h <= sunset)
    {
      hourly[h] = v_min + (v_max - v_min) *
        sin((M_PI / 2.0) * ((h - t_min) / (t_max - t_min)));
    }
    else
    {
      /* h > sunset && h < 24 + t_min tomorrow */
      hourly[h] = tv_min + (v_sunset - tv_min) *
        exp(c_gamma * ((h - sunset) / (24 - sunset + time_min_tom)));
    }
  }
  return v_sunset;
}
