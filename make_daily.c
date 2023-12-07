/*
inputs full hourly weather stream only.
outputs daily weather stream
*/

#include <stdio.h>
#include <math.h>
#include <float.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"

int main(int argc, char* argv[])
{
  /*  CSV headers */
  static const char* header = "lat,long,yr,mon,day,hr,temp,rh,ws,prec";
  static const char* header_out = "lat,long,yr,mon,day,temp,rh,ws,prec";
  static const int LST_NOON = 12;
  double atemp[24] = {0.0};
  double arh[24] = {0.0};
  double awind[24] = {0.0};
  double arain[24] = {0.0};

  if (argc != 3)
  {
    printf("Command line:   %s <input file> <output file>\n\n", argv[0]);
    printf("INPUT FILE format must be HOURLY weather data, comma seperated and take the form\n");
    printf("%s\n\n", header);
    printf("All times should be local standard time\n");
    exit(1);
  }

  FILE* inp = fopen(argv[1], "r");
  printf("Opening input file >>> %s   \n", argv[1]);
  if (NULL == inp)
  {
    printf("\n\n ***** FILE  %s  does not exist\n", argv[1]);
    exit(1);
  }

  check_header(inp, header);

  FILE* out = fopen(argv[2], "w");
  fprintf(out, "%s\n", header_out);

  struct row cur;
  struct row old;
  int err = read_row(inp, &cur);

  double rain_pm = 0.0;
  double rain_am = 0.0;
  double rain_pm_old = 0.0;
  while (err > 0)
  {
    old = cur;
    /* loading a new 24 hour period, so reset rain */
    rain_am = 0.0;
    rain_pm_old = rain_pm;
    rain_pm = 0.0;
    while (err > 0 && old.mon == cur.mon && old.day == cur.day)
    {
      atemp[cur.hour] = cur.temp;
      arh[cur.hour] = cur.rh;
      awind[cur.hour] = cur.ws;
      arain[cur.hour] = cur.rain;
      if (cur.hour <= LST_NOON)
      {
        rain_am += cur.rain;
      }
      else
      {
        rain_pm += cur.rain;
      }
      err = read_row(inp, &cur);
    } /* end the while to read thru a day */
    double rain24 = rain_am + rain_pm_old;
    fprintf(out,
            "%.4f,%.4f,%4d,%02d,%02d,%.1f,%.0f,%.1f,%.1f\n",
            old.lat,
            old.lon,
            old.year,
            old.mon,
            old.day,
            atemp[LST_NOON],
            arh[LST_NOON],
            awind[LST_NOON],
            rain24);
    printf("%.4f,%.4f,%4d,%02d,%02d,%.1f,%.0f,%.1f,%.1f\n",
           old.lat,
           old.lon,
           old.year,
           old.mon,
           old.day,
           atemp[LST_NOON],
           arh[LST_NOON],
           awind[LST_NOON],
           rain24);
  } /* end the main while(err>0)  */
  fclose(inp);
  fclose(out);
  return 0;
}
