/*
inputs daily weather stream
outputs min/max weather stream
*/

#include <stdio.h>
#include <math.h>
#include <float.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"

/*  This function is for the method that takes old traditional 1pm weather (labelled here as noon)
    and estimates a diurnal temperature range to fit into the scheme that flows into the max/min method
    Temp input in Celsius   RH input in Percent.   These should be that traditional 1pm values
     Written as a function to enable upgrading later if needs be
*/
void temp_min_max(double temp_noon, double rh_noon, double *temp_min, double *temp_max)
{
  double temp_range = 17 - 0.16 * rh_noon + 0.22 * temp_noon;
  if (temp_range <= 2)
  {
    *temp_max = temp_noon + 1;
    *temp_min = temp_noon - 1;
  }
  else
  {
    *temp_max = temp_noon + 2;
    *temp_min = *temp_max - temp_range;
  }
}

int main(int argc, char *argv[])
{
  /*  CSV headers */
  static const char *header = "lat,long,yr,mon,day,temp,rh,ws,prec";
  static const char *header_out = "lat,long,yr,mon,day,temp_min,temp_max,rh_min,rh_max,ws_min,ws_max,prec";
  if (argc != 3)
  {
    printf("Command line:   %s <input file> <output file>\n\n", argv[0]);
    printf("INPUT FILE format must be DAILY weather data, comma seperated and take the form\n");
    printf("%s\n\n", header);
    printf("All times should be local standard time\n");
    exit(1);
  }
  printf("Opening input file >>> %s   \n", argv[1]);
  FILE *inp = fopen(argv[1], "r");
  if (NULL == inp)
  {
    printf("\n\n ***** FILE  %s  does not exist\n", argv[1]);
    exit(1);
  }
  check_header(inp, header);
  FILE *out = fopen(argv[2], "w");
  fprintf(out, "%s\n", header_out);
  struct row_daily cur;
  int err = read_row_daily(inp, &cur);
  while (err > 0)
  {
    double temp_min;
    double temp_max;
    temp_min_max(cur.temp, cur.rh, &temp_min, &temp_max);
    double q = findQ(cur.temp, cur.rh);
    double rh_min = _min(100, _max(0, findrh(q, temp_max)));
    double rh_max = _min(100, _max(0, findrh(q, temp_min)));
    double ws_min = 0.15 * cur.wind;
    double ws_max = 1.25 * cur.wind;
    save_csv(out,
             "%.4f,%.4f,%4d,%02d,%02d,%.1f,%.1f,%.0f,%.0f,%.1f,%.1f,%.2f\n",
             cur.lat,
             cur.lon,
             cur.year,
             cur.mon,
             cur.day,
             temp_min,
             temp_max,
             rh_min,
             rh_max,
             ws_min,
             ws_max,
             cur.rain);
    printf("%.4f,%.4f,%4d,%02d,%02d,%.1f,%.1f,%.0f,%.0f,%.1f,%.1f,%.2f\n",
           cur.lat,
           cur.lon,
           cur.year,
           cur.mon,
           cur.day,
           temp_min,
           temp_max,
           rh_min,
           rh_max,
           ws_min,
           ws_max,
           cur.rain);
    err = read_row_daily(inp, &cur);
  } /* end the main while(err>0)  */
  fclose(inp);
  fclose(out);
  return 0;
}
