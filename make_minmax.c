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
float temp_min_max(float temp_noon, float rh_noon, float* temp_min, float* temp_max)
{
  float range = 17 - 0.16 * rh_noon + 0.22 * temp_noon;
  if ((temp_noon < 3 && rh_noon == 100) || range < 2)
  {
    *temp_max = temp_noon + (range / 2.0);
    *temp_min = temp_noon - (range / 2.0);
  }
  else if (range > 2)
  {
    *temp_max = temp_noon + 2;
    *temp_min = *temp_max - range;
  }
}

int main(int argc, char* argv[])
{
  if (argc != 3)
  {
    printf("Command line:   %s <input file> <output file>\n\n", argv[0]);
    printf("INPUT FILE format must be DAILY weather data, comma seperated and take the form\n");
    printf("Latitude,Longitude,YEAR,MONTH,DAY,HOUR,Temperature(C),Relative_humidity(%%),Wind_speed(km/h),Rainfall(mm)\n\n");
    printf("All times should be local standard time\n");
    exit(1);
  }
  printf("Opening input file >>> %s   \n", argv[1]);
  FILE* inp = fopen(argv[1], "r");
  if (NULL == inp)
  {
    printf("\n\n ***** FILE  %s  does not exist\n", argv[1]);
    exit(1);
  }
  /*  CSV headers */
  const char* header = "lat,long,year,mon,day,hour,temp,rh,wind,rain";
  check_header(inp, header);
  FILE* out = fopen(argv[2], "w");
  fprintf(out, "%s\n", "lat,long,year,mon,day,hour,temp_min,temp_max,rh_min,rh_max,wind_min,wind_max,rain");
  struct row cur;
  int err = read_row(inp, &cur);
  while (err > 0)
  {
    if (12 != cur.hour)
    {
      printf("Expected daily weather (hour value should be 12 but got %d)\n", cur.hour);
      exit(1);
    }
    float temp_min;
    float temp_max;
    temp_min_max(cur.temp, cur.rh, &temp_min, &temp_max);
    float q = findQ(cur.temp, cur.rh);
    float rh_min = findrh(q, temp_max);
    if (rh_min < 0)
    {
      rh_min = 0;
    }
    float rh_max = findrh(q, temp_min);
    if (rh_max > 100)
    {
      rh_max = 100;
    }
    float wind_min = 0.15 * cur.wind;
    float wind_max = 1.25 * cur.wind;
    fprintf(out,
            "%.4f,%.4f,%4d,%02d,%02d,%02d,%.1f,%.1f,%.0f,%.0f,%.1f,%.1f,%.1f\n",
            cur.lat,
            cur.lon,
            cur.year,
            cur.mon,
            cur.day,
            cur.hour,
            temp_min,
            temp_max,
            rh_min,
            rh_max,
            wind_min,
            wind_max,
            cur.rain);
    printf("%.4f,%.4f,%4d,%02d,%02d,%02d,%.1f,%.1f,%.0f,%.0f,%.1f,%.1f,%.1f\n",
           cur.lat,
           cur.lon,
           cur.year,
           cur.mon,
           cur.day,
           cur.hour,
           temp_min,
           temp_max,
           rh_min,
           rh_max,
           wind_min,
           wind_max,
           cur.rain);
    err = read_row(inp, &cur);
  } /* end the main while(err>0)  */
  fclose(inp);
  fclose(out);
  return 0;
}
