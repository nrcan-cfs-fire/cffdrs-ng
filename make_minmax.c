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
    float temp_min = cur.temp - 15;
    float temp_max = cur.temp + 2;
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
    fprintf(out, "%.4f,%.4f,%4d,%02d,%02d,%02d,%.1f,%.1f,%.0f,%.0f,%.1f,%.1f,%.1f\n", cur.lat, cur.lon, cur.year, cur.mon, cur.day, cur.hour, temp_min, temp_max, rh_min, rh_max, wind_min, wind_max, cur.rain);
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
