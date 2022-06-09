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


/* A row from the input file */
struct row {
  float lat, lon;
  int year, mon, day, hour;
  float temp, rh, wind, rain;
};

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

int main(int argc, char *argv[]) {
  float atemp[24] = {0.0};
  float arh[24] = {0.0};
  float awind[24] = {0.0};
  float arain[24] = {0.0};

  if(argc != 3) {
    printf("Command line:   %s <input file> <output file>\n\n", argv[0]);
    printf("INPUT FILE format must be HOURLY weather data, comma seperated and take the form\n");
    printf("Latitude,Longitude,YEAR,MONTH,DAY,HOUR,Temperature(C),Relative_humitiy(%%),Wind_speed(km/h),Rainfall(mm)\n\n");
    printf("All times should be local standard time\n");
    exit(1);
  }

  FILE* inp = fopen(argv[1], "r");
  printf("Opening input file >>> %s   \n", argv[1]);
  if (NULL == inp) { printf("\n\n ***** FILE  %s  does not exist\n",argv[1]); exit(1); }

  /*  CSV headers */
  const char* header = "lat,long,year,mon,day,hour,temp,rh,wind,rain";
  check_header(inp, header);

  FILE* out=fopen(argv[2],"w");
  fprintf(out, "%s\n", header);

  struct row cur;
  struct row old;
  int err = read_row(inp, &cur);

  float rain_pm = 0.0;
  float rain_am = 0.0;
  float rain_pm_old = 0.0;
  while (err > 0) {
    old = cur;
    /* loading a new 24 hour period, so reset rain */
    rain_am = 0.0;
    rain_pm_old = rain_pm;
    rain_pm = 0.0;
    while(err > 0 && old.mon == cur.mon && old.day == cur.day) {
      atemp[cur.hour] = cur.temp;
      arh[cur.hour] = cur.rh;
      awind[cur.hour] = cur.wind;
      arain[cur.hour] = cur.rain;
      if (cur.hour <= 12) {
        rain_am += cur.rain;
      } else {
        rain_pm += cur.rain;
      }
      err = read_row(inp, &cur);
    }  /* end the while to read thru a day */
    float rain24 = rain_am + rain_pm_old;
    int h = 12;
    fprintf(out,"%.4f,%.4f,%4d,%02d,%02d,%02d,%.1f,%.0f,%.1f,%.1f\n",
            old.lat,old.lon,old.year,old.mon,old.day,h,atemp[h],arh[h],awind[h],rain24);
    printf("%.4f,%.4f,%4d,%02d,%02d,%02d,%.1f,%.0f,%.1f,%.1f\n",
           old.lat,old.lon,old.year,old.mon,old.day,h,atemp[h],arh[h],awind[h],rain24);
  }  /* end the main while(err>0)  */
  fclose(inp);
  fclose(out);
  return 0;
}
