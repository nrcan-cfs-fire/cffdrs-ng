/*
inputs min/max weather stream
outputs hourly weather stream
*/

#include <stdio.h>
#include <math.h>
#include <float.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"

/* Alpha/Beta/Gamma coefficients for Temperature, RH, and Wind Speed */
const float C_TEMP[3] = {0.3, 2.0, -3.3};
const float C_RH[3] = {0.4, 2.0, -3.4};
const float C_WIND[3] = {0.3, 3.5, -3.3};

/* A row from the input file */
struct row {
  float lat, lon;
  int year, mon, day, hour;
  float temp_min, temp_max, rh_min, rh_max, wind_min, wind_max, rain;
};

float diurnal(float v_min, float v_max, float tv_min, float yv_sunset, float sunrise, float sunset, float solarnoon_yest, float sunset_yest, float sunrise_tom, const float* abg, float* hourly);

int read_row(FILE *inp, struct row* r)
{
  char a[1]; /* this is declared as an array just to make it a pointer ...for reading commas easily*/
  return fscanf(inp,"%f%c%f%c%d%c%d%c%d%c%d%c%f%c%f%c%f%c%f%c%f%c%f%c%f",
  &r->lat,a,&r->lon,a,&r->year,a,&r->mon,a,&r->day,a,&r->hour,a,&r->temp_min,a,&r->temp_max,a,&r->rh_min,a,&r->rh_max,a,&r->wind_min,a,&r->wind_max,a,&r->rain);
}

void main(int argc, char *argv[]){
  const char* header = "lat,long,year,mon,day,hour,temp_min,temp_max,rh_min,rh_max,wind_min,wind_max,rain";
  FILE *inp, *out;
  int err;

  float TZadjust;
  char a[1]; /* this is declared as an array just to make it a pointer ...for reading commas easily*/

   if(argc != 4){
       printf("Command line:   %s <local GMToffset> <input file> <output file>\n\n", argv[0]);
       printf("<local GMToffset> is the off of Greenich mean time (for Eastern = -5  Central=-6   MT=-7  PT=-8 )  \n");
       printf("INPUT FILE format must be DAILY min/max weather data, comma seperated and take the form\n");
       printf("%s\n\n", header);
       printf("All times should be local standard time\n");
       exit(1);
   }

   inp=fopen(argv[2],"r");
   printf("Openning input file >>> %s   \n",argv[2]);
   if(inp==NULL){printf("/n/n ***** FILE  %s  does not exist\n",argv[2]);exit(1);}
   out=fopen(argv[3],"w");

   TZadjust=atoi(argv[1]);
   if(TZadjust <-9 || TZadjust> -2 ){ printf("/n *****   LOCal time zone adjustment must be vaguely in CAnada so between -9 and -2 \n"); exit(1);}

  check_header(inp, header);
  /*  CSV headers */
   fprintf(out, "%s\n", "lat,long,year,mon,day,hour,temp,rh,wind,rain");

   struct row tom;
   struct row cur;
   struct row yday;
   
   err=read_row(inp, &cur);
   if (!(err > 0))
   {
        printf("Unable to read first row of indices\n");
        exit(1);
   }
   /* use today's values for yesterday because we need to do something */
   yday = cur;
   float solarnoon_yest = 0.0;
   float sunset_yest = 0.0;
   float ytemp_sunset = 0.0;
   float yrh_sunset = 0.0;
   float ywind_sunset = 0.0;
   int done_first = 0;
   while(err>0){
     if (12 != cur.hour)
     {
       printf("Expected daily weather (hour value should be 12 but got %d)\n", cur.hour);
       exit(1);
     }
     if (done_first)
     {
       err = read_row(inp, &tom);
     } else {
       yday = cur;
       tom = cur;
     }
     if (!(err > 0))
     {
       /* error but we still want to figure out today's values */
       tom = cur;
     }
     printf("%d %d %d %d  %5.1f  %5.1f %5.1f %5.1f %5.1f %5.1f %5.1f\n",
            cur.year, cur.mon, cur.day, cur.hour,
            cur.temp_min, cur.temp_max,
            cur.rh_min, cur.rh_max,
            cur.wind_min, cur.wind_max,
            cur.rain);
     int jd = julian(cur.mon, cur.day);
     float sunrise, sunset;
     float solar=sun_julian(cur.lat,cur.lon,jd,cur.hour,TZadjust,&sunrise,&sunset );
     float solarnoon = (sunset - sunrise) / 2.0 + sunrise;
     float sunrise_tom, sunset_tom;
     float solar_tom=sun_julian(cur.lat,cur.lon,jd+1,cur.hour,TZadjust,&sunrise_tom,&sunset_tom );
     float atemp[24], arh[24], awind[24], arain[24];
     int h;
     for (h = 0; h < 24; ++h)
     {
       atemp[h] = 0.0;
       arh[h] = 0.0;
       awind[h] = 0.0;
       arain[h] = 0.0;
     }
     ytemp_sunset = diurnal(cur.temp_min, cur.temp_max, tom.temp_min, ytemp_sunset, sunrise, sunset, solarnoon_yest, sunset_yest, sunrise_tom, C_TEMP, atemp);
     yrh_sunset = diurnal(1.0 - cur.rh_max/100.0, 1.0-cur.rh_min/100.0, 1.0-tom.rh_max/100.0, yrh_sunset, sunrise, sunset, solarnoon_yest, sunset_yest, sunrise_tom, C_RH, arh);
     ywind_sunset = diurnal(cur.wind_min, cur.wind_max, tom.wind_min, ywind_sunset, sunrise, sunset, solarnoon_yest, sunset_yest, sunrise_tom, C_WIND, awind);
     arain[7] = cur.rain;
     if (done_first)
     {
       for (h = 0; h < 24; ++h)
       {
          arh[h] = 100 * (1.0 - arh[h]);
          fprintf(out,"%.4f,%.4f,%4d,%02d,%02d,%02d,%.1f,%.0f,%.1f,%.1f\n",
                  cur.lat,cur.lon,cur.year,cur.mon,cur.day,h,atemp[h],arh[h],awind[h],arain[h]);
       }
     } else {
       done_first = 1;
     }
     /* move today to yesterday's variables */
     yday = cur;
     solarnoon_yest = solarnoon;
     sunset_yest = sunset;
     /* advance to the next day */
     cur = tom;
   }  /* end the main while(err>0)  */

  /* printf("output has been written to>>> %s\n",argv[6]); */
  fclose(inp);
  fclose(out);
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
float diurnal(float v_min, float v_max, float tv_min, float yv_sunset, float sunrise, float sunset,
              float solarnoon_yest, float sunset_yest, float sunrise_tom, const float* abg, float* hourly)
{
  const float c_alpha = abg[0];
  const float c_beta = abg[1];
  const float c_gamma = abg[2];
  const float solarnoon = (sunset - sunrise) / 2.0 + sunrise;
  const float t_min = sunrise + c_alpha;
  const float t_max = solarnoon + c_beta;
  const float v_sunset = v_min + (v_max - v_min) * sin((pi / 2.0)*((sunset - t_min)/(t_max - t_min)));
  const float yt_min = solarnoon_yest + c_beta;
  const float time_min_tom = sunrise_tom + c_alpha;
  int h;
  for (h = 0; h < 24; ++h)
  {
    if (h <= t_min)
    {
      hourly[h] = v_min + (yv_sunset - v_min) * exp(c_gamma * ((h + 24 - sunset_yest)/(24 - sunset_yest + t_min)));
    } else if (h > t_min && h <= sunset) {
      hourly[h] = v_min + (v_max - v_min) * sin((pi / 2.0)*((h - t_min)/(t_max - t_min)));
    } else {
      /* h > sunset && h < 24 + t_min tomorrow */
      hourly[h] = tv_min + (v_sunset - tv_min) * exp(c_gamma * ((h - sunset)/(24 - sunset + time_min_tom)));
    }
  }
  return v_sunset;
}
