/*
inputs daily weather stream
outputs min/max weather stream
*/

#include <stdio.h>
#include <math.h>
#include <float.h>
#include <stdlib.h>
#include <string.h>

const float pi = 3.14159265;
const float C_TEMP[3] = {0.3, 2.0, -3.3};
const float C_RH[3] = {0.4, 2.0, -3.4};
const float C_WIND[3] = {0.3, 3.5, -3.3};

struct row {
  float lat, lon;
  int year, mon, day, hour;
  float temp_min, temp_max, rh_min, rh_max, wind_min, wind_max, rain;
};

float findQ(float temp, float rh);
float findrh(float q, float temp);
float sun(float lat,float lon, int mon,int day,int hour, int timezone, float *sunrise, float *sunset);
float sun_julian(float lat,float lon, int jd,int hour,int timezone, float *sunrise, float *sunset);
int julian(int mon, int day);
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

  /*  CSV headers */
   fprintf(out, "%s\n", "lat,long,year,mon,day,hour,temp,rh,wind,rain");

   /* check that the header matches what is expected */
   const int header_len = strlen(header);
   int i;
   /* do this one character at a time because unsure how long line would be if we used %s */
   for(i = 0; i < header_len; ++i)
   {
     fscanf(inp, "%c", a);
     if (a[0] != header[i])
     {
       printf("Expected columns to be '%s'\n", header);
       exit(1);
     }
   }
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


float sun(float lat,float lon, int mon,int day,int hour,int timezone, float *sunrise, float *sunset)
{
    int jd = julian(mon, day);
    return sun_julian(lat, lon, jd, hour, timezone, sunrise, sunset);
}

float sun_julian(float lat,float lon, int jd,int hour,int timezone, float *sunrise, float *sunset)
/*

this routine approximately calcualtes sunrise and sunset and daylength
REally any routine like this could be used,  some are more precise than others.

It takes in:
latitude:   in degrees north -  poistive number
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

{
  float zenith;
  float dechour=12.0,fracyear,eqtime,decl,halfday,hourangle,tst,timeoffset,solrad;

  fracyear=2.0*pi/365.0*( (float)(jd)-1.0+((float)(dechour)-12.0)/24.0);

  eqtime = 229.18*( 0.000075+ 0.001868*cos(fracyear) - 0.032077*sin (fracyear) - 0.014615*cos(2.0*fracyear) - 0.040849*sin(2.0*fracyear) );

  decl=0.006918-0.399912*cos(fracyear) + 0.070257*sin(fracyear)
    - 0.006758*cos(fracyear*2.0)+0.000907*sin(2.0*fracyear) - 0.002697*cos(3.0*fracyear) + 0.00148*sin(3.0*fracyear);
  timeoffset=eqtime+4*lon-60*timezone;

  tst=(float)hour*60.0+timeoffset;
  hourangle=tst/4-180;
  zenith=acos(sin(lat*pi/180)*sin(decl)+cos(lat*pi/180)*cos(decl)*cos(hourangle*pi/180) );
  solrad=0.95*cos(zenith);
  if(solrad<0)solrad=0.0;
  printf(" SOLAR: %d  %d fracyear=%f dec=%f  toff=%f  tst=%fha=%f zen=%f  solrad=%f\n",jd,hour,fracyear,decl,timeoffset,tst,hourangle,zenith,solrad);

  zenith=90.833*pi/180.0;


  halfday=180.0/pi*acos( cos(zenith)/(cos(lat*pi/180.0)*cos(decl))-tan(lat*pi/180.0)*tan(decl) );
  *sunrise=(720.0-4.0*(lon+halfday)-eqtime)/60+timezone;
  *sunset=(720.0-4.0*(lon-halfday)-eqtime)/60+timezone;
  return solrad;

}

int julian(int mon, int day)
{
  int month[13]={0,31,59,90,120,151,181,212,242,273,304,334,365};
  return month[mon-1]+day;

}
