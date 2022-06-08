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

void main(int argc, char *argv[]){
  FILE *inp, *out;
  int err, year, mon, day, hour;

  float lat,lon, temp,rh,wind,rain;
  char a[1]; /* this is declared as an array just to make it a pointer ...for reading commas easily*/

   if(argc != 3){
       printf("Command line:   %s <input file> <output file>\n\n", argv[0]);
       printf("INPUT FILE format must be DAILY weather data, comma seperated and take the form\n");
       printf("Latitude,Longitude,YEAR,MONTH,DAY,HOUR,Temperature(C),Relative_humitiy(%%),Wind_speed(km/h),Rainfall(mm)\n\n");
       printf("All times should be local standard time\n");
       exit(1);
   }

   inp=fopen(argv[1],"r");
   printf("Opening input file >>> %s   \n",argv[1]);
   if(inp==NULL){printf("\n\n ***** FILE  %s  does not exist\n",argv[1]);exit(1);}
   out=fopen(argv[2],"w");

  /*  CSV headers */
   const char* header = "lat,long,year,mon,day,hour,temp,rh,wind,rain";
   check_header(inp, header);
   fprintf(out, "%s\n", "lat,long,year,mon,day,hour,temp_min,temp_max,rh_min,rh_max,wind_min,wind_max,rain");

   err=fscanf(inp,"%f%c%f%c%d%c%d%c%d%c%d%c%f%c%f%c%f%c%f",&lat,a,&lon,a,&year,a,&mon,a,&day,a,&hour,a,&temp,a,&rh,a,&wind,a,&rain);

         printf("%d %d %d %d  %5.1f  %5.1f  %5.1f %5.1f %d\n", year, mon , day,hour,temp,rh,wind,rain,err);
   while(err>0){
     if (12 != hour)
     {
       printf("Expected daily weather (hour value should be 12 but got %d)\n", hour);
       exit(1);
     }
     check_inputs(temp, rh, wind, rain);
     float temp_min = temp - 15;
     float temp_max = temp + 2;
     float q = findQ(temp, rh);
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
     float wind_min = 0.15 * wind;
     float wind_max = 1.25 * wind;
     fprintf(out,"%.4f,%.4f,%4d,%02d,%02d,%02d,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f\n",
      lat,lon,year,mon,day,hour,temp_min,temp_max,rh_min,rh_max,wind_min,wind_max,rain);

     printf("%.4f,%.4f,%4d,%02d,%02d,%02d,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f\n",
      lat,lon,year,mon,day,hour,temp_min,temp_max,rh_min,rh_max,wind_min,wind_max,rain);
     err=fscanf(inp,"%f%c%f%c%d%c%d%c%d%c%d%c%f%c%f%c%f%c%f",&lat,a,&lon,a,&year,a,&mon,a,&day,a,&hour,a,&temp,a,&rh,a,&wind,a,&rain);
   }  /* end the main while(err>0)  */

  /* printf("output has been written to>>> %s\n",argv[6]); */
  fclose(inp);
  fclose(out);
}
