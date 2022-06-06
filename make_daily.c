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

void main(int argc, char *argv[]){
  FILE *inp, *out;
  int err, year, mon, day, hour,h, oyear,omon,oday,ohour;

  float lat,lon, olat,olon,temp,rh,ws,rain,atemp[24],arh[24],aws[24],arain[24];
  float minRH,mcgmc,gfmc,solar,rain24,Wdmc24,Wdc24, sunrise, sunset, daylength, ffmcRF, dmcDryFrac,dcDryFrac ;
  float reff,b,rw,smi;
  float DELTA_mcdmcrain24, DELTA_DCrain24, solprop;
  char a[1]; /* this is declared as an array just to make it a pointer ...for reading commas easily*/

   if(argc != 3){
       printf("Command line:   %s <input file> <output file>\n\n", argv[0]);
       printf("INPUT FILE format must be HOURLY weather data, comma seperated and take the form\n");
       printf("Latitude,Longitude,YEAR,MONTH,DAY,HOUR,Temperature(C),Relative_humitiy(%%),Wind_speed(km/h),Rainfall(mm)\n\n");
       printf("All times should be local standard time\n");
       exit(1);
   }

   inp=fopen(argv[1],"r");
   printf("Openning input file >>> %s   \n",argv[1]);
   if(inp==NULL){printf("/n/n ***** FILE  %s  does not exist\n",argv[1]);exit(1);}
   out=fopen(argv[2],"w");

  /*  CSV headers */
   const char* header = "lat,long,year,mon,day,hour,temp,rh,wind,rain";
   check_header(inp, header);
   fprintf(out, "%s\n", header);

   for(hour=0;hour<24;hour++){
     atemp[hour]=0.0;arh[hour]=0.0;aws[hour]=0.0;arain[hour]=0.0;
   }
   err=fscanf(inp,"%f%c%f%c%d%c%d%c%d%c%d%c%f%c%f%c%f%c%f",&lat,a,&lon,a,&year,a,&mon,a,&day,a,&hour,a,&temp,a,&rh,a,&ws,a,&rain);
   olat=lat;olon=lon;oyear=year;omon=mon;oday=day;ohour=hour;

         printf("%d %d %d %d  %5.1f  %5.1f  %5.1f %5.1f %d\n", oyear, omon , oday,hour,atemp[hour],arh[hour],aws[hour],arain[hour],err);
   float rain_pm = 0.0;
   float rain_am = 0.0;
   float rain_pm_old = 0.0;
   while(err>0){
     /* loading a new 24 hour period, so reset rain */
     rain_am = 0.0;
     rain_pm_old = rain_pm;
     rain_pm = 0.0;
     while(err>0 && omon==mon && oday==day){  /* This loads up 24 hours at a time  ...need to for this version*/
         atemp[hour]=temp;
         arh[hour]=rh;
         aws[hour]=ws;
         arain[hour]=rain;
         if (hour <= 12)
         {
           rain_am += rain;
         } else {
           rain_pm += rain;
         }
         err=fscanf(inp,"%f%c%f%c%d%c%d%c%d%c%d%c%f%c%f%c%f%c%f",&lat,a,&lon,a,&year,a,&mon,a,&day,a,&hour,a,&temp,a,&rh,a,&ws,a,&rain);
         /* printf("%d %d %d %d  %5.1f  %5.1f  %5.1f %5.1f %d\n", oyear, omon , oday,ohour,atemp[ohour],arh[ohour],aws[ohour],arain[ohour],err); */
     }  /* end the while to read thru a day */
     rain24 = rain_am + rain_pm_old;
     /* now go through the loop again ,  calcuate houlry values AND output*/
     h = 12;
     fprintf(out,"%.4f,%.4f,%4d,%02d,%02d,%02d,%g,%g,%g,%g\n",
      olat,olon,oyear,omon,oday,h,atemp[h],arh[h],aws[h],rain24);

     printf("%.4f,%.4f,%4d,%02d,%02d,%02d,%5.1f,%3.0f,%5.1f,%5.1f\n",
      olat,olon,oyear,omon,oday,h,atemp[h],arh[h],aws[h],rain24);
     oyear=year;omon=mon;oday=day;ohour=hour;olat=lat;olon=lon;
   }  /* end the main while(err>0)  */

  /* printf("output has been written to>>> %s\n",argv[6]); */
  fclose(inp);
  fclose(out);
}
