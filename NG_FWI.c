
/*  Phase1  NG-FWI.c

inputs full hourly weather sttream only.
new hourly DMC and DC
calcuats solar radiation from lat/long
grass moisture, grass spread index and grass fire weahter index added.\

 THis is only set up for ONE station at a time.   And ONE year really.

version 0.9    (still testing)

bmw/2021
*/

#include <stdio.h>
#include <math.h>
#include <float.h>
#include <stdlib.h>
#include <string.h>

/*    gcc -o ngfwi NG_FWI.c -lm -std=c90 */
/*    ./ngfwi -6 85 6 15 ../data/BAK2018_hourly.csv test.csv */

float hourly_ffmc(float temp,float rh,float wind,float rain,float oldffmc);
float hourly_DMC (float Temp, float rh, float ws, float rain, int mon, float lastdmc, float DryFrac, float rain24,float DELTA,float tnoon,float rhnoon);
float hourly_DC(float Temp, float rh,float ws, float rain, float lastdc, int mon, float rain24, float dryfrac, float DELTA, float temp12);
float ISIcalc(float wind, float ffmc);
float BUIcalc (float dmc, float dc);
float FWIcalc(float ISI, float BUI);
float hourly_gfmc(float temp,float rh,float wind,float rain,float lastgmc,float solrad,float time);
float grassISI (float wind, float GMC, float cur);
float grassFWI (float gsi, float grassload);
float DMCdryingweight (float T,float RH,float ws);
float DCdryingweight (float T,float RH,float ws);

float sun(float lat,float lon, int mon,int day,int hour, int timezone, float *sunrise, float *sunset);
int julian(int mon, int day);

void main(int argc, char *argv[]){
  FILE *inp, *out;
  int err, year, mon, day, hour,h, oyear,omon,oday,ohour;
  int TZadjust;

  float grassfuelload=0.35,  maxsolprop=0.85, percent_cured=100.0;

  float lat,lon, olat,olon,temp,rh,ws,rain,lastffmc,lastdmc,lastdc,lastmcgmc,dmc,dc,ffmc,isi,bui,fwi,gsi,gfwi,atemp[24],arh[24],aws[24],arain[24];
  float minRH,mcgmc,gfmc,solar,rain24,Wdmc24,Wdc24, sunrise, sunset, daylength, ffmcRF, dmcDryFrac,dcDryFrac ;
  float reff,b,rw,smi;
  float DELTA_mcdmcrain24, DELTA_DCrain24, solprop;
  char a[1]; /* this is declared as an array just to make it a pointer ...for reading commas easily*/

   if(argc!=7){
       printf("Command line:   %s <local GMToffset> <starting FFMC> <starting DMC> <starting DC> <input file> <output file>\n\n", argv[0]);
       printf("<local GMToffset> is the off of Greenich mean time (for Eastern = -5  Central=-6   MT=-7  PT=-8 )  \n");
       printf("All times should be local standard time\n");
       printf("INPUT FILE format must be HOURLY weather data, comma seperated and take the form\n");
       printf("Latitude,Longitude,YEAR,MONTH,DAY,HOUR,Temperature(C),Relative_humitiy(%%),Wind_speed(km/h),Rainfall(mm)\n\n");
       exit(1);
   }

   inp=fopen(argv[5],"r");
   printf("Openning input file >>> %s   \n",argv[5]);
   if(inp==NULL){printf("/n/n ***** FILE  %s  does not exist\n",argv[5]);exit(1);}
   out=fopen(argv[6],"w");

  /*  CSV headers */
   fprintf(out,"year,mon,day,hour,temp,rh,wind,rain,ffmc,dmc,dc,isi,bui,fwi,gfmc,gsi,gfwi\n");

   TZadjust=atoi(argv[1]);
   if(TZadjust <-9 || TZadjust> -2 ){ printf("/n *****   LOCal time zone adjustment must be vaguely in CAnada so between -9 and -2 \n"); exit(1);}
   lastffmc=atof(argv[2]);
   if(lastffmc>101 || lastffmc<0){ printf(" /n/n *****   FFMC must be between 0 and 101 \n"); exit(1);}
   lastdmc=atof(argv[3]);
   if(lastdmc<0){ printf(" /n/n *****  starting DMC must be >=0  \n"); exit(1);}
   lastdc=atof(argv[4]);
   if(lastdc<0){ printf(" /n/n *****   starting DC must be >=0\n"); exit(1);}

   printf("TZ=%d    start ffmc=%f  dmc=%f\n",TZadjust,lastffmc,lastdmc);
   lastmcgmc=101-lastffmc;  /* approximation for a start up*/

   for(hour=0;hour<24;hour++){
     atemp[hour]=0.0;arh[hour]=0.0;aws[hour]=0.0;arain[hour]=0.0;
   }
   /* check that the header matches what is expected */
   const char* header = "lat,long,year,mon,day,hour,temp,rh,wind,rain";
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

   err=fscanf(inp,"%f%c%f%c%d%c%d%c%d%c%d%c%f%c%f%c%f%c%f",&lat,a,&lon,a,&year,a,&mon,a,&day,a,&hour,a,&temp,a,&rh,a,&ws,a,&rain);
   oyear=year;omon=mon;oday=day;ohour=hour;

         printf("%d %d %d %d  %5.1f  %5.1f  %5.1f %5.1f %d\n", oyear, omon , oday,hour,atemp[hour],arh[hour],aws[hour],arain[hour],err);

   while(err>0){
     while(err>0 && omon==mon && oday==day){  /* This loads up 24 hours at a time  ...need to for this version*/
         atemp[hour]=temp;
         arh[hour]=rh;
         aws[hour]=ws;
         arain[hour]=rain;
         err=fscanf(inp,"%f%c%f%c%d%c%d%c%d%c%d%c%f%c%f%c%f%c%f",&lat,a,&lon,a,&year,a,&mon,a,&day,a,&hour,a,&temp,a,&rh,a,&ws,a,&rain);
         /* printf("%d %d %d %d  %5.1f  %5.1f  %5.1f %5.1f %d\n", oyear, omon , oday,ohour,atemp[ohour],arh[ohour],aws[ohour],arain[ohour],err); */
     }  /* end the while to read thru a day */


     solar=sun(lat,lon,omon,oday,12,TZadjust,&sunrise,&sunset );
     daylength=sunset-sunrise;

     printf("here : %f %f  %d   %d %d  SUNrise=%5.2f  sunset=%5.2f\n",lat,lon,oyear,omon,oday,sunrise,sunset);

     /* an initial go thru the loop to summarize daily information*/
     for(h=0,rain24=0.0, minRH=100.0,Wdmc24=0.0,Wdc24=0.0; h<24; h++)
     {
         rain24+=arain[h];
         if(arh[h]<minRH)minRH=arh[h];
         if(h>=sunrise && h<=sunset)Wdmc24+=DMCdryingweight(atemp[h],arh[h],aws[h]);
         if(h>=sunrise && h<=sunset)Wdc24+=DCdryingweight(atemp[h],arh[h],aws[h]);
        }

     if(rain24>0.5) ffmcRF=(rain24-0.5)/rain24;   /* so we can use the old horly routine directly*/
     else ffmcRF=0.0;

     if(rain24>1.5){
        reff=(0.92*rain24-1.27);
        if(lastdmc<=33) b = 100.0/(0.5+0.3* lastdmc);
        else if(lastdmc<=65) b = 14.0-1.3*log(lastdmc);
        else b= 6.2*log(lastdmc)-17.2;
        DELTA_mcdmcrain24 = 1000.0*reff/(48.77+b*reff);  /* This is the change in MC (moisturecontent)  from FULL DAY's rain  */
     }
     else DELTA_mcdmcrain24=0.0;

     /* printf("  %02d-%02d: dDELTAmcdmc=%f   rain24=%f b=%f  lastdmc=%f  rain24=%f \n",mon,day,DELTA_mcdmcrain24, rain24,b,lastdmc,rain24); */

    if(rain24>2.8){
        rw = 0.83*rain24 - 1.27;
        smi = 800*exp(-lastdc/400);
        DELTA_DCrain24 =  -400.0*log(1.0+3.937*rw/smi);   /* TOTAL change for the TOTAL 24 hour rain from FWI1970 model  */
     }
     else DELTA_DCrain24=0.0;;



     /* now go through the loop again ,  calcuate houlry values AND output*/
     for(h=0; h<24; h++){
         solar=sun(lat,lon,omon,oday,h,TZadjust,&sunrise,&sunset );

         ffmc=hourly_ffmc(atemp[h],arh[h],aws[h],(arain[h]*ffmcRF),lastffmc);

         if(Wdmc24>0){
              if(h>=sunrise && h<=sunset) dmcDryFrac=DMCdryingweight(atemp[h],arh[h],aws[h]) / Wdmc24;
              else dmcDryFrac=0.0;
         }
         else{
             printf("   ********WDMC24===0 \n");
             if(h>=sunrise && h<=sunset)dmcDryFrac=1/(sunset-sunrise);  /* is VPD is 0 all day long --rh=100 all day -- then set drying fraction to be uniform */
             else dmcDryFrac=0.0;
         }
         /* for each hour, calculate a full day of drying at those conditions, and then use fraction of that */
         dmc=hourly_DMC(atemp[h],arh[h],aws[h],arain[h],omon,lastdmc,dmcDryFrac,rain24,DELTA_mcdmcrain24,atemp[12],arh[12] );

         if(Wdc24>0){
             if(h>=sunrise && h<=sunset) dcDryFrac = DCdryingweight(atemp[h],arh[h],aws[h])/Wdc24;
             else dcDryFrac=0.0;
         }
         else {
             printf("   ********WDC24===0 \n");
             if(h>=sunrise && h<=sunset) dcDryFrac=1/(sunset-sunrise); /* is VPD is 0 all day long --rh=100 all day -- then set drying fraction to be uniform */
             else dcDryFrac=0.0;
         }

         dc=hourly_DC(atemp[h],arh[h],aws[h],arain[h],lastdc,omon, rain24,dcDryFrac,DELTA_DCrain24,atemp[12] );

         isi=ISIcalc(aws[h],ffmc);
         bui=BUIcalc(dmc,dc);
         fwi=FWIcalc(isi,bui);

/* grass */
         if(minRH>=100) minRH=99.5;
         /*
         right now this is:
         if minimum RH for the calendar date is > 30 then calculate solar prop based on 2300 rh
         should probably be:
         if minimum RH for the calendar date is >30 then calculate solar prop based on rh for the hour
         or
         if rh > 30 then calculate solar prop based on rh for the hour
         or
         if minimum RH for the calendar date is >30 then calculate solar prop based on min rh for the day
         if(minRH>30) solprop=maxsolprop*(1.27-0.0111*rh);
         */
         if(minRH>30) solprop=maxsolprop*(1.27-0.0111*minRH);
         else solprop=maxsolprop;
         if (solprop<0)solprop=0;
         solar=solar*solprop;   /*  just reducing solar radiation a little off of full value */
         mcgmc=hourly_gfmc(atemp[h],arh[h],aws[h],arain[h],lastmcgmc,solar,1.0);
         gfmc=59.5*(250-mcgmc)/(147.2772277+mcgmc);
         gsi=grassISI(aws[h],mcgmc, percent_cured);
         gfwi=grassFWI( gsi, grassfuelload);
         fprintf(out,"%4d,%2d,%2d,%2d,%5.1f,%3.0f,%5.1f,%5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f\n",
          oyear,omon,oday,h,atemp[h],arh[h],aws[h],arain[h],ffmc,dmc,dc,isi,bui,fwi, gfmc,gsi,gfwi);

         printf("%4d,%2d,%2d,%2d,%5.1f,%3.0f,%5.1f,%5.1f,     %5.2f, %5.2f, %5.2f, %5.1f, %5.1f, %5.1f | %5.1f, %5.1f, %5.1f  %5.2f\n",
          oyear,omon,oday,h,atemp[h],arh[h],aws[h],arain[h],ffmc,dmc,dc,isi,bui,fwi, gfmc, gsi, gfwi, mcgmc);
         lastffmc=ffmc;lastdmc=dmc;lastdc=dc; lastmcgmc=mcgmc;
     }  /* end for(h=0; h<24; h++)   */
     oyear=year;omon=mon;oday=day;ohour=hour;olat=lat;olon=lon;
   }  /* end the main while(err>0)  */

  /* printf("output has been written to>>> %s\n",argv[6]); */
  fclose(inp);
  fclose(out);
}


float hourly_ffmc(float temp,float rh,float wind,float rain,float oldffmc)
/* this is the hourly ffmc routine given wx and previous ffmc */
{
  float rf=42.5,drf=0.0579;
  float mo,ed,ew,moew,moed,xm,a1,e,moe,xkd;
  mo=147.27723*(101-oldffmc)/(59.5+oldffmc);



  if(rain!=0.0)
   {
     if(mo>150) mo+= 0.0015*(mo-150)*(mo-150)*sqrt(rain)
               +rf*rain*exp(-100.0/(251-mo))*(1.0-exp(-6.93/rain));
       else mo+=rf*rain*exp(-100.0/(251-mo))*(1.0-exp(-6.93/rain));
     if(mo>250) mo=250;
   }
  ed=0.942*pow(rh,0.679)+(11.0*exp( (rh-100)/10.0))+0.18*(21.1-temp)*
     (1.0-1.0/exp(0.115*rh));
  moed=mo-ed;
  ew=0.618*pow(rh,0.753)+(10.0*exp((rh-100)/10.0))+0.18*(21.1-temp)*
     (1.0-1.0/exp(0.115*rh));
  moew=mo-ew;
  if (moed==0 || (moew>=0 && moed<0))
  {
    xm=mo;
    if(moed==0) e=ed;
    if(moew>=0) e=ew;
  }
  else
  {
    if( moed>0)
    {
      a1=rh/100;
      e=ed;
      moe=moed;
    }
    else
    {
      a1=(100.0-rh)/100.0;
      e=ew;
      moe=moew;
    }
   xkd=(0.424*(1-pow(a1,1.7))+(0.0694*sqrt(wind)*(1-pow(a1,8))));
   xkd=xkd*drf*exp(0.0365*temp);
   xm=e+moe*exp(-log(10.0)*xkd);
  }
  mo=xm;
  return ( 59.5*(250-xm)/(147.27723+xm) );
}


float sun(float lat,float lon, int mon,int day,int hour,int timezone, float *sunrise, float *sunset)
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
  float pi=3.14159265, zenith;
  float dechour=12.0,fracyear,eqtime,decl,halfday,hourangle,tst,timeoffset,solrad;
  int jd;


  jd=julian(mon, day);
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

float DMCdryingweight (float Temp,float RH,float ws)
/* this is set up to be flexibilty...but is current vapour pressure deficit */
{
    float es,vpd;
    es=6.1078*exp(17.269*Temp/(237.3+Temp));
    vpd=es*(1.0-RH/100.0)  ;
    return vpd;
}

float DCdryingweight (float Temp, float RH, float ws)
/* currently using the VPD method.....  */

{
    float es,vpd;
    es=6.1078*exp(17.269*Temp/(237.3+Temp));
    vpd=es*(1.0-RH/100.0)  ;
    return vpd;
}


float hourly_DMC ( float t, float rh, float ws, float rain, int mon, float lastdmc, float DryFrac, float rain24, float DELTA_MCrain, float tnoon,float rhnoon)
{
    float el[] = {6.5, 7.5, 9.0, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8.0, 7.0, 6.0};
    float mc,dmc,DELTA_dry;
     /* wetting FROM rain  */
     if(rain>0 && DELTA_MCrain>0.0){
        /* printf("rain=%f  change=%f lastdmc=%f\n",rain, DELTA_MCrain, lastdmc); */
        mc= 20.0+280.0/exp(0.023*lastdmc);
        mc+=DELTA_MCrain*(rain/rain24);  /*  the MC increase by the rain in this hour...  total * rain_hour/rain24*/
        lastdmc = 43.43*(5.6348-log(mc-20));

     }

     /*drying all day long too */
     if(tnoon<-1.1) tnoon=-1.1;

     DELTA_dry = 1.894*(tnoon+1.1)*(100.0-rhnoon)*el[mon-1]*0.0001;  /* full day of drying in old FWI/DMC  */

     /* printf("delta dmc, %f ,lastDMC,%f , frac,%f , fractional,%f\n",DELTA_mcrain,lastdmc, DryFrac, (DELTA_dry*DryFrac)); */

     dmc= lastdmc + (DELTA_dry*DryFrac);

     return dmc;

}


float hourly_DC(float t, float rh,float ws, float rain, float lastdc, int mon, float rain24, float dryfrac, float DELTArain24,float temp12)
{
     float fl[] = {-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5.0, 2.4, 0.4, -1.6, -1.6};
     float dc,DELTAdry24;

     if(rain>0 && DELTArain24<0.0){
        lastdc+= DELTArain24*(rain/rain24);  /* (weight it by Rainhour/rain24 )*/
     }

     DELTAdry24 = (0.36*(temp12+2.8) + fl[mon-1])/2.0;   /* toatl dry for the DAY  */
     if (DELTAdry24<0.0) DELTAdry24 = 0.0;    /* the fix for winter negative DC change...shoulders*/

     dc = lastdc + DELTAdry24*dryfrac;   /* dry frac is VPD weighted value for the hour */
     if(dc<0) dc=0;
     return dc;
}
float ISIcalc(float ws, float ffmc)
{
    float isi,fm,sf;
    fm = 147.2773*(101.0-ffmc)/(59.5+ffmc);
    sf = 19.115*exp(-0.1386*fm)*(1.0+pow(fm,5.31)/4.93e07);
    isi = sf*exp(0.05039*ws);
    return isi;
}

float BUIcalc (float dmc, float dc)
{
    float bui,p,cc;
    if(dmc==0 && dc==0) bui=0;
    else bui = 0.8*dc*dmc/(dmc+0.4*dc);
    if(bui<dmc)
     {
       p=(dmc-bui)/dmc;
       cc = 0.92+pow( (0.0114*dmc),1.7);
       bui = dmc - cc*p;
       if(bui<0) bui=0;
     }
    return bui;
}

float FWIcalc(float isi, float bui)
{
    float bb,fwi;
    if(bui>80)bb= 0.1*isi*(1000.0/(25.0+108.64/exp(0.023*bui)));
    else bb= 0.1*isi*(0.626*pow(bui,0.809)+2.0);
    if (bb<=1)fwi=bb;
    else fwi=exp(2.72*pow(0.434*log(bb),0.647) );
   return fwi;
}

float hourly_gfmc(float temp,float rh,float wind,float rain,float lastmc,float solrad, float time)
/* MARK II of the model (2016) wth new solar rad model specific to grass

   Temp is temperature in C
   RH is realtive humidty in %
   wind is average wind speed in km/h
   rain is rainfall in mm
   solrad is kW/m2  (radiaiton reaching fuel)
   mo is the old grass fuel moisture   (not as a code value...so elimates the conversion to code)
   time - time between obs in HOURS


DRF of 1/16.1 comes from reducting the standard response time curve
at 26.7C, 20%RH, 2 km/h to 0.85hr.



bmw
*/
{
  float drf=0.389633,tf,rhf;
  float mo,ed,ew,moew,moed,xm,a1,e,moe,xkd;

  mo=lastmc;

  /* fuel temperature/humidity  */
  tf=temp+17.9*solrad*exp(-0.034*wind);   /* fuel temp from CEVW*/

  if(tf>temp)rhf=rh*6.107*pow(10.0,7.5*temp/(temp+237.0) )/(6.107*pow(10.0,7.5*tf/(tf+237.0) ));
  else rhf=rh;
  if(rain!=0)
   {
/*     mo+=rain*rf*exp(-100.0/(251.0-mo))*(1.0-exp(-6.93/rain));*/  /* old routine*/
     /* this new routine assumes layer is 0.3 kg/m2 so 0.3mm of rain adds +100%MC*/
     mo=mo+ rain/0.3*100.0;   /* *100 to convert to %...  *1/.3 because of 0.3mm=100%  */
     if(mo>250.0) mo=250.0;
   }
   ed=1.62*pow(rhf,0.532)+(13.7*exp( (rhf-100)/13.0))+0.27*(26.7-tf)*
     (1.0-1.0/exp(0.115*rhf));   /*GRASS EMC*/
   moed=mo-ed;
   ew=1.42*pow(rhf,0.512)+(12.0*exp((rhf-100)/18.0))+0.27*(26.7-tf)*
     (1.0-1.0/exp(0.115*rhf));     /*GRASS EMC*/
   moew=mo-ew;
  if (moed==0 || (moew>=0 && moed<0))
  {
    xm=mo;
    if(moed==0) e=ed;
    if(moew>=0) e=ew;
  }
  else
  {
    if( moed>0)
    {
      a1=rhf/100;
      e=ed;
      moe=moed;
    }
    else
    {
      a1=(100.0-rhf)/100.0;
      e=ew;
      moe=moew;
    }
   xkd=(0.424*(1-pow(a1,1.7))+(0.0694*sqrt(wind)*(1-pow(a1,8))));
   xkd=xkd*drf*exp(0.0365*tf);

    /* printf("tf=%8.4f rhf=%6.2f e=%4.1f mo=%5.2f xkd=%6.4f moed=%5.1f moew=%5.1f\n",tf,rhf,e,mo,xkd,moed,moew); */

   xm=e+moe*exp(-1.0*log(10.0)*xkd*time);
  }

  return ( xm );
}



float grassISI (float wind, float mc, float cur)
{
    float fw,fm,cf,GSI;

    if(wind<5)fw =( 0.054 +0.209*wind)*16.67;
    else fw = (1.1 + 0.715*(wind-5.0)*0.844)*16.67;

    if(mc<12)fm = exp(-0.108*mc);
    else if(mc<20.0 && wind<10.0) fm = 0.684-0.0342*mc;
    else if(mc<23.9 && wind>=10.0) fm = 0.547-0.0228*mc;
    else fm = 0;

    if(cur>20)cf = 1.034/(1+104*exp(-0.1*(cur-20)));
    else cf = 0.0;
    GSI  =  1.11* fw *fm * cf;
    return GSI;
}
float grassFWI (float gsi, float load)
{
    float Fint, GFWI, ros;
    ros=gsi/1.11;  /*  this just converts back to ROS in m/min*/
    Fint=300.0*load * ros;
    if (Fint>100)GFWI = ( log(Fint/60.0) ) / 0.14     ;
    else GFWI=Fint/25.0;
    return GFWI;
}

