
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
#include "util.h"

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

void main(int argc, char *argv[]){
  FILE *inp, *out;
  int err, h;
  int TZadjust;

  float grassfuelload=0.35,  maxsolprop=0.85, percent_cured=100.0;

  float lastffmc,lastdmc,lastdc,lastmcgmc,dmc,dc,ffmc,isi,bui,fwi,gsi,gfwi,atemp[24],arh[24],aws[24],arain[24];
  float minRH,mcgmc,gfmc,solar,rain24,Wdmc24,Wdc24, sunrise, sunset, daylength, ffmcRF, dmcDryFrac,dcDryFrac ;
  float reff,b,rw,smi;
  float DELTA_mcdmcrain24, DELTA_DCrain24, solprop;

   if(argc!=7){
       printf("Command line:   %s <local GMToffset> <starting FFMC> <starting DMC> <starting DC> <input file> <output file>\n\n", argv[0]);
       printf("<local GMToffset> is the off of Greenich mean time (for Eastern = -5  Central=-6   MT=-7  PT=-8 )  \n");
       printf("All times should be local standard time\n");
       printf("INPUT FILE format must be HOURLY weather data, comma seperated and take the form\n");
       printf("Latitude,Longitude,YEAR,MONTH,DAY,HOUR,Temperature(C),Relative_humidity(%%),Wind_speed(km/h),Rainfall(mm)\n\n");
       exit(1);
   }

   inp=fopen(argv[5],"r");
   printf("Opening input file >>> %s   \n",argv[5]);
   if(inp==NULL){printf("\n\n ***** FILE  %s  does not exist\n",argv[5]);exit(1);}
   out=fopen(argv[6],"w");

  /*  CSV headers */
   fprintf(out,"year,mon,day,hour,temp,rh,wind,rain,ffmc,dmc,dc,isi,bui,fwi,gfmc,gsi,gfwi\n");

   TZadjust=atoi(argv[1]);
   if(TZadjust <-9 || TZadjust> -2 ){ printf("/n *****   Local time zone adjustment must be vaguely in Canada so between -9 and -2 \n"); exit(1);}
   lastffmc=atof(argv[2]);
   if(lastffmc>101 || lastffmc<0){ printf(" /n/n *****   FFMC must be between 0 and 101 \n"); exit(1);}
   lastdmc=atof(argv[3]);
   if(lastdmc<0){ printf(" /n/n *****  starting DMC must be >=0  \n"); exit(1);}
   lastdc=atof(argv[4]);
   if(lastdc<0){ printf(" /n/n *****   starting DC must be >=0\n"); exit(1);}

   printf("TZ=%d    start ffmc=%f  dmc=%f\n",TZadjust,lastffmc,lastdmc);
   lastmcgmc=101-lastffmc;  /* approximation for a start up*/

   for(h=0;h<24;h++){
     atemp[h]=0.0;arh[h]=0.0;aws[h]=0.0;arain[h]=0.0;
   }
   /* check that the header matches what is expected */
   const char* header = "lat,long,year,mon,day,hour,temp,rh,wind,rain";
   check_header(inp, header);
   struct row cur;
   struct row old;
   err = read_row(inp, &cur);
   old = cur;

   while(err>0){
     while(err>0 && old.mon==cur.mon && old.day==cur.day){  /* This loads up 24 hours at a time  ...need to for this version*/
         atemp[cur.hour]=cur.temp;
         arh[cur.hour]=cur.rh;
         aws[cur.hour]=cur.wind;
         arain[cur.hour]=cur.rain;
         old = cur;
         err = read_row(inp, &cur);
         if (err > 0 && (old.lon != cur.lon || old.lat != cur.lat))
         {
           printf("Latitude and Longitude must be constant\n");
           exit(1);
         }
         if (err > 0 && (1 != (cur.hour - old.hour) && !(23 == old.hour && 0 == cur.hour)))
         {
           printf("Hours must be sequential but went from %d to %d\n", old.hour, cur.hour);
           exit(1);
         }
         /* printf("%d %d %d %d  %5.1f  %5.1f  %5.1f %5.1f %d\n", old.year, old.mon , old.day,old.hour,atemp[old.hour],arh[old.hour],aws[old.hour],arain[old.hour],err); */
     }  /* end the while to read thru a day */


     solar=sun(old.lat,old.lon,old.mon,old.day,12,TZadjust,&sunrise,&sunset );
     daylength=sunset-sunrise;

     printf("here : %f %f  %d   %d %d  SUNrise=%5.2f  sunset=%5.2f\n",old.lat,old.lon,old.year,old.mon,old.day,sunrise,sunset);

     /* an initial go thru the loop to summarize daily information*/
     for(h=0,rain24=0.0, minRH=100.0,Wdmc24=0.0,Wdc24=0.0; h<24; h++)
     {
         rain24+=arain[h];
         if(arh[h]<minRH)minRH=arh[h];
         if(h>=sunrise && h<=sunset)Wdmc24+=DMCdryingweight(atemp[h],arh[h],aws[h]);
         if(h>=sunrise && h<=sunset)Wdc24+=DCdryingweight(atemp[h],arh[h],aws[h]);
        }

     if(rain24>0.5) ffmcRF=(rain24-0.5)/rain24;   /* so we can use the old hourly routine directly*/
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
         solar=sun(old.lat,old.lon,old.mon,old.day,h,TZadjust,&sunrise,&sunset );

         ffmc=hourly_ffmc(atemp[h],arh[h],aws[h],(arain[h]*ffmcRF),lastffmc);

         if(Wdmc24>0){
              if(h>=sunrise && h<=sunset) dmcDryFrac=DMCdryingweight(atemp[h],arh[h],aws[h]) / Wdmc24;
              else dmcDryFrac=0.0;
         }
         else{
             printf("   ********WDMC24===0 \n");
             if(h>=sunrise && h<=sunset)dmcDryFrac=1/daylength;  /* is VPD is 0 all day long --rh=100 all day -- then set drying fraction to be uniform */
             else dmcDryFrac=0.0;
         }
         /* for each hour, calculate a full day of drying at those conditions, and then use fraction of that */
         dmc=hourly_DMC(atemp[h],arh[h],aws[h],arain[h],old.mon,lastdmc,dmcDryFrac,rain24,DELTA_mcdmcrain24,atemp[12],arh[12] );

         if(Wdc24>0){
             if(h>=sunrise && h<=sunset) dcDryFrac = DCdryingweight(atemp[h],arh[h],aws[h])/Wdc24;
             else dcDryFrac=0.0;
         }
         else {
             printf("   ********WDC24===0 \n");
             if(h>=sunrise && h<=sunset) dcDryFrac=1/daylength; /* is VPD is 0 all day long --rh=100 all day -- then set drying fraction to be uniform */
             else dcDryFrac=0.0;
         }

         dc=hourly_DC(atemp[h],arh[h],aws[h],arain[h],lastdc,old.mon, rain24,dcDryFrac,DELTA_DCrain24,atemp[12] );

         isi=ISIcalc(aws[h],ffmc);
         bui=BUIcalc(dmc,dc);
         fwi=FWIcalc(isi,bui);

/* grass */
         if(minRH>=100) minRH=99.5;
         /* if min RH for the calendar date is >30 then calculate solar prop based on min rh for the day */
         if(minRH>30) solprop=maxsolprop*(1.27-0.0111*minRH);
         else solprop=maxsolprop;
         if (solprop<0)solprop=0;
         solar=solar*solprop;   /*  just reducing solar radiation a little off of full value */
         mcgmc=hourly_gfmc(atemp[h],arh[h],aws[h],arain[h],lastmcgmc,solar,1.0);
         gfmc=59.5*(250-mcgmc)/(147.2772277+mcgmc);
         gsi=grassISI(aws[h],mcgmc, percent_cured);
         gfwi=grassFWI( gsi, grassfuelload);
         fprintf(out,"%4d,%2d,%2d,%2d,%5.1f,%3.0f,%5.1f,%5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f\n",
          old.year,old.mon,old.day,h,atemp[h],arh[h],aws[h],arain[h],ffmc,dmc,dc,isi,bui,fwi, gfmc,gsi,gfwi);

         printf("%4d,%2d,%2d,%2d,%5.1f,%3.0f,%5.1f,%5.1f,     %5.2f, %5.2f, %5.2f, %5.1f, %5.1f, %5.1f | %5.1f, %5.1f, %5.1f  %5.2f\n",
          old.year,old.mon,old.day,h,atemp[h],arh[h],aws[h],arain[h],ffmc,dmc,dc,isi,bui,fwi, gfmc, gsi, gfwi, mcgmc);
         lastffmc=ffmc;lastdmc=dmc;lastdc=dc; lastmcgmc=mcgmc;
     }  /* end for(h=0; h<24; h++)   */
     old = cur;
   }  /* end the main while(err>0)  */

  /* printf("output has been written to>>> %s\n",argv[6]); */
  fclose(inp);
  fclose(out);
}

/**
 * Calculate hourly FFMC value
 *
 * @param temp            Temperature (Celcius)
 * @param rh              Relative Humidity (percent, 0-100)
 * @param wind            Wind Speed (km/h)
 * @param rain            Precipitation (mm)
 * @param oldffmc         Previous hourly FFMC
 * @return                Hourly FFMC
 */
float hourly_ffmc(float temp, float rh, float wind, float rain, float oldffmc)
{
  /* this is the hourly ffmc routine given wx and previous ffmc */
  float rf = 42.5;
  float drf = 0.0579;
  float xm, a1, e, moe;
  float mo = 147.27723 * (101 - oldffmc) / (59.5 + oldffmc);
  if(rain != 0.0)
  {
    /* duplicated in both formulas, so calculate once */
    float m = rf * rain * exp(-100.0 / (251 - mo)) * (1.0 - exp(-6.93 / rain));
    if (mo > 150) { mo += 0.0015 * (mo - 150) * (mo - 150) * sqrt(rain) + m; }
    else { mo += m; }
    if (mo > 250) { mo=250; }
  }
  /* duplicated in both formulas, so calculate once */
  float e1 = 0.18 * (21.1 - temp) * (1.0 - (1.0 / exp(0.115 * rh)));
  float ed = 0.942 * pow(rh, 0.679) + (11.0 * exp((rh - 100) / 10.0)) + e1;
  float moed = mo - ed;
  float ew = 0.618 * pow(rh, 0.753) + (10.0 * exp((rh - 100) / 10.0)) + e1;
  float moew = mo - ew;
  if (moed==0 || (moew>=0 && moed<0))
  {
    xm = mo;
    if (moed == 0) { e = ed; }
    if (moew >= 0) { e = ew; }
  }
  else
  {
    if (moed > 0)
    {
      a1 = rh / 100;
      e = ed;
      moe = moed;
    }
    else
    {
      a1 = (100.0 - rh) / 100.0;
      e = ew;
      moe = moew;
    }
    float xkd = (0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(wind) * (1 - pow(a1, 8))));
    xkd *= drf * exp(0.0365 * temp);
    xm = e + moe * exp(-log(10.0) * xkd);
  }
  mo = xm;
  return (59.5 * (250 - xm) / (147.27723 + xm));
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
     if (dmc < 0) { dmc = 0; }

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

/**
 * Calculate Initial Spread Index (ISI)
 *
 * @param wind            Wind Speed (km/h)
 * @param ffmc            Fine Fuel Moisure Code
 * @return                Initial Spread Index
 */
float ISIcalc(float ws, float ffmc)
{
  float fm = 147.2773 * (101.0 - ffmc) / (59.5 + ffmc);
  float fw;
  if (ws >= 40) { fw = 12 * (1 - exp(-0.0818 * (ws - 28))); }
  else { fw = exp(0.05039 * ws); }
  float sf = 19.115 * exp(-0.1386 * fm) * (1.0 + pow(fm, 5.31) / 4.93e07);
  float isi = sf * fw;
  return isi;
}

/**
 * Calculate Build-up Index (BUI)
 *
 * @param dmc             Duff Moisture Code
 * @param dc              Drought Code
 * @return                Build-up Index
 */
float BUIcalc (float dmc, float dc)
{
  float bui;
  if (dmc == 0 && dc == 0) { bui=0; }
  else { bui = 0.8 * dc * dmc / (dmc + 0.4 * dc); }
  if (bui < dmc)
  {
    float p = (dmc - bui) / dmc;
    float cc = 0.92 + pow((0.0114 * dmc), 1.7);
    bui = dmc - cc * p;
    if (bui < 0) { bui = 0; }
  }
  return bui;
}

/**
 * Calculate Fire Weather Index (FWI)
 *
 * @param isi             Initial Spread Index
 * @param bui             Build-up Index
 * @return                Fire Weather Index
 */
float FWIcalc(float isi, float bui)
{
  float bb, fwi;
  if (bui > 80) { bb = 0.1 * isi * (1000.0 / (25.0 + 108.64 / exp(0.023 * bui))); }
  else { bb = 0.1 * isi * (0.626 * pow(bui, 0.809) + 2.0); }
  if (bb <= 1) { fwi = bb; }
  else { fwi = exp(2.72 * pow(0.434 * log(bb), 0.647)); }
  return fwi;
}

/**
 * Calculate Hourly Grass Fuel Moisture. Needs to be converted to get GFMC.
 *
 * @param temp            Temperature (Celcius)
 * @param rh              Relative Humidity (percent, 0-100)
 * @param wind            Wind Speed (km/h)
 * @param rain            Precipitation (mm)
 * @param lastmc          Previous grass fuel moisture (percent)
 * @param solrad          Solar radiation (kW/m^2)
 * @param time            Time since last observation (hours)
 * @return                Grass Fuel Moisture (percent)
 */
float hourly_gfmc(float temp, float rh, float wind, float rain, float lastmc, float solrad, float time)
{
  /* MARK II of the model (2016) wth new solar rad model specific to grass

     Temp is temperature in C
     RH is relative humidty in %
     wind is average wind speed in km/h
     rain is rainfall in mm
     solrad is kW/m2  (radiaiton reaching fuel)
     mo is the old grass fuel moisture   (not as a code value...so elimates the conversion to code)
     time - time between obs in HOURS

  DRF of 1/16.1 comes from reducting the standard response time curve
  at 26.7C, 20%RH, 2 km/h to 0.85hr.

  bmw
  */
  float rhf, xm, e;
  float drf = 0.389633;
  float mo = lastmc;
  /* fuel temp from CEVW*/
  float tf = temp + 17.9 * solrad * exp(-0.034 * wind);
  /* fuel humidity */
  if (tf > temp) {
    rhf = rh * 6.107 * pow(10.0, 7.5 * temp / (temp + 237.0)) / (6.107 * pow(10.0, 7.5 * tf / (tf + 237.0)));
  } else {
    rhf = rh;
  }
  if (rain != 0)
  {
    /*     mo+=rain*rf*exp(-100.0/(251.0-mo))*(1.0-exp(-6.93/rain));*/  /* old routine*/
    /* this new routine assumes layer is 0.3 kg/m2 so 0.3mm of rain adds +100%MC*/
    mo = mo + rain / 0.3 * 100.0;   /* *100 to convert to %...  *1/.3 because of 0.3mm=100%  */
    if (mo > 250.0) { mo = 250.0; }
  }
  float ed = 1.62 * pow(rhf, 0.532) + (13.7 * exp((rhf - 100) / 13.0))
              + 0.27 * (26.7 - tf)* (1.0 - (1.0 / exp(0.115 * rhf)));   /*GRASS EMC*/
  float moed = mo - ed;
  float ew = 1.42 * pow(rhf, 0.512) + (12.0 * exp((rhf - 100) / 18.0))
              + 0.27 * (26.7 - tf) * (1.0 - (1.0 / exp(0.115 * rhf)));  /*GRASS EMC*/
  float moew = mo - ew;
  if (moed == 0 || (moew >= 0 && moed < 0))
  {
    xm = mo;
    if (moed == 0) { e=ed; }
    if (moew >= 0) { e=ew; }
  }
  else
  {
    float a1, moe;
    if (moed > 0)
    {
      a1 = rhf / 100;
      e = ed;
      moe = moed;
    }
    else
    {
      a1 = (100.0 - rhf) / 100.0;
      e = ew;
      moe = moew;
    }
    float xkd = (0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(wind) * (1 - pow(a1,8))));
    xkd *= drf * exp(0.0365 * tf);
    /* printf("tf=%8.4f rhf=%6.2f e=%4.1f mo=%5.2f xkd=%6.4f moed=%5.1f moew=%5.1f\n",tf,rhf,e,mo,xkd,moed,moew); */
    xm = e + moe * exp(-1.0 * log(10.0) * xkd * time);
  }
  return xm;
}

/**
 * Calculate Grass Spread Index (GSI)
 *
 * @param wind            Wind Speed (km/h)
 * @param mc              Grass moisture content (percent)
 * @param cur             Degree of curing (percent, 0-100)
 * @return                Grass Spread Index
 */
float grassISI (float wind, float mc, float cur)
{
  float fw;
  if (wind < 5) { fw = (0.054 + 0.209 * wind) * 16.67; }
  else { fw = (1.1 + 0.715 * (wind - 5.0) * 0.844) * 16.67; }
  float fm;
  if (mc < 12) { fm = exp(-0.108 * mc); }
  else if (mc < 20.0 && wind < 10.0) { fm = 0.684 - 0.0342 * mc; }
  else if (mc < 23.9 && wind >= 10.0) { fm = 0.547 - 0.0228 * mc; }
  else { fm = 0; }
  float cf;
  if (cur > 20) { cf = 1.034 / (1 + 104 * exp(-0.1 * (cur - 20))); }
  else { cf = 0.0; }
  float GSI = 1.11 * fw * fm * cf;
  return GSI;
}

/**
 * Calculate Grass Fire Weather Index
 *
 * @param gsi               Grass Spread Index
 * @param load              Fuel Load (kg/m^2)
 * @return                  Grass Fire Weather Index
 */
float grassFWI (float gsi, float load)
{
  float ros = gsi / 1.11;  /*  this just converts back to ROS in m/min*/
  float Fint = 300.0 * load * ros;
  float GFWI;
  if (Fint > 100) { GFWI = log(Fint / 60.0) / 0.14; }
  else { GFWI = Fint / 25.0; }
  return GFWI;
}
