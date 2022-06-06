#include "util.h"
const float pi = 3.14159265;
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
