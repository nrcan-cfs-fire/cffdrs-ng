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
static const float EL_DMC[] = {6.5, 7.5, 9.0, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8.0, 7.0, 6.0};
static const float FL_DC[] = {-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5.0, 2.4, 0.4, -1.6, -1.6};
static const float MPCT_TO_MC = 147.2772277;
/* Fuel Load (kg/m^2) */
static const float DEFAULT_GRASS_FUEL_LOAD = 0.35;
static const float MAX_SOLAR_PROPAGATION = 0.85;

/* FIX: figure out what this should be */
static const float DEFAULT_LATITUDE = 55.0;
static const float DEFAULT_LONGITUDE = -120.0;

/* default startup values */
static const float FFMC_DEFAULT = 85;
static const float DMC_DEFAULT = 6;
static const float DC_DEFAULT = 15;

/*
 * Fine Fuel Moisture Code (FFMC) from moisture %
 */
float fine_fuel_moisture_code(float moisture_percent)
{
  return (59.5 * (250 - moisture_percent) / (MPCT_TO_MC + moisture_percent));
}

/*
 * Fine Fuel Moisture (%) from FFMC
 */
float fine_fuel_moisture_from_code(float moisture_code)
{
  return MPCT_TO_MC * (101 - moisture_code) / (59.5 + moisture_code);
}

/**
 * Calculate hourly Fine Fuel Moisture (%) value
 *
 * @param temp            Temperature (Celcius)
 * @param rh              Relative Humidity (percent, 0-100)
 * @param wind            Wind Speed (km/h)
 * @param rain            Precipitation (mm)
 * @param mo              Previous Fine Fuel Moisture (%)
 * @return                Hourly Fine Fuel Moisture (%)
 */
float fine_fuel_moisture(float temp, float rh, float wind, float rain, float mo)
{
  /* this is the hourly ffmc routine given wx and previous ffmc */
  /* use moisture directly instead of converting to/from ffmc */
  if (rain != 0.0)
  {
    /* duplicated in both formulas, so calculate once */
    float m = 42.5 * rain * exp(-100.0 / (251 - mo)) * (1.0 - exp(-6.93 / rain));
    if (mo > 150)
    {
      mo += 0.0015 * pow(mo - 150, 2) * sqrt(rain);
    }
    mo += m;
    if (mo > 250)
    {
      mo = 250;
    }
  }
  /* default value */
  float m = mo;
  /* duplicated in both formulas, so calculate once */
  float e1 = 0.18 * (21.1 - temp) * (1.0 - (1.0 / exp(0.115 * rh)));
  float ed = 0.942 * pow(rh, 0.679) + (11.0 * exp((rh - 100) / 10.0)) + e1;
  if (mo > ed)
  {
    float a1 = rh / 100;
    float ka = (0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(wind) * (1 - pow(a1, 8))));
    float kd = 0.0579 * ka * exp(0.0365 * temp);
    m = ed + (mo - ed) * exp(-2.303 * kd);
  }
  else if (mo < ed)
  {
    float ew = 0.618 * pow(rh, 0.753) + (10.0 * exp((rh - 100) / 10.0)) + e1;
    if (mo < ew)
    {
      float a1 = (100.0 - rh) / 100.0;
      float kb = (0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(wind) * (1 - pow(a1, 8))));
      float kw = 0.0579 * kb * exp(0.0365 * temp);
      m = ew + (mo - ew) * exp(-2.303 * kw);
    }
    /* implicit */
    /*
    if (mo == ew)
    {
      m = mo;
    }
    if (ed > mo && mo > ew)
    {
      m = mo;
    }
    */
  }
  /* implicit */
  /*
  else if (mo == ed)
  {
    m = mo;
  }
  */
  return m;
}

/**
 * Calculate Initial Spread Index (ISI)
 *
 * @param wind            Wind Speed (km/h)
 * @param ffmc            Fine Fuel Moisure Code
 * @return                Initial Spread Index
 */
float initial_spread_index(float ws, float ffmc)
{
  float fm = fine_fuel_moisture_from_code(ffmc);
  float fw;
  if (ws >= 40)
  {
    fw = 12 * (1 - exp(-0.0818 * (ws - 28)));
  }
  else
  {
    fw = exp(0.05039 * ws);
  }
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
float buildup_index(float dmc, float dc)
{
  float bui;
  if (dmc == 0 && dc == 0)
  {
    bui = 0;
  }
  else
  {
    bui = 0.8 * dc * dmc / (dmc + 0.4 * dc);
  }
  if (bui < dmc)
  {
    float p = (dmc - bui) / dmc;
    float cc = 0.92 + pow((0.0114 * dmc), 1.7);
    bui = dmc - cc * p;
    if (bui < 0)
    {
      bui = 0;
    }
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
float fire_weather_index(float isi, float bui)
{
  float bb, fwi;
  if (bui > 80)
  {
    bb = 0.1 * isi * (1000.0 / (25.0 + 108.64 / exp(0.023 * bui)));
  }
  else
  {
    bb = 0.1 * isi * (0.626 * pow(bui, 0.809) + 2.0);
  }
  if (bb <= 1)
  {
    fwi = bb;
  }
  else
  {
    fwi = exp(2.72 * pow(0.434 * log(bb), 0.647));
  }
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
float grass_fuel_moisture(float temp,
                          float rh,
                          float wind,
                          float rain,
                          float lastmc,
                          float solrad,
                          float time)
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
  if (tf > temp)
  {
    rhf = rh * 6.107 * pow(10.0, 7.5 * temp / (temp + 237.0))
        / (6.107 * pow(10.0, 7.5 * tf / (tf + 237.0)));
  }
  else
  {
    rhf = rh;
  }
  if (rain != 0)
  {
    /*     mo+=rain*rf*exp(-100.0/(251.0-mo))*(1.0-exp(-6.93/rain));*/ /* old routine*/
    /* this new routine assumes layer is 0.3 kg/m2 so 0.3mm of rain adds +100%MC*/
    /* *100 to convert to %...  *1/.3 because of 0.3mm=100%  */
    mo = mo + rain / 0.3 * 100.0;
    if (mo > 250.0)
    {
      mo = 250.0;
    }
  }
  /*GRASS EMC*/
  float ed = 1.62 * pow(rhf, 0.532) + (13.7 * exp((rhf - 100) / 13.0))
           + 0.27 * (26.7 - tf) * (1.0 - (1.0 / exp(0.115 * rhf)));
  float moed = mo - ed;
  /*GRASS EMC*/
  float ew = 1.42 * pow(rhf, 0.512) + (12.0 * exp((rhf - 100) / 18.0))
           + 0.27 * (26.7 - tf) * (1.0 - (1.0 / exp(0.115 * rhf)));
  float moew = mo - ew;
  if (moed == 0 || (moew >= 0 && moed < 0))
  {
    xm = mo;
    if (moed == 0)
    {
      e = ed;
    }
    if (moew >= 0)
    {
      e = ew;
    }
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
    float xkd = (0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(wind) * (1 - pow(a1, 8))));
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
float grass_spread_index(float wind, float mc, float cur)
{
  float fw;
  if (wind < 5)
  {
    fw = (0.054 + 0.209 * wind) * 16.67;
  }
  else
  {
    fw = (1.1 + 0.715 * (wind - 5.0) * 0.844) * 16.67;
  }
  float fm;
  if (mc < 12)
  {
    fm = exp(-0.108 * mc);
  }
  else if (mc < 20.0 && wind < 10.0)
  {
    fm = 0.684 - 0.0342 * mc;
  }
  else if (mc < 23.9 && wind >= 10.0)
  {
    fm = 0.547 - 0.0228 * mc;
  }
  else
  {
    fm = 0;
  }
  float cf;
  if (cur > 20)
  {
    cf = 1.034 / (1 + 104 * exp(-0.1 * (cur - 20)));
  }
  else
  {
    cf = 0.0;
  }
  return 1.11 * fw * fm * cf;
}

/**
 * Calculate Grass Fire Weather Index
 *
 * @param gsi               Grass Spread Index
 * @param load              Fuel Load (kg/m^2)
 * @return                  Grass Fire Weather Index
 */
float grass_fire_weather_index(float gsi, float load)
{
  /*  this just converts back to ROS in m/min*/
  float ros = gsi / 1.11;
  float Fint = 300.0 * load * ros;
  float GFWI;
  if (Fint > 100)
  {
    GFWI = log(Fint / 60.0) / 0.14;
  }
  else
  {
    GFWI = Fint / 25.0;
  }
  return GFWI;
}

/*
 * Calculate number of drying "units" this hour contributes
 */
float drying_units(float temp, float rh, float wind, float rain, float solar)
{
  /* for now, just add 1 drying "unit" per hour */
  return 1.0;
}

float dmc_drying(float temp, float rh, float ws, float prec, int mon)
{
  if (temp <= 1.1)
  {
    return 0.0;
  }
  return 1.894 * (temp + 1.1) * (100.0 - rh) * EL_DMC[mon - 1] * 0.0001;
}

float dc_drying(float temp, float rh, float ws, float prec, int mon)
{
  if (temp <= 2.8)
  {
    return 0.0;
  }
  return (0.36 * (temp + 2.8) + FL_DC[mon - 1]) / 2.0;
}

float dmc_wetting(float rain_total, float lastdmc)
{
  if (rain_total <= 1.5)
  {
    return 0.0;
  }
  float b;
  if (lastdmc <= 33)
  {
    b = 100.0 / (0.5 + 0.3 * lastdmc);
  }
  else if (lastdmc <= 65)
  {
    b = 14.0 - 1.3 * log(lastdmc);
  }
  else
  {
    b = 6.2 * log(lastdmc) - 17.2;
  }
  const float reff = (0.92 * rain_total - 1.27);
  /* This is the change in MC (moisturecontent)  from FULL DAY's rain  */
  return 1000.0 * reff / (48.77 + b * reff);
}

float dc_wetting(float rain_total, float lastdc)
{
  if (rain_total <= 2.8)
  {
    return 0.0;
  }
  const float rw = 0.83 * rain_total - 1.27;
  const float smi = 800 * exp(-lastdc / 400);
  /* TOTAL change for the TOTAL 24 hour rain from FWI1970 model  */
  return -400.0 * log(1.0 + 3.937 * rw / smi);
}

float duff_moisture_code(float temp, float rh, float wind, float rain, float drying_fraction, float mon, float dmc)
{
  /* full amount of wetting because it uses rain so far, but just one hour of drying */
  /* NOTE: this is going to have some compounding error vs daily function because it's based on previous hour's value and not the daily */
  return dmc + dmc_wetting(rain, dmc) + drying_fraction * dmc_drying(temp, rh, wind, rain, mon);
}

float drought_code(float temp, float rh, float wind, float rain, float drying_fraction, float mon, float dc)
{
  /* full amount of wetting because it uses rain so far, but just one hour of drying */
  /* NOTE: this is going to have some compounding error vs daily function because it's based on previous hour's value and not the daily */
  return dc + dc_wetting(rain, dc) + drying_fraction * dc_drying(temp, rh, wind, rain, mon);
}

int populate_row(FILE* inp, struct row* cur, float TZadjust)
{
  int err = read_row(inp, cur);
  float sunrise;
  float sunset;
  float solar = sun(cur->lat, cur->lon, cur->mon, cur->day, cur->hour, TZadjust, &(cur->sunrise), &(cur->sunset));
  float julian_day = julian(cur->mon, cur->day);
  /* assuming we want curing to change based on current day and not remain */
  /* the same across entire period based on start date */
  cur->percent_cured = seasonal_curing(julian_day);
  /* FIX: use a constant grass fuel load for now */
  cur->grass_fuel_load = DEFAULT_GRASS_FUEL_LOAD;
  return err;
}

void main(int argc, char* argv[])
{
  if (7 != argc)
  {
    printf("Command line:   %s <local GMToffset> <starting FFMC> <starting DMC> <starting DC> <input file> <output file>\n\n", argv[0]);
    printf("<local GMToffset> is the off of Greenich mean time (for Eastern = -5  Central=-6   MT=-7  PT=-8 )  \n");
    printf("All times should be local standard time\n");
    printf("INPUT FILE format must be HOURLY weather data, comma seperated and take the form\n");
    printf("Latitude,Longitude,YEAR,MONTH,DAY,HOUR,Temperature(C),Relative_humidity(%%),Wind_speed(km/h),Rainfall(mm)\n\n");
    exit(1);
  }

  FILE* inp = fopen(argv[5], "r");
  printf("Opening input file >>> %s   \n", argv[5]);
  if (inp == NULL)
  {
    printf("\n\n ***** FILE  %s  does not exist\n", argv[5]);
    exit(1);
  }
  FILE* out = fopen(argv[6], "w");

  /*  CSV headers */
  fprintf(out, "year,mon,day,hour,temp,rh,wind,rain,ffmc,dmc,dc,isi,bui,fwi,gfmc,gsi,gfwi\n");

  int TZadjust = atoi(argv[1]);
  if (TZadjust < -9 || TZadjust > -2)
  {
    printf("/n *****   Local time zone adjustment must be vaguely in Canada so between -9 and -2 \n");
    exit(1);
  }
  float lastffmc = atof(argv[2]);
  if (lastffmc > 101 || lastffmc < 0)
  {
    printf(" /n/n *****   FFMC must be between 0 and 101 \n");
    exit(1);
  }
  float dmc = atof(argv[3]);
  if (dmc < 0)
  {
    printf(" /n/n *****  starting DMC must be >=0  \n");
    exit(1);
  }
  float dc = atof(argv[4]);
  if (dc < 0)
  {
    printf(" /n/n *****   starting DC must be >=0\n");
    exit(1);
  }

  printf("TZ=%d    start ffmc=%f  dmc=%f\n", TZadjust, lastffmc, dmc);
  /* approximation for a start up*/
  float lastmcgmc = 101.0 - lastffmc;
  float lastmcffmc = fine_fuel_moisture_from_code(lastffmc);

  /* check that the header matches what is expected */
  const char* header = "lat,long,year,mon,day,hour,temp,rh,wind,rain";
  check_header(inp, header);
  struct row cur;
  int err = populate_row(inp, &cur, TZadjust);
  struct row old = cur;
  float rain_total = 0.0;
  float drying_since_intercept = 0.0;
  /* for now, want 5 "units" of drying (which is 1 per hour to start) */
  const float TARGET_DRYING_SINCE_INTERCEPT = 5;
  while (err > 0)
  {
    if (cur.day != old.day || cur.mon != old.mon)
    {
      printf("here : %f %f  %d   %d %d  SUNrise=%5.2f  sunset=%5.2f\n",
             cur.lat,
             cur.lon,
             cur.year,
             cur.mon,
             cur.day,
             cur.sunrise,
             cur.sunset);
    }
    if (0 < cur.rain)
    {
      rain_total += cur.rain;
      /* no drying if still raining */
      drying_since_intercept = 0.0;
    }
    else
    {
      drying_since_intercept += drying_units(cur.temp, cur.rh, cur.wind, cur.rain, cur.solar);
      if (drying_since_intercept >= TARGET_DRYING_SINCE_INTERCEPT)
      {
        /* reset rain if intercept reset criteria met */
        rain_total = 0.0;
      }
    }
    rain_total += cur.rain;
    /* apply rain intercept of 0.5mm to ffmc at start of period */
    const float rain_ffmc = cur.rain > 0.5 ? cur.rain - 0.5 : 0.0;
    float mcffmc = fine_fuel_moisture(cur.temp, cur.rh, cur.wind, rain_ffmc, lastmcffmc);
    /* convert to code for output, but keep using moisture % for precision */
    float ffmc = fine_fuel_moisture_code(mcffmc);
    int sunlight_hours = round(cur.sunset) - round(cur.sunrise);
    /* apply one hour of drying if during sunlight hours */
    float drying_fraction = (cur.hour >= round(cur.sunset) && cur.hour < round(cur.sunrise))
                            ? 1.0 / sunlight_hours
                            : 0.0;
    /* HACK: pass drying fraction instead of solar radiation for now */
    dmc = duff_moisture_code(cur.temp, cur.rh, cur.wind, rain_total, cur.mon, drying_fraction, dmc);
    dc = drought_code(cur.temp, cur.rh, cur.wind, rain_total, cur.mon, drying_fraction, dc);
    float isi = initial_spread_index(cur.wind, ffmc);
    float bui = buildup_index(dmc, dc);
    float fwi = fire_weather_index(isi, bui);
    float mcgmc = grass_fuel_moisture(cur.temp, cur.rh, cur.wind, cur.rain, lastmcgmc, cur.solar, 1.0);
    float gfmc = fine_fuel_moisture_code(mcgmc);
    float gsi = grass_spread_index(cur.wind, mcgmc, cur.percent_cured);
    float gfwi = grass_fire_weather_index(gsi, cur.grass_fuel_load);
    fprintf(out,
            "%4d,%2d,%2d,%2d,%5.1f,%3.0f,%5.1f,%5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f\n",
            old.year,
            old.mon,
            old.day,
            cur.hour,
            cur.temp,
            cur.rh,
            cur.wind,
            cur.rain,
            ffmc,
            dmc,
            dc,
            isi,
            bui,
            fwi,
            gfmc,
            gsi,
            gfwi);

    printf("%4d,%2d,%2d,%2d,%5.1f,%3.0f,%5.1f,%5.1f,     %5.2f, %5.2f, %5.2f, %5.1f, %5.1f, %5.1f | %5.1f, %5.1f, %5.1f  %5.2f\n",
           old.year,
           old.mon,
           old.day,
           cur.hour,
           cur.temp,
           cur.rh,
           cur.wind,
           cur.rain,
           ffmc,
           dmc,
           dc,
           isi,
           bui,
           fwi,
           gfmc,
           gsi,
           gfwi,
           mcgmc);
    /* ffmc and gmc are calculated from previous hourly values */
    lastmcffmc = mcffmc;
    lastmcgmc = mcgmc;
    old = cur;
    err = populate_row(inp, &cur, TZadjust);
    if (err > 0 && (old.lon != cur.lon || old.lat != cur.lat))
    {
      printf("Latitude and Longitude must be constant\n");
      exit(1);
    }
    if (err > 0 && (1 != (cur.hour - old.hour) && !(23 == old.hour && 0 == cur.hour)))
    {
      printf("Hours must be sequential but went from %d to %d\n",
             old.hour,
             cur.hour);
      exit(1);
    }
  } /* end the main while(err>0)  */

  /* printf("output has been written to>>> %s\n",argv[6]); */
  fclose(inp);
  fclose(out);
}
