/*  Phase1  NG-FWI.c

inputs full hourly weather sttream only.
new hourly DMC and DC
calcuats solar radiation from lat/long
grass moisture, grass spread index and grass fire weahter index added.\

 THis is only set up for ONE station at a time.   And ONE year really.

version 0.9    (still testing)

bmw/2021
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "util.h"

#ifndef FLT_EPSILON
#define FLT_EPSILON 1E-5
#endif

#define assert_equals(a, b)                      \
  if (abs(a - b) <= FLT_EPSILON)                 \
  {                                              \
  }                                              \
  else                                           \
  {                                              \
    printf("%s#L%d:\n\t%s != %s:\n\t\t%f != %f", \
           __FILE__,                             \
           __LINE__,                             \
           #a,                                   \
           #b,                                   \
           a,                                    \
           b);                                   \
    exit(-1);                                    \
  }

/* Fuel Load (kg/m^2) */
static const float DEFAULT_GRASS_FUEL_LOAD = 0.35;
static const float MAX_SOLAR_PROPAGATION = 0.85;

/* default startup values */
static const float FFMC_DEFAULT = 85;
static const float DMC_DEFAULT = 6;
static const float DC_DEFAULT = 15;

/* FIX: figure out what this should be */
static const float DEFAULT_LATITUDE = 55.0;
static const float DEFAULT_LONGITUDE = -120.0;

static const float EL_DMC[] = {6.5, 7.5, 9.0, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8.0, 7.0, 6.0};
static const float FL_DC[] = {-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5.0, 2.4, 0.4, -1.6, -1.6};
static const float MPCT_TO_MC = 147.2772277;
static const float DMC_INTERCEPT = 1.5;
static const float DC_INTERCEPT = 2.8;

/*
 * Fine Fuel Moisture Code (FFMC) from moisture %
 */
float fine_fuel_moisture_code(float moisture_percent)
{
  return (59.5 * (250 - moisture_percent) / (MPCT_TO_MC + moisture_percent));
}

/*
 * Fine Fuel Moisture (percent) from FFMC
 */
float fine_fuel_moisture_from_code(float moisture_code)
{
  return MPCT_TO_MC * (101 - moisture_code) / (59.5 + moisture_code);
}

/**
 * Calculate hourly Fine Fuel Moisture (percent) value
 *
 * @param temp            Temperature (Celcius)
 * @param rh              Relative Humidity (percent, 0-100)
 * @param ws              Wind Speed (km/h)
 * @param rain            Precipitation (mm)
 * @param mo              Previous Fine Fuel Moisture (percent)
 * @return                Hourly Fine Fuel Moisture (percent)
 */
float hourly_fine_fuel_moisture(const float temp,
                                const float rh,
                                const float ws,
                                const float rain,
                                const float lastmc)
{
  static const float drf = 0.0579;
  /* Time since last observation (hours) */
  static const float time = 1.0;
  /* use moisture directly instead of converting to/from ffmc */
  /* expects any rain intercept to already be applied */
  float mo = lastmc;
  if (rain != 0.0)
  {
    /* duplicated in both formulas, so calculate once */
    /* lastmc == mo, but use lastmc since mo changes after first equation */
    mo += 42.5 * rain * exp(-100.0 / (251 - lastmc)) * (1.0 - exp(-6.93 / rain));
    if (lastmc > 150)
    {
      mo += 0.0015 * pow(lastmc - 150, 2) * sqrt(rain);
    }
    if (mo > 250)
    {
      mo = 250;
    }
  }
  /* duplicated in both formulas, so calculate once */
  const float e1 = 0.18 * (21.1 - temp) * (1.0 - (1.0 / exp(0.115 * rh)));
  const float ed = 0.942 * pow(rh, 0.679) + (11.0 * exp((rh - 100) / 10.0)) + e1;
  /* m = ed if mo >= ed else ew */
  float m = mo < ed
            ? 0.618 * pow(rh, 0.753) + (10.0 * exp((rh - 100) / 10.0)) + e1
            : ed;
  if (mo != ed)
  {
    /* these are the same formulas with a different value for a1 */
    const float a1 = mo < ed
                     ? (100.0 - rh) / 100.0
                     : rh / 100.0;
    const float k0_or_k1 = 0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(ws) * (1 - pow(a1, 8)));
    const float kd_or_kw = drf * k0_or_k1 * exp(0.0365 * temp);
    m += (mo - m) * pow(10, -kd_or_kw * time);
  }
  return m;
}

/**
 * Calculate Initial Spread Index (ISI)
 *
 * @param ws              Wind Speed (km/h)
 * @param ffmc            Fine Fuel Moisure Code
 * @return                Initial Spread Index
 */
float initial_spread_index(float ws, float ffmc)
{
  const float fm = fine_fuel_moisture_from_code(ffmc);
  const float fw = 40 <= ws
                   ? 12 * (1 - exp(-0.0818 * (ws - 28)))
                   : exp(0.05039 * ws);
  const float sf = 19.115 * exp(-0.1386 * fm) * (1.0 + pow(fm, 5.31) / 4.93e07);
  const float isi = sf * fw;
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
  float bui = 0 == dmc && 0 == dc
              ? 0.0
              : 0.8 * dc * dmc / (dmc + 0.4 * dc);
  if (bui < dmc)
  {
    const float p = (dmc - bui) / dmc;
    const float cc = 0.92 + pow(0.0114 * dmc, 1.7);
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
  const float bb = 0.1 * isi
                 * (bui > 80
                      ? 1000.0 / (25.0 + 108.64 / exp(0.023 * bui))
                      : 0.626 * pow(bui, 0.809) + 2.0);
  const float fwi = bb <= 1
                    ? bb
                    : exp(2.72 * pow(0.434 * log(bb), 0.647));
  return fwi;
}

/**
 * Calculate Hourly Grass Fuel Moisture. Needs to be converted to get GFMC.
 *
 * @param temp            Temperature (Celcius)
 * @param rh              Relative Humidity (percent, 0-100)
 * @param ws              Wind Speed (km/h)
 * @param rain            Precipitation (mm)
 * @param lastmc          Previous grass fuel moisture (percent)
 * @param solrad          Solar radiation (kW/m^2)
 * @return                Grass Fuel Moisture (percent)
 */
float hourly_grass_fuel_moisture(float temp,
                                 float rh,
                                 float ws,
                                 float rain,
                                 float lastmc,
                                 float solrad)
{
  /* MARK II of the model (2016) wth new solar rad model specific to grass

     Temp is temperature in C
     RH is relative humidty in %
     ws is average wind speed in km/h
     rain is rainfall in mm
     solrad is kW/m2  (radiaiton reaching fuel)
     mo is the old grass fuel moisture   (not as a code value...so elimates the conversion to code)
     time - time between obs in HOURS

  DRF of 1/16.1 comes from reducting the standard response time curve
  at 26.7C, 20%RH, 2 km/h to 0.85hr.

  bmw
  */
  static const float drf = 0.389633;
  /* Time since last observation (hours) */
  static const float time = 1.0;
  float mo = lastmc;
  if (rain != 0)
  {
    /*     mo+=rain*rf*exp(-100.0/(251.0-mo))*(1.0-exp(-6.93/rain));*/ /* old routine*/
    /* this new routine assumes layer is 0.3 kg/m2 so 0.3mm of rain adds +100%MC*/
    /* *100 to convert to %...  *1/.3 because of 0.3mm=100%  */
    mo += rain / 0.3 * 100.0;
    if (mo > 250.0)
    {
      mo = 250.0;
    }
  }
  /* fuel temp from CEVW*/
  const float tf = temp + 17.9 * solrad * exp(-0.034 * ws);
  /* fuel humidity */
  const float rhf = tf > temp
                    ? (rh * 6.107 * pow(10.0, 7.5 * temp / (temp + 237.0))
                       / (6.107 * pow(10.0, 7.5 * tf / (tf + 237.0))))
                    : rh;
  /* duplicated in both formulas, so calculate once */
  const float e1 = 0.27 * (26.7 - tf) * (1.0 - (1.0 / exp(0.115 * rhf)));
  /*GRASS EMC*/
  const float ed = 1.62 * pow(rhf, 0.532) + (13.7 * exp((rhf - 100) / 13.0)) + e1;
  const float ew = 1.42 * pow(rhf, 0.512) + (12.0 * exp((rhf - 100) / 18.0)) + e1;
  if (mo == ed || (mo > ew && ed > mo))
  {
    return mo;
  }
  /* mo != ed */
  /* these are the same formulas with a different value for a1 */
  const float a1 = mo < ed
                   ? (100.0 - rhf) / 100.0
                   : rhf / 100.0;
  const float k0_or_k1 = 0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(ws) * (1 - pow(a1, 8)));
  const float kd_or_kw = drf * k0_or_k1 * exp(0.0365 * tf);
  /* m = ed if mo >= ed else ew */
  const float m = mo < ed
                  ? ew
                  : ed;
  return m + (mo - m) * pow(10, -kd_or_kw * time);
}

/**
 * Calculate Grass Spread Index (GSI)
 *
 * @param ws              Wind Speed (km/h)
 * @param mc              Grass moisture content (percent)
 * @param cur             Degree of curing (percent, 0-100)
 * @return                Grass Spread Index
 */
float grass_spread_index(float ws, float mc, float cur)
{
  const float fw = 16.67 * (ws < 5 ? 0.054 + 0.209 * ws : 1.1 + 0.715 * (ws - 5.0) * 0.844);
  /* NOTE: between [12, ~12.01754] the value for ws < 10 is greater than ws >= 10 */
  /* using 0.6838 instead would mean this is always less than ws >= 10 */
  /* mc < 23.9 because of check at start of function, so last expression is any ws >= 10 */
  const float fm = mc < 12
                   ? exp(-0.108 * mc)
                   : (mc < 20.0 && ws < 10.0
                        ? 0.684 - 0.0342 * mc
                        : (mc < 23.9 && ws >= 10.0
                             ? 0.547 - 0.0228 * mc
                             : 0.0));
  const float cf = cur > 20
                   ? 1.034 / (1 + 104 * exp(-0.1 * (cur - 20)))
                   : 0.0;
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
  const float ros = gsi / 1.11;
  const float Fint = 300.0 * load * ros;
  return Fint > 100
         ? log(Fint / 60.0) / 0.14
         : Fint / 25.0;
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
  /* compare floats by using tolerance */
  if (rain_total <= (DMC_INTERCEPT + FLT_EPSILON))
  /* if (rain_total <= DMC_INTERCEPT) */
  {
    return 0.0;
  }
  const float b = lastdmc <= 33
                  ? 100.0 / (0.5 + 0.3 * lastdmc)
                  : (lastdmc <= 65
                       ? 14.0 - 1.3 * log(lastdmc)
                       : 6.2 * log(lastdmc) - 17.2);
  const float reff = 0.92 * rain_total - 1.27;
  /* This is the change in MC (moisturecontent)  from FULL DAY's rain  */
  return 1000.0 * reff / (48.77 + b * reff);
}

float dc_wetting(float rain_total, float lastdc)
{
  /* compare floats by using tolerance */
  if (rain_total <= (DC_INTERCEPT + FLT_EPSILON))
  /* if (rain_total <= DC_INTERCEPT) */
  {
    return 0.0;
  }
  const float rw = 0.83 * rain_total - 1.27;
  const float smi = 800 * exp(-lastdc / 400);
  /* TOTAL change for the TOTAL 24 hour rain from FWI1970 model  */
  return 400.0 * log(1.0 + 3.937 * rw / smi);
}

int populate_row(FILE* inp, struct row* cur, float TZadjust)
{
  int err = read_row(inp, cur);
  cur->solar = sun(cur->lat, cur->lon, cur->mon, cur->day, cur->hour, TZadjust, &(cur->sunrise), &(cur->sunset));
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
  fprintf(out, "year,mon,day,hour,temp,rh,wind,rain,ffmc,dmc,dc,isi,bui,fwi,gfmc,gsi,gfwi,mcffmc,mcgmc,percent_cured,grass_fuel_load\n");
  int TZadjust = atoi(argv[1]);
  if (TZadjust < -9 || TZadjust > -2)
  {
    printf("/n *****   Local time zone adjustment must be vaguely in Canada so between -9 and -2 \n");
    exit(1);
  }
  float ffmc = atof(argv[2]);
  if (ffmc > 101 || ffmc < 0)
  {
    printf(" /n/n *****   FFMC must be between 0 and 101 \n");
    exit(1);
  }
  const float dmc_startup = atof(argv[3]);
  if (dmc_startup < 0)
  {
    printf(" /n/n *****  starting DMC must be >=0  \n");
    exit(1);
  }
  float dc_startup = atof(argv[4]);
  if (dc_startup < 0)
  {
    printf(" /n/n *****   starting DC must be >=0\n");
    exit(1);
  }
  /* printf("TZ=%d    start ffmc=%f  dmc=%f\n", TZadjust, ffmc, dmc_startup); */
  /* approximation for a start up*/
  float mcgmc = 101.0 - ffmc;
  float mcffmc = fine_fuel_moisture_from_code(ffmc);

  /* check that the header matches what is expected */
  const char* header = "lat,long,year,mon,day,hour,temp,rh,wind,rain";
  check_header(inp, header);
  struct row cur;
  int err = populate_row(inp, &cur, TZadjust);
  struct row old = cur;
  float rain_total = 0.0;
  float rain_prev = 0.0;
  float dmc_running = dmc_startup;
  float dmc_drying_total = 0.0;
  float dmc_before_precip = dmc_startup;
  float dmc_wetting_since_precip = 0.0;
  float dmc_wetting_total = 0.0;
  float dc_running = dc_startup;
  float dc_drying_total = 0.0;
  float dc_before_precip = dc_startup;
  float dc_wetting_since_precip = 0.0;
  float dc_wetting_total = 0.0;
  float drying_since_intercept = 0.0;
  /* for now, want 5 "units" of drying (which is 1 per hour to start) */
  const float TARGET_DRYING_SINCE_INTERCEPT = 5;
  while (err > 0)
  {
    /*
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
    */
    /* HACK: for some reason round() isn't working */
    int sunrise = (int)(cur.sunrise + 0.5);
    int sunset = (int)(cur.sunset + 0.5);
    int sunlight_hours = sunset - sunrise;
    /* apply one hour of drying if during sunlight hours */
    double drying_fraction = cur.hour >= sunrise && cur.hour < sunset
                             ? (1.0 / sunlight_hours)
                             : 0.0;
    rain_prev = rain_total;
    rain_total += cur.rain;
    float dmc_daily = dmc_drying(cur.temp, cur.rh, cur.wind, cur.rain, cur.mon);
    float dmc_hourly = drying_fraction * dmc_daily;
    dmc_drying_total += dmc_hourly;
    /* wetting is calculated based on initial dmc when rain started and rain since */
    /* full amount of wetting because it uses rain so far, but just one hour of drying */
    float dmc_wetting_current = dmc_wetting(rain_total, dmc_before_precip);
    /* apply wetting since last period */
    float dmc_wetting_hourly = dmc_wetting_current - dmc_wetting_since_precip;
    float dmc_wetting_prev = dmc_wetting(rain_total - cur.rain, dmc_before_precip);
    float dmc_wetting_rain_prev = dmc_wetting(rain_prev, dmc_before_precip);
    printf("%f, %f, %f\n", rain_prev, rain_total, rain_total - cur.rain);
    if (rain_prev == (rain_total - cur.rain))
    {
      printf("%0.30f == %0.30f\n", rain_prev, (rain_total - cur.rain));
    }
    else
    {
      printf("%0.30f != %0.30f\n", rain_prev, (rain_total - cur.rain));
    }
    /* if (iseqsig((float_t)(rain_prev), (float_t)(rain_total - cur.rain))) */
    if (abs(rain_prev - (rain_total - cur.rain)) <= FLT_EPSILON)
    {
      printf("%0.30f == %0.30f\n", rain_prev, (rain_total - cur.rain));
    }
    else
    {
      printf("%0.30f != %0.30f\n", rain_prev, (rain_total - cur.rain));
    }
    printf("%f, %f, %f\n", dmc_wetting_rain_prev, dmc_wetting_current, dmc_wetting_since_precip);
    assert_equals(dmc_wetting_rain_prev, dmc_wetting_since_precip);
    assert_equals(dmc_wetting_prev, dmc_wetting_rain_prev);
    assert_equals(dmc_wetting_prev, dmc_wetting_since_precip);
    float dmc_wetting_diff = dmc_wetting_current - dmc_wetting_prev;
    assert_equals(dmc_wetting_diff, dmc_wetting_hourly);
    /* dmc calculated from start of stream to just before this rain */
    float dmc = dmc_startup + dmc_drying_total - dmc_wetting_total;
    dmc_running += dmc_hourly;
    assert_equals(dmc_running, dmc);
    /* at most apply same wetting as current value (don't go below 0) */
    if (dmc_wetting_hourly > dmc)
    {
      dmc_wetting_hourly = dmc;
    }
    dmc_wetting_total += dmc_wetting_hourly;
    /* should be no way this is below 0 because we just made sure it wasn't > dmc */
    dmc -= dmc_wetting_hourly;
    dmc_running -= dmc_wetting_hourly;
    assert_equals(dmc_running, dmc);
    dmc_wetting_since_precip = dmc_wetting_current;
    float dc_daily = dc_drying(cur.temp, cur.rh, cur.wind, cur.rain, cur.mon);
    float dc_hourly = drying_fraction * dc_daily;
    dc_drying_total += dc_hourly;
    float dc_wetting_current = dc_wetting(rain_total, dc_before_precip);
    float dc_wetting_hourly = dc_wetting_current - dc_wetting_since_precip;
    float dc_wetting_prev = dc_wetting(rain_total - cur.rain, dc_before_precip);
    float dc_wetting_rain_prev = dc_wetting(rain_prev, dc_before_precip);
    printf("%f, %f, %f\n", dc_wetting_rain_prev, dc_wetting_current, dc_wetting_since_precip);
    assert_equals(dc_wetting_rain_prev, dc_wetting_since_precip);
    assert_equals(dc_wetting_prev, dc_wetting_rain_prev);
    assert_equals(dc_wetting_prev, dc_wetting_since_precip);
    float dc_wetting_diff = dc_wetting_current - dc_wetting_prev;
    assert_equals(dc_wetting_diff, dc_wetting_hourly);
    float dc = dc_startup + dc_drying_total - dc_wetting_total;
    dc_running += dc_hourly;
    assert_equals(dc_running, dc);
    /* at most apply same wetting as current value (don't go below 0) */
    if (dc_wetting_hourly > dc)
    {
      dc_wetting_hourly = dc;
    }
    dc_wetting_total += dc_wetting_hourly;
    /* should be no way this is below 0 because we just made sure it wasn't > dmc */
    dc -= dc_wetting_hourly;
    dc_running -= dc_wetting_hourly;
    assert_equals(dc_running, dc);
    /* printf("%0.2f - %0.2f = %0.2f, ", dc_hourly, dc_wetting_hourly, dc); */
    dc_wetting_since_precip = dc_wetting_current;
    if (0 < cur.rain)
    {
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
        drying_since_intercept = 0.0;
      }
    }
    if (rain_total <= DMC_INTERCEPT)
    {
      dmc_before_precip = dmc;
      dmc_wetting_since_precip = 0.0;
      /* dmc_drying_since_wetting_start = 0; */
    }
    if (rain_total <= DC_INTERCEPT)
    {
      dc_before_precip = dc;
      dc_wetting_since_precip = 0.0;
      /* dc_drying_since_wetting_start = 0; */
    }
    /* printf("%0.2f, ", dmc_at_wetting_start); */
    /* printf("%0.2f, ", dmc_wetting_total); */
    /* rain_total += cur.rain; */
    /* use lesser of remaining intercept and current hour's rain */
    float rain_ffmc = rain_total <= 0.5
                      ? 0.0
                      : ((rain_total - 0.5) > cur.rain
                           ? cur.rain
                           : rain_total - 0.5);
    mcffmc = hourly_fine_fuel_moisture(cur.temp, cur.rh, cur.wind, rain_ffmc, mcffmc);
    /* convert to code for output, but keep using moisture % for precision */
    ffmc = fine_fuel_moisture_code(mcffmc);
    float isi = initial_spread_index(cur.wind, ffmc);
    float bui = buildup_index(dmc, dc);
    float fwi = fire_weather_index(isi, bui);
    mcgmc = hourly_grass_fuel_moisture(cur.temp, cur.rh, cur.wind, cur.rain, mcgmc, cur.solar);
    float gfmc = fine_fuel_moisture_code(mcgmc);
    float gsi = grass_spread_index(cur.wind, mcgmc, cur.percent_cured);
    float gfwi = grass_fire_weather_index(gsi, cur.grass_fuel_load);
    /* printf("\n"); */
    fprintf(out,
            "%4d,%2d,%2d,%2d,%5.1f,%3.0f,%5.1f,%5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.2f, %5.1f, %5.1f, %5.1f\n",
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
            mcffmc,
            mcgmc,
            cur.percent_cured,
            cur.grass_fuel_load);
    printf("%4d,%2d,%2d,%2d,%5.1f,%3.0f,%5.1f,%5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.1f, %5.2f, %5.1f, %5.1f, %5.1f\n",
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
           mcffmc,
           mcgmc,
           cur.percent_cured,
           cur.grass_fuel_load);
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
