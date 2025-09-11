/*  Phase1  NG-FWI.c

inputs full hourly weather sttream only.
new hourly DMC and DC
calcuats solar radiation from lat/long
grass moisture, grass spread index and grass fire weahter index added.\

 THis is only set up for ONE station at a time.   And ONE year really.

version 0.9    (still testing)

bmw/2021
*/

#include "util.h"
#include <stdlib.h>
#include <stdbool.h>

static const double DAILY_K_DMC_DRYING = 1.894;
static const double DAILY_K_DC_DRYING = 3.937;

static const double HOURLY_K_DMC = 2.22;
static const double HOURLY_K_DC = 0.085;
static const double DMC_OFFSET_TEMP = 0.0;
static const double DC_OFFSET_TEMP = 0.0;

static const double DC_DAILY_CONST = 0.36;
static const double DC_HOURLY_CONST =   0.36/3.397; //DC_DAILY_CONST / DAILY_K_DC_DRYING;

static const double OFFSET_SUNRISE = 0.0; //2.5;
static const double OFFSET_SUNSET = 0.0; //0.5;

/* Fuel Load (kg/m^2) */
static const double DEFAULT_GRASS_FUEL_LOAD = 0.35;

/* default startup values */
static const double FFMC_DEFAULT = 85;
static const double DMC_DEFAULT = 6;
static const double DC_DEFAULT = 15;

/* FIX: figure out what this should be */
static const double DEFAULT_LATITUDE = 55.0;
static const double DEFAULT_LONGITUDE = -120.0;

static const double MPCT_TO_MC = 250.0 * 59.5 / 101.0;
static const double FFMC_INTERCEPT = 0.5;
static const double DMC_INTERCEPT = 1.5;
static const double DC_INTERCEPT = 2.8;

/*
 * Fine Fuel Moisture Code (FFMC) from moisture %
 */
double fine_fuel_moisture_code(double moisture_percent)
{
  return (59.5 * (250.0 - moisture_percent) / (MPCT_TO_MC + moisture_percent));
}

/*
 * Fine Fuel Moisture (percent) from FFMC
 */
double fine_fuel_moisture_from_code(double moisture_code)
{
  return MPCT_TO_MC * (101.0 - moisture_code) / (59.5 + moisture_code);
}

/**
 * Calculate hourly Fine Fuel Moisture (percent) value
 *
 * @param temp            Temperature (Celcius)
 * @param rh              Relative Humidity (percent, 0-100)
 * @param ws              Wind Speed (km/h)
 * @param rain            Precipitation (mm)
 * @param lastmc          Previous Fine Fuel Moisture (percent)
 * @return                Hourly Fine Fuel Moisture (percent)
 */
double hourly_fine_fuel_moisture(const double temp,
                                 const double rh,
                                 const double ws,
                                 const double rain,
                                 const double lastmc)
{
  /* printf("%.1f,%.1f,%.1f,%.1f,%.1f\n", temp, rh, ws, rain, lastmc); */
  static const double rf = 42.5;
  static const double drf = 0.0579;
  /* Time since last observation (hours) */
  static const double time = 1.0;
  /* use moisture directly instead of converting to/from ffmc */
  /* expects any rain intercept to already be applied */
  double mo = lastmc;
  if (rain != 0.0)
  {
    /* duplicated in both formulas, so calculate once */
    /* lastmc == mo, but use lastmc since mo changes after first equation */
    mo += rf * rain * exp(-100.0 / (251 - lastmc)) * (1.0 - exp(-6.93 / rain));
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
  const double e1 = 0.18 * (21.1 - temp) * (1.0 - (1.0 / exp(0.115 * rh)));
  const double ed = 0.942 * pow(rh, 0.679) + (11.0 * exp((rh - 100) / 10.0)) + e1;
  const double ew = 0.618 * pow(rh, 0.753) + (10.0 * exp((rh - 100) / 10.0)) + e1;
  /* m = ed if mo >= ed else ew */
  double m = mo < ed
                 ? ew
                 : ed;
  /* printf("%.8f,%.8f,%.8f,%.8f,%.8f,%.8f\n", lastmc, mo, e1, ed, ew, m); */
  if (mo != ed)
  {
    /* these are the same formulas with a different value for a1 */
    const double a1 = mo > ed
                          ? rh / 100.0
                          : (100.0 - rh) / 100.0;
    const double k0_or_k1 = 0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(ws) * (1 - pow(a1, 8)));
    const double kd_or_kw = (1.0/0.50)*drf * k0_or_k1 * exp(0.0365 * temp);
    m += (mo - m) * pow(10, -kd_or_kw * time);
    /* printf("%.8f,%.8f,%.8f,%.8f\n", a1, k0_or_k1, kd_or_kw, m); */
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
double initial_spread_index(double ws, double ffmc)
{
  const double fm = fine_fuel_moisture_from_code(ffmc);
  const double fw = 40 <= ws
                        ? 12 * (1 - exp(-0.0818 * (ws - 28)))
                        : exp(0.05039 * ws);
  const double ff = 91.9 * exp(-0.1386 * fm) * (1.0 + pow(fm, 5.31) / 4.93e07);
  const double isi = 0.208 * fw * ff;
  return isi;
}

/**
 * Calculate Build-up Index (BUI)
 *
 * @param dmc             Duff Moisture Code
 * @param dc              Drought Code
 * @return                Build-up Index
 */
double buildup_index(double dmc, double dc)
{
  double bui = 0 == dmc && 0 == dc
                   ? 0.0
                   : 0.8 * dc * dmc / (dmc + 0.4 * dc);
  if (bui < dmc)
  {
    const double p = (dmc - bui) / dmc;
    const double cc = 0.92 + pow(0.0114 * dmc, 1.7);
    bui = dmc - cc * p;
    if (bui <= 0)
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
double fire_weather_index(double isi, double bui)
{
  const double bb = 0.1 * isi * (bui > 80 ? 1000.0 / (25.0 + 108.64 / exp(0.023 * bui)) : 0.626 * pow(bui, 0.809) + 2.0);
  const double fwi = bb <= 1
                         ? bb
                         : exp(2.72 * pow(0.434 * log(bb), 0.647));
  return fwi;
}

double daily_severity_rating(double fwi)
{
  return 0.0272 * pow(fwi, 1.77);
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
double hourly_grass_fuel_moisture(double temp,
                                  double rh,
                                  double ws,
                                  double rain,
                                  double solrad,
                                  double lastmc)
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
  static const double rf = 0.27;
  static const double drf = 0.389633;
  /* Time since last observation (hours) */
  static const double time = 1.0;
  double mo = lastmc;
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
  const double tf = temp + 17.9 * solrad * exp(-0.034 * ws);
  /* fuel humidity */
  const double rhf = tf > temp
                         ? (rh * 6.107 * pow(10.0, 7.5 * temp / (temp + 237.0)) / (6.107 * pow(10.0, 7.5 * tf / (tf + 237.0))))
                         : rh;
  /* printf("%.1f,%.1f,%.1f,%.1f,%.8f,%.8f\n", temp, rh, ws, rain, tf, rhf); */
  /* duplicated in both formulas, so calculate once */
  const double e1 = rf * (26.7 - tf) * (1.0 - (1.0 / exp(0.115 * rhf)));
  /*GRASS EMC*/
  const double ed = 1.62 * pow(rhf, 0.532) + (13.7 * exp((rhf - 100) / 13.0)) + e1;
  const double ew = 1.42 * pow(rhf, 0.512) + (12.0 * exp((rhf - 100) / 18.0)) + e1;
  /* m = ed if mo >= ed else ew */
  double m = (mo < ed && mo < ew)
                 ? ew
                 : ed;
  /* printf("%.8f,%.8f,%.8f,%.8f,%.8f,%.8f\n", lastmc, mo, e1, ed, ew, m); */
  if (mo > ed || (mo < ed && mo < ew))
  {
    /* these are the same formulas with a different value for a1 */
    const double a1 = mo > ed
                          ? rhf / 100.0
                          : (100.0 - rhf) / 100.0;
    const double k0_or_k1 = 0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(ws) * (1 - pow(a1, 8)));
    const double kd_or_kw = drf * k0_or_k1 * exp(0.0365 * tf);
    m += (mo - m) * pow(10, -kd_or_kw * time);
    /* printf("%.8f,%.8f,%.8f,%.8f\n", a1, k0_or_k1, kd_or_kw, m); */
  }
  return m;
}

double Pign(double mc, double wind2m, double Cint, double Cmc, double Cws)
/* Thisd is the general standard form for the probability of sustained flaming models for each FF cover type
   here :
     mc is cured moisture (%) in the litter fuels being ignited
     wind2m (km/h)  is the estimated 2 metre standard height for wind at hte site of the fire ignition
     Cint, Cmc and Cws   are coefficients for the standard Pign model form for a given FF cover type

     return >> is the Probability of Sustained flaming from a single small flaming ember/brand
*/
{
  const double Prob = 1.0 / (1.0 + exp(-1.0 * (Cint + Cmc * mc + Cws * wind2m)));

  return Prob;
}

double curing_factor(double cur)
/*  cur is the percentage cure of the grass fuel complex.  100= fully cured
  ....The OPPOSITE (100-x) of greenness...

   This is the Cruz et al (2015) model with the original precision of the coefficent estimates
   and as in CSIRO code:https://research.csiro.au/spark/resources/model-library/csiro-grassland-models/

   */
{
  double cf;
  if (cur >= 20.0)
    cf = 1.036 / (1 + 103.989 * exp(-0.0996 * (cur - 20)));
  else
    cf = 0.0;

  return cf;
}

double grass_moisture_code(double mc, double cur, double wind)
{
  /* THIS is the way to get the CODE value from cured grassland moisture
    IT takes fully cured grass moisture  (from the grass moisture model (100% cured)  (from the FMS...updated version of Wotton 2009)
       and a estimate of the fuel complex curing (as percent cured)
       and estimated wind speed (necessary for a calc
    and calculated the probability of sustainable flaming ignition  (a funciton of MC  and wind)
    THEN it accounts for curing effect on probability of fire spread sustainability, using the curing factor from Cruz et al (2015) for grass
    THEN from this calcuates an 'effective moisture content' which is the moisture content that would be required to achieve
       the curing adjusted probabiltiy of sustained flaming if one were calcuating it directly through the standard Pign equation.
    and THEN converts this effective moisture content to a CODE value via the FF-scale the FFMC uses for consistency

    relies on models of:
       Prob of sustained flaming for grass model (PsusF(grass)
       and  the curing_factor  function
       AND and estiamte of open 10 m to 2 m wind reduction (0.75)...hardcoded in here now.....

MC is moisture content (%)
cur=percent curing of the grassland  (%)
wind=  10 m open wind (km/h)

    currently (NOv 2023) the coefficients for the PsusF(grass) models are hardcoded into the GFMC function
  */

  const double wind2m_open_factor = 0.75;
  double probign, wind2m, newPign, egmc;

  const double Intercept = 1.49;
  const double Cmoisture = -0.11;
  const double Cwind = 0.075;
  /* GRASS: these coefficients (above) could change down the road .....explicitly coded in above*/

  wind2m = wind2m_open_factor * wind; /* convert from 10 m wind in open to 2 m wind in open COULD be updated */

  probign = Pign(mc, wind2m, Intercept, Cmoisture, Cwind);

  /* adjust ignition diretctly with the curing function on ROS */
  newPign = curing_factor(cur) * probign;

  /* now to back calc effective moisture - algebraically reverse the Pign equation*/
  if (newPign > 0.0)
    egmc = (log(newPign / (1.0 - newPign)) - Intercept - Cwind * wind2m) / Cmoisture;
  else
    egmc = 250; /* a saturation value just a check*/

  return fine_fuel_moisture_code(egmc); /*   convert to code with FF-scale */
}

double matted_grass_spread_ROS(double ws, double mc, double cur)
/*  CUT grass  Rate  of spread from cheney 1998  (and new CSIRO grassland code
 We use this for MATTED grass in our post-winter context
 --ws=10 m open wind km/h
 --mc = moisture content in  cured grass  (%)
 --cur = percentage of grassland cured  (%)
 output should be ROS in m/min   */

{
  const double fw = 16.67 * (ws < 5 ? 0.054 + 0.209 * ws : 1.1 + 0.715 * (ws - 5.0) * 0.844);

  /* NOTE: between [12, ~12.01754] the value for ws < 10 is greater than ws >= 10 */
  /* using 0.6838 instead would mean this is always less than ws >= 10
  ........this is fine with me   BMW*/
  /* mc < 23.9 because of check at start of function, so last expression is any ws >= 10 */

  const double fm = mc < 12
                        ? exp(-0.108 * mc)
                        : (mc < 20.0 && ws < 10.0
                               ? 0.6838 - 0.0342 * mc
                               : (mc < 23.9 && ws >= 10.0
                                      ? 0.547 - 0.0228 * mc
                                      : 0.0));

  const double cf = curing_factor(cur);

  return (fw * fm * cf);
}

double standing_grass_spread_ROS(double ws, double mc, double cur)
/*  standing grass  Rate  of spread from cheney 1998  (and new CSIRO grassland code)
 We use this for standing grass in our post-winter context
 ITS only the WIND function that chnges here between cut and standing

 --ws=10 m open wind km/h
 --mc = moisture content in grass  (%)
 --cur = percentage of grassland cured  (%)
 output should be ROS in m/min   */

{
  const double fw = 16.67 * (ws < 5 ? 0.054 + 0.269 * ws : 1.4 + 0.838 * (ws - 5.0) * 0.844);

  /* NOTE: between [12, ~12.01754] the value for ws < 10 is greater than ws >= 10 */
  /* using 0.6838 instead would mean this is always less than ws >= 10
  ........this is fine with me   BMW*/
  /* mc < 23.9 because of check at start of function, so last expression is any ws >= 10 */

  const double fm = mc < 12
                        ? exp(-0.108 * mc)
                        : (mc < 20.0 && ws < 10.0
                               ? 0.6838 - 0.0342 * mc
                               : (mc < 23.9 && ws >= 10.0
                                      ? 0.547 - 0.0228 * mc
                                      : 0.0));

  const double cf = curing_factor(cur);

  return fw * fm * cf;
}

/**
 * Calculate Grass Spread Index (GSI)
 *
 * @param ws              10 metre OPEN Wind Speed (km/h)
 * @param mc              Grass moisture content (fully cured grass) (percent)
 * @param cur             Degree of curing (percent, 0-100)
 * @return                Grass Spread Index
 */
double grass_spread_index(double ws, double mc, double cur)
/*
   So we don't have to transition midseason between standing and matted grass spread rate models
   We will simply scale   GSI   by the average of the   matted and standing spread rates

*/

{

  const double ros = (matted_grass_spread_ROS(ws, mc, cur) + standing_grass_spread_ROS(ws, mc, cur)) / 2.0;
  return 1.11 * ros;
}

/**
 * Calculate Grass Fire Weather Index
 *
 * @param gsi               Grass Spread Index
 * @param load              Fuel Load (kg/m^2)
 * @return                  Grass Fire Weather Index
 */
double grass_fire_weather_index(double gsi, double load)
{
  /*  this just converts back to ROS in m/min*/
  const double ros = gsi / 1.11;
  const double Fint = 300.0 * load * ros;
  return Fint > 100
             ? log(Fint / 60.0) / 0.14
             : Fint / 25.0;
}

double dmc_wetting(double rain_total, double lastdmc)
{
  /* compare floats by using tolerance */
  if (rain_total <= DMC_INTERCEPT)
  {
    return 0.0;
  }
  const double b = lastdmc <= 33
                       ? 100.0 / (0.5 + 0.3 * lastdmc)
                       : (lastdmc <= 65
                              ? 14.0 - 1.3 * log(lastdmc)
                              : 6.2 * log(lastdmc) - 17.2);
  const double rw = 0.92 * rain_total - 1.27;
  const double wmi = 20 + 280 / exp(0.023 * lastdmc);
  const double wmr = wmi + 1000 * rw / (48.77 + b * rw);
  double dmc = 43.43 * (5.6348 - log(wmr - 20));
  if (dmc <= 0.0)
  {
    dmc = 0.0;
  }
  /* total amount of wetting since lastdmc */
  const double w = lastdmc - dmc;
  return w;
}

double dc_wetting(double rain_total, double lastdc)
{
  /* compare floats by using tolerance */
  if (rain_total <= DC_INTERCEPT)
  {
    return 0.0;
  }
  const double rw = 0.83 * rain_total - 1.27;
  const double smi = 800 * exp(-lastdc / 400);
  /* TOTAL change for the TOTAL 24 hour rain from FWI1970 model  */
  return 400.0 * log(1.0 + 3.937 * rw / smi);
}

double dmc_wetting_between(double rain_total_previous, double rain_total, double lastdmc)
{
  if (rain_total_previous >= rain_total)
  {
    return 0.0;
  }
  /* wetting is calculated based on initial dmc when rain started and rain since */
  const double current = dmc_wetting(rain_total, lastdmc);
  /* recalculate instead of storing so we don't need to reset this too */
  /* NOTE: rain_total_previous != (rain_total - cur.rain) due to floating point math */
  const double previous = dmc_wetting(rain_total_previous, lastdmc);
  return current - previous;
}

double dc_wetting_between(double rain_total_previous, double rain_total, double lastdc)
{
  if (rain_total_previous >= rain_total)
  {
    return 0.0;
  }
  /* wetting is calculated based on initial dc when rain started and rain since */
  const double current = dc_wetting(rain_total, lastdc);
  /* recalculate instead of storing so we don't need to reset this too */
  /* NOTE: rain_total_previous != (rain_total - cur.rain) due to floating point math */
  const double previous = dc_wetting(rain_total_previous, lastdc);
  return current - previous;
}

double dmc_drying_ratio(double temp, double rh)
{
  return _max(0.0, HOURLY_K_DMC * (temp + DMC_OFFSET_TEMP) * (100.0 - rh) * 0.0001);
}

double duff_moisture_code(double last_dmc,
                          double temp,
                          double rh,
                          double ws,
                          double rain,
                          int mon,
                          int hour,
                          double solrad,
                          double sunrise,
                          double sunset,
                          double *dmc_before_rain,
                          double rain_total_prev,
                          double rain_total)
{
  /* printf("%.1f,%.1f,%.1f,%.1f,%d,%d\n", temp, rh, ws, rain, mon, hour); */
  if (0 == rain_total)
  {
    *dmc_before_rain = last_dmc;
  }
  /* apply wetting since last period */
  double dmc_wetting_hourly = dmc_wetting_between(
      rain_total_prev,
      rain_total,
      *dmc_before_rain);
  /* at most apply same wetting as current value (don't go below 0) */
  double dmc = _max(0.0, last_dmc - dmc_wetting_hourly);
  double sunrise_start = round(sunrise + OFFSET_SUNRISE);
  double sunset_start = round(sunset + OFFSET_SUNSET);
  double dmc_hourly = (((hour >= sunrise_start) && (hour < sunset_start))
                           ? (dmc_drying_ratio(temp, rh))
                           : 0.0);
  dmc = dmc + dmc_hourly;
  /* printf("%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f\n",
         *dmc_before_rain,
         last_dmc,
         dmc,
         dmc_daily,
         df,
         dmc_hourly,
         dmc_wetting_hourly); */
  return dmc;
}

double dc_drying_hourly(double temp)
{
  return _max(0.0, HOURLY_K_DC * (temp + DC_OFFSET_TEMP));
}

double drought_code_mike_version(
    double last_dc,
    double temp,
    double rh,
    double ws,
    double rain,
    int mon,
    int hour,
    double solrad,
    double sunrise,
    double sunset,
    double* dc_before_rain,
    double rain_total_prev,
    double rain_total){
  double offset = 3.0;
  double mult = 0.015;
  double pe = 0.0;
  double rw = 0.0;
  double mr = 0.0;
  double mcdc = 0.0;
  
  double last_mc_dc = 400*exp(-last_dc/400);
  double TIME_INCREMENT = 1.0;
  if (temp > 0){
    pe = mult * temp + offset/16.0;
  }
  
  double invtau = pe/400.0;
  
  if (rain_total_prev + rain <= 2.8){
    mr = last_mc_dc;
  }
  else {
    if (rain_total_prev <= 2.8){
      rw = (rain_total + rain)*0.83 - 1.27;
    }
    else {
      rw = rain*0.83;
    }
    mr = last_mc_dc + 3.937*rw/2.0;
  }
  
  if(mr > 400.0){
    mr = 400.0;
  }
  
  bool is_daytime = false;
  if((hour >= sunrise) && (hour <= sunset)){
    is_daytime = true;
  }
  
  if(is_daytime){
    mcdc = 0.0 + (mr+0.0)*exp(-1.0*TIME_INCREMENT*invtau);
  }
  else{
    mcdc = mr;
  }
  if (mcdc > 400.0){
    mcdc = 400.0;
  }
  double dc = 400.0*log(400.0/mcdc);
  return dc;
  
}

double drought_code(double last_dc,
                    double temp,
                    double rh,
                    double ws,
                    double rain,
                    int mon,
                    int hour,
                    double solrad,
                    double sunrise,
                    double sunset,
                    double *dc_before_rain,
                    double rain_total_prev,
                    double rain_total)
{
  //#################################################################################
  // for now we are using Mike's method for calculating DC
  if (0 == rain_total){
    *dc_before_rain = last_dc;
  }
  double dc = drought_code_mike_version(
    last_dc = last_dc,
    temp = temp,
    rh = rh,
    ws = ws,
    rain = rain,
    mon = mon,
    hour = hour,
    solrad = solrad,
    sunrise = sunrise,
    sunset = sunset,
    dc_before_rain = dc_before_rain,
    rain_total_prev = rain_total_prev,
    rain_total = rain_total);
  return dc;
  
  //###################################################################################
  
 
 // if (0 == rain_total)
  //{
    //*dc_before_rain = last_dc;
  //}
  /* apply wetting since last period */
  //double dc_wetting_hourly = dc_wetting_between(rain_total_prev, rain_total, *dc_before_rain);
  /* at most apply same wetting as current value (don't go below 0) */
  //double dc = _max(0.0, last_dc - dc_wetting_hourly);
  //double dc_hourly = dc_drying_hourly(temp);
  /*   double drying = HOURLY_K_DC * (temp + DC_OFFSET_TEMP);
    //printf("temp=%0.2f, HOURLY_K_DC=%0.3f, DC_OFFSET_TEMP=%0.2f, drying=%0.2f, _max=%0.2f, last_dc=%0.2f, dc_wetting_hourly=%0.2f, dc=%0.2f, dc_hourly=%0.2f\n",
      //     temp,
        //   HOURLY_K_DC,
          // DC_OFFSET_TEMP,
           //drying,
          // _max(0.0, drying),
          // last_dc,
           //dc_wetting_hourly,
           //dc,
           //dc_hourly); */
  //dc = dc + dc_hourly;
  //return dc;
}

/*
 * Calculate number of drying "units" this hour contributes
 */
double drying_units(double temp, double rh, double wind, double rain, double solrad)
{
  /* for now, just add 1 drying "unit" per hour */
  return 1.0;
}
struct rain_intercept
{
  double rain_total;
  double rain_total_prev;
  double drying_since_intercept;
};

/* HACK: use struct so it's closer to how R can return multiple values */
void rain_since_intercept_reset(double temp,
                                double rh,
                                double ws,
                                double rain,
                                int mon,
                                int hour,
                                double solrad,
                                double sunrise,
                                double sunset,
                                struct rain_intercept *canopy)
{
  /* for now, want 5 "units" of drying (which is 1 per hour to start) */
  static const double TARGET_DRYING_SINCE_INTERCEPT = 5;
  if (0 < rain)
  {
    /* no drying if still raining */
    canopy->drying_since_intercept = 0.0;
  }
  else
  {
    canopy->drying_since_intercept += drying_units(temp, rh, ws, rain, solrad);
    if (canopy->drying_since_intercept >= TARGET_DRYING_SINCE_INTERCEPT)
    {
      /* reset rain if intercept reset criteria met */
      canopy->rain_total = 0.0;
      canopy->drying_since_intercept = 0.0;
    }
  }
  canopy->rain_total_prev = canopy->rain_total;
  canopy->rain_total += rain;
}

int main(int argc, char *argv[])
{
  /*  CSV headers */
  static const char *header = "lat,long,yr,mon,day,hr,temp,rh,ws,prec,solrad,percent_cured,grass_fuel_load";
  static const char *header_out = "lat,long,yr,mon,day,hr,temp,rh,ws,prec,solrad,ffmc,dmc,dc,isi,bui,fwi,dsr,gfmc,gsi,gfwi,mcffmc,mcgfmc,percent_cured,grass_fuel_load";
  if (7 != argc)
  {
    printf("Command line:   %s <local GMToffset> <starting FFMC> <starting DMC> <starting DC> <input file> <output file>\n\n", argv[0]);
    printf("<local GMToffset> is the off of Greenich mean time (for Eastern = -5  Central=-6   MT=-7  PT=-8 )  \n");
    printf("All times should be local standard time\n");
    printf("INPUT FILE format must be HOURLY weather data, comma seperated and take the form\n");
    printf("%s\n\n", header);
    exit(1);
  }

  FILE *inp = fopen(argv[5], "r");
  printf("Opening input file >>> %s   \n", argv[5]);
  if (inp == NULL)
  {
    printf("\n\n ***** FILE  %s  does not exist\n", argv[5]);
    exit(1);
  }
  int TZadjust = atoi(argv[1]);
  if (TZadjust < -9 || TZadjust > -2)
  {
    printf("/n *****   Local time zone adjustment must be vaguely in Canada so between -9 and -2 \n");
    exit(1);
  }
  double ffmc_old = atof(argv[2]);
  if (ffmc_old > 101 || ffmc_old < 0)
  {
    printf(" /n/n *****   FFMC must be between 0 and 101 \n");
    exit(1);
  }
  const double dmc_old = atof(argv[3]);
  if (dmc_old < 0)
  {
    printf(" /n/n *****  starting DMC must be >=0  \n");
    exit(1);
  }
  double dc_old = atof(argv[4]);
  if (dc_old < 0)
  {
    printf(" /n/n *****   starting DC must be >=0\n");
    exit(1);
  }
  /* printf("TZ=%d    start ffmc=%f  dmc=%f\n", TZadjust, ffmc_old, dmc_old); */
  double mcffmc = fine_fuel_moisture_from_code(ffmc_old);
  /* assuming this is fine because swiss sfms uses it now */
  double mcgfmc = mcffmc;
  /* check that the header matches what is expected */
  check_header(inp, header);
  struct row cur;
  int err = read_row_inputs(inp, &cur);
  struct row old = {0};
  double dmc = dmc_old;
  double dmc_before_rain = dmc_old;
  double dc = dc_old;
  double dc_before_rain = dc_old;
  double sunrise;
  double sunset;
  struct rain_intercept canopy = {0.0, 0.0, 0.0};
  FILE *out = fopen(argv[6], "w");
  fprintf(out, "%s\n", header_out);
  while (err > 0)
  {
    if (cur.day != old.day || cur.mon != old.mon)
    {
      /* Only need to calculate sunrise/sunset once per day */
      sunrise_sunset(cur.lat, cur.lon, cur.mon, cur.day, TZadjust, &sunrise, &sunset);
    }
    cur.sunrise = sunrise;
    cur.sunset = sunset;
    rain_since_intercept_reset(
        cur.temp,
        cur.rh,
        cur.ws,
        cur.rain,
        cur.mon,
        cur.hour,
        cur.solrad,
        cur.sunrise,
        cur.sunset,
        &canopy);
    /* use lesser of remaining intercept and current hour's rain */
    double rain_ffmc = canopy.rain_total <= FFMC_INTERCEPT
                           ? 0.0
                           : ((canopy.rain_total - FFMC_INTERCEPT) > cur.rain
                                  ? cur.rain
                                  : canopy.rain_total - FFMC_INTERCEPT);
    mcffmc = hourly_fine_fuel_moisture(cur.temp, cur.rh, cur.ws, rain_ffmc, mcffmc);
    /* convert to code for output, but keep using moisture % for precision */
    double ffmc = fine_fuel_moisture_code(mcffmc);
    /* not ideal, but at least encapsulates the code for each index */
    dmc = duff_moisture_code(
        dmc,
        cur.temp,
        cur.rh,
        cur.ws,
        cur.rain,
        cur.mon,
        cur.hour,
        cur.solrad,
        cur.sunrise,
        cur.sunset,
        &dmc_before_rain,
        canopy.rain_total_prev,
        canopy.rain_total);
    dc = drought_code(
        dc,
        cur.temp,
        cur.rh,
        cur.ws,
        cur.rain,
        cur.mon,
        cur.hour,
        cur.solrad,
        cur.sunrise,
        cur.sunset,
        &dc_before_rain,
        canopy.rain_total_prev,
        canopy.rain_total);
    double isi = initial_spread_index(cur.ws, ffmc);
    double bui = buildup_index(dmc, dc);
    double fwi = fire_weather_index(isi, bui);
    double dsr = daily_severity_rating(fwi);
    mcgfmc = hourly_grass_fuel_moisture(cur.temp, cur.rh, cur.ws, cur.rain, cur.solrad, mcgfmc);
    double gfmc = grass_moisture_code(mcgfmc, cur.percent_cured, cur.ws);
    double gsi = grass_spread_index(cur.ws, mcgfmc, cur.percent_cured);
    double gfwi = grass_fire_weather_index(gsi, cur.grass_fuel_load);
    /* printf("\n"); */
    save_csv(out,
             "%.4f,%.4f,%4d,%02d,%02d,%02d,%.1f,%.0f,%.1f,%.2f,%.4f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.4f,%.4f,%.1f,%.2f\n",
             cur.lat,
             cur.lon,
             cur.year,
             cur.mon,
             cur.day,
             cur.hour,
             cur.temp,
             cur.rh,
             cur.ws,
             cur.rain,
             cur.solrad,
             ffmc,
             dmc,
             dc,
             isi,
             bui,
             fwi,
             dsr,
             gfmc,
             gsi,
             gfwi,
             mcffmc,
             mcgfmc,
             cur.percent_cured,
             cur.grass_fuel_load);
    /*     printf("%.4f,%.4f,%4d,%02d,%02d,%02d,%.1f,%.0f,%.1f,%.2f,%.4f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.4f,%.4f,%.1f,%.2f\n",
               cur.lat,
               cur.lon,
               cur.year,
               cur.mon,
               cur.day,
               cur.hour,
               cur.temp,
               cur.rh,
               cur.ws,
               cur.rain,
               cur.solrad,
               ffmc,
               dmc,
               dc,
               isi,
               bui,
               fwi,
               dsr,
               gfmc,
               gsi,
               gfwi,
               mcffmc,
               mcgfmc,
               cur.percent_cured,
               cur.grass_fuel_load); */
    old = cur;
    err = read_row_inputs(inp, &cur);
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

  return 0;
}
