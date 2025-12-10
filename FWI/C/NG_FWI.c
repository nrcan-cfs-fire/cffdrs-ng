#include "util.h"
#include "NG_FWI.h"
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>



// see NG_FWI.h for variable definitions and function help

double ffmc_to_mcffmc(double ffmc) {
  return MPCT_TO_MC * (101.0 - ffmc) / (59.5 + ffmc);
}

double mcffmc_to_ffmc(double mcffmc) {
  return (59.5 * (250.0 - mcffmc) / (MPCT_TO_MC + mcffmc));
}

double dmc_to_mcdmc(double dmc) {
  return 280.0 / exp(dmc / 43.43) + 20.0;
}

double mcdmc_to_dmc(double mcdmc) {
  return 43.43 * log(280.0 / (mcdmc - 20.0));
}

double dc_to_mcdc(double dc) {
  return 400.0 * exp(-dc / 400.0);
}

double mcdc_to_dc(double mcdc) {
  return 400.0 * log(400.0 / mcdc);
}

double hourly_fine_fuel_moisture(double lastmc, double temp, double rh, double ws,
  double rain, double time_increment)
{
  static const double rf = 42.5;
  static const double drf = 0.0579;
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

  double m;
  if (mo < ed) {
    m = ew;
  } else {
    m = ed;
  }
  
  if (mo != ed)
  {
    double a1, k0_or_k1, kd_or_kw;
    /* these are the same formulas with a different value for a1 */
    if (mo > ed) {
      a1 = rh / 100.0;
    } else {
      a1 = (100.0 - rh) / 100.0;
    }
    k0_or_k1 = 0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(ws) * (1 - pow(a1, 8)));
    kd_or_kw = (1.0 / 0.50) * drf * k0_or_k1 * exp(0.0365 * temp);
    m += (mo - m) * pow(10, -kd_or_kw * time_increment);
    
  }
  return m;
}

double duff_moisture_code(double last_mcdmc, int hour,
  double temp, double rh, double prec, double sunrise, double sunset,
  double prec_cumulative_prev, double time_increment)
{
  double mr, mcdmc;

  // wetting
  if (prec_cumulative_prev + prec > DMC_INTERCEPT) {  // prec_cumulative above threshold
    double rw, last_dmc, b;

    if (prec_cumulative_prev < DMC_INTERCEPT) {  // just passed threshold
      rw = (prec_cumulative_prev + prec) * 0.92 - 1.27;
    } else {
      rw = prec * 0.92;
    }

    last_dmc = mcdmc_to_dmc(last_mcdmc);
    if (last_dmc <= 33.0) {
      b = 100.0 / (0.3 * last_dmc + 0.5);
    } else if (last_dmc <= 65.0) {
      b = -1.3 * log(last_dmc) + 14.0;
    } else {
      b = 6.2 * log(last_dmc) - 17.2;
    }

    mr = last_mcdmc + 1e3 * rw / (b * rw + 48.77);
  } else {  // prec_cumulative below threshold
    mr = last_mcdmc;
  }

  if (mr > 300.0) {
    mr = 300.0;
  }
  
  // drying
  double sunrise_start, sunset_start;
  sunrise_start = sunrise + OFFSET_SUNRISE;
  sunset_start = sunset + OFFSET_SUNSET;
  // since sunset can be > 24, in some cases we ignore change between days and check hr + 24
  double hr = (double) hour;
  if ((hr >= sunrise && hr <= sunset) ||
    (hr < 6 && hr + 24 >= sunrise && hr + 24 <= sunset)) {  // day
    double rk, invtau;

    if (temp < 0) {
      temp = 0.0;
    }
    rk = HOURLY_K_DMC * 1e-4 * (temp + DMC_OFFSET_TEMP) * (100.0 - rh);
    invtau = rk / 43.43;
    mcdmc = (mr - 20.0) * exp(-time_increment * invtau) + 20.0;
  } else {  // night
    mcdmc = mr;  // no overnight drying
  }

  if (mcdmc > 300.0) {
    mcdmc = 300.0;
  }

  return mcdmc;
}

double drought_code(double last_mcdc, int hour, double temp, double prec,
  double sunrise, double sunset, double prec_cumulative_prev, double time_increment)
{
  double mr, mcdc;

  // wetting
  if (prec_cumulative_prev + prec > DC_INTERCEPT) {  // prec_cumulative above threshold
    double rw;

    if (prec_cumulative_prev <= DC_INTERCEPT) {  // just passed threshold
      rw = (prec_cumulative_prev + prec) * 0.83 - 1.27;
    } else {
      rw = prec * 0.83;
    }
    mr = last_mcdc + 3.937 * rw / 2.0;
  } else {
    mr = last_mcdc;
  }
  
  if (mr > 400.0) {
    mr = 400.0;
  }

  // drying
  double sunrise_start, sunset_start;
  sunrise_start = sunrise + OFFSET_SUNRISE;
  sunset_start = sunset + OFFSET_SUNSET;
  // since sunset can be > 24, in some cases we ignore change between days and check hr + 24
  double hr = (double) hour;
  if ((hr >= sunrise && hr <= sunset) ||
    (hr < 6 && hour + 24 >= sunrise && hr + 24 <= sunset)) {
    double pe, invtau;
    double offset = 3.0;
    double mult = 0.015;
    if (temp > 0) {
      pe = mult * temp + offset / 16.0;
    } else {
      pe = 0.0;
    }
    invtau = pe / 400.0;
    mcdc = mr * exp(-time_increment * invtau);
  } else {
    mcdc = mr;
  }

  if (mcdc > 400.0) {
    mcdc = 400.0;
  }
  
  return mcdc;
}

double initial_spread_index(double ws, double ffmc)
{
  double mcffmc, fw, ff, isi;
  mcffmc = ffmc_to_mcffmc(ffmc);
  fw = (40 <= ws) ? 12 * (1 - exp(-0.0818 * (ws - 28))) : exp(0.05039 * ws);
  ff = 91.9 * exp(-0.1386 * mcffmc) * (1.0 + pow(mcffmc, 5.31) / 4.93e7);
  isi = 0.208 * fw * ff;
  return isi;
}

double buildup_index(double dmc, double dc)
{
  double bui, p, cc;
  bui = (0 == dmc && 0 == dc) ? 0.0 : 0.8 * dc * dmc / (dmc + 0.4 * dc);
  if (bui < dmc)
  {
    p = (dmc - bui) / dmc;
    cc = 0.92 + pow(0.0114 * dmc, 1.7);
    bui = dmc - cc * p;
    if (bui <= 0.0)
    {
      bui = 0.0;
    }
  }
  return bui;
}

double fire_weather_index(double isi, double bui)
{
  double bb, fwi;
  if (bui > 80) {
    bb = 0.1 * isi * 1000.0 / (25.0 + 108.64 / exp(0.023 * bui));
  } else {
    bb = 0.1 * isi * (0.626 * pow(bui, 0.809) + 2.0);
  }
  fwi = (bb <= 1) ? bb : exp(2.72 * pow(0.434 * log(bb), 0.647));
  return fwi;
}

double daily_severity_rating(double fwi)
{
  return 0.0272 * pow(fwi, 1.77);
}

/**
 * Calculate Hourly Grass Fuel Moisture. Needs to be converted to get GFMC.
 * MARK II of the model (2016) wth new solar rad model specific to grass
 *
 * @param temp            Temperature (Celcius)
 * @param rh              Relative Humidity (percent, 0-100)
 * @param ws              Wind Speed (km/h)
 * @param rain            Precipitation (mm)
 * @param lastmc          Previous grass fuel moisture (percent)
 * @param solrad          Solar radiation (kW/m^2)
 * @return                Grass Fuel Moisture (percent)
 */
double hourly_grass_fuel_moisture(double temp, double rh, double ws, double rain,
  double solrad, double lastmc, double load)
{  
  static const double rf = 0.27;
  // DRF of 1/16.1 comes from reducting the standard response time curve
  // at 26.7C, 20%RH, 2 km/h to 0.85hr.
  static const double drf = 0.389633;
  /* Time since last observation (hours) */
  static const double time_increment = 1.0;
  double mo = lastmc;

  if (rain != 0)
  {
    mo += rain / load * 100.0;
    if (mo > 250.0)
    {
      mo = 250.0;
    }
  }

  /* temp of fuel from CEVW and RH of fuel */
  double tf, rhf;
  tf = temp + 17.9 * solrad * exp(-0.034 * ws);
  if (tf > temp) {
    rhf = rh * pow(10.0, 7.5 * temp / (temp + 237.0)) /
      pow(10.0, 7.5 * tf / (tf + 237.0));
  } else {
    rhf = rh;
  }
  
  // Equilibrium moisture contents
  double e1, ed, ew;
  e1 = rf * (26.7 - tf) * (1.0 - (1.0 / exp(0.115 * rhf)));
  ed = 1.62 * pow(rhf, 0.532) + (13.7 * exp((rhf - 100) / 13.0)) + e1;
  ew = 1.42 * pow(rhf, 0.512) + (12.0 * exp((rhf - 100) / 18.0)) + e1;
  
  // Moisture content differences
  double moed = mo - ed;
  double moew = mo - ew;

  double e, a1, m, moe;
  if (moed == 0 || (moew >= 0 && moed < 0)) {
    m = mo;
    if (moed == 0) {
      e = ed;
    }
    if (moew >= 0) {
      e = ew;
    }
  } else {
    if (moed > 0) {
      a1 = rhf / 100.0;
      e = ed;
      moe = moed;
    } else {
      a1 = (100.0 - rhf) / 100.0;
      e = ew;
      moe = moew;
    }
    if (a1 < 0) {
      // avoids complex number in a1^1.7 xkd calculation
      a1 = 0;
    }

    double xkd;
    xkd = (0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(ws) * (1 - pow(a1, 8)))) *
      drf * exp(0.0365 * tf);
    m = e + moe * exp(-1.0 * log(10.0) * xkd * time_increment);
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


  if(egmc > 250.0){
    egmc = 250.0;
  }
  return mcffmc_to_ffmc(egmc); /*   convert to code with FF-scale */
}

double matted_grass_spread_ROS(double ws, double mc, double cur)
/*  CUT grass  Rate  of spread from cheney 1998  (and new CSIRO grassland code
 We use this for MATTED grass in our post-winter context
 --ws=10 m open wind km/h
 --mc = moisture content in  cured grass  (%)
 --cur = percentage of grassland cured  (%)
 output should be ROS in m/min   */

{
  const double fw = 16.67 * (ws < 5 ? 0.054 + 0.209 * ws : 1.1 + 0.715 * pow((ws - 5.0),0.844));

  /* NOTE: between [12, ~12.01754] the value for ws < 10 is greater than ws >= 10 */
  /* using 0.6838 instead would mean this is always less than ws >= 10
  ........this is fine with me   BMW*/
  /* mc < 23.9 because of check at start of function, so last expression is any ws >= 10 */

  double fm = mc < 12
                        ? exp(-0.108 * mc)
                        : (mc < 20.0 && ws < 10.0
                               ? 0.6838 - 0.0342 * mc
                               : (mc < 23.9 && ws >= 10.0
                                      ? 0.547 - 0.0228 * mc
                                      : 0.0));

  if (fm < 0.0){
    fm = 0.0;
  }


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
  const double fw = 16.67 * (ws < 5 ? 0.054 + 0.269 * ws : 1.4 + 0.838 * pow((ws - 5.0),0.844));

  /* NOTE: between [12, ~12.01754] the value for ws < 10 is greater than ws >= 10 */
  /* using 0.6838 instead would mean this is always less than ws >= 10
  ........this is fine with me   BMW*/
  /* mc < 23.9 because of check at start of function, so last expression is any ws >= 10 */

  double fm = mc < 12
                        ? exp(-0.108 * mc)
                        : (mc < 20.0 && ws < 10.0
                               ? 0.6838 - 0.0342 * mc
                               : (mc < 23.9 && ws >= 10.0
                                      ? 0.547 - 0.0228 * mc
                                      : 0.0));

  if (fm < 0.0){
    fm = 0.0;
  }

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
double grass_spread_index(double ws, double mc, double cur, bool standing)
/*
   So we don't have to transition midseason between standing and matted grass spread rate models
   We will simply scale   GSI   by the average of the   matted and standing spread rates

*/

{

 // const double ros = (matted_grass_spread_ROS(ws, mc, cur) + standing_grass_spread_ROS(ws, mc, cur)) / 2.0;


  //now allowing switch between standing and matted grass
  double ros = 0;
  if (standing){
    ros = standing_grass_spread_ROS(ws,mc,cur);
    
  }
  else{
    ros = matted_grass_spread_ROS(ws,mc,cur);
    
  }

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

/*
 * Calculate number of drying "units" this hour contributes
 */
double drying_units(double temp, double rh, double wind, double rain, double solrad)
{
  /* for now, just add 1 drying "unit" per hour */
  return 1.0;
}

/* HACK: use struct so it's closer to how R can return multiple values */
void rain_since_intercept_reset(double temp, double rh, double ws, double rain,
  int mon, int hour, double solrad, double sunrise, double sunset,
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
