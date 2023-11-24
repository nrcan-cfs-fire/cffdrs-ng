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

static const double HOURLY_K_DMC = 2.10;
static const double HOURLY_K_DC = 0.017;
static const double DC_OFFSET_TEMP = 0.0;

static const double OFFSET_SUNRISE = 2.5;
static const double OFFSET_SUNSET = 0.5;

/* Fuel Load (kg/m^2) */
static const double DEFAULT_GRASS_FUEL_LOAD = 0.35;
static const double MAX_SOLAR_PROPAGATION = 0.85;

/* default startup values */
static const double FFMC_DEFAULT = 85;
static const double DMC_DEFAULT = 6;
static const double DC_DEFAULT = 15;

/* FIX: figure out what this should be */
static const double DEFAULT_LATITUDE = 55.0;
static const double DEFAULT_LONGITUDE = -120.0;

static const double MPCT_TO_MC = 147.2772277;
static const double FFMC_INTERCEPT = 0.5;
static const double DMC_INTERCEPT = 1.5;
static const double DC_INTERCEPT = 2.8;

/*
 * Fine Fuel Moisture Code (FFMC) from moisture %
 */
double fine_fuel_moisture_code(double moisture_percent)
{
  return (59.5 * (250 - moisture_percent) / (MPCT_TO_MC + moisture_percent));
}

/*
 * Fine Fuel Moisture (percent) from FFMC
 */
double fine_fuel_moisture_from_code(double moisture_code)
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
 * @param lastmc          Previous Fine Fuel Moisture (percent)
 * @return                Hourly Fine Fuel Moisture (percent)
 */
double hourly_fine_fuel_moisture(const double temp,
                                 const double rh,
                                 const double ws,
                                 const double rain,
                                 const double lastmc)
{
  printf("%.1f,%.1f,%.1f,%.1f,%.1f\n", temp, rh, ws, rain, lastmc);
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
  printf("%.8f,%.8f,%.8f,%.8f,%.8f,%.8f\n", lastmc, mo, e1, ed, ew, m);
  if (mo != ed)
  {
    /* these are the same formulas with a different value for a1 */
    const double a1 = mo > ed
                      ? rh / 100.0
                      : (100.0 - rh) / 100.0;
    const double k0_or_k1 = 0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(ws) * (1 - pow(a1, 8)));
    const double kd_or_kw = drf * k0_or_k1 * exp(0.0365 * temp);
    m += (mo - m) * pow(10, -kd_or_kw * time);
    printf("%.8f,%.8f,%.8f,%.8f\n", a1, k0_or_k1, kd_or_kw, m);
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
  const double sf = 19.115 * exp(-0.1386 * fm) * (1.0 + pow(fm, 5.31) / 4.93e07);
  const double isi = sf * fw;
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
double fire_weather_index(double isi, double bui)
{
  const double bb = 0.1 * isi
                  * (bui > 80
                       ? 1000.0 / (25.0 + 108.64 / exp(0.023 * bui))
                       : 0.626 * pow(bui, 0.809) + 2.0);
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
                     ? (rh * 6.107 * pow(10.0, 7.5 * temp / (temp + 237.0))
                        / (6.107 * pow(10.0, 7.5 * tf / (tf + 237.0))))
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

/**
 * Calculate Grass Spread Index (GSI)
 *
 * @param ws              Wind Speed (km/h)
 * @param mc              Grass moisture content (percent)
 * @param cur             Degree of curing (percent, 0-100)
 * @return                Grass Spread Index
 */
double grass_spread_index(double ws, double mc, double cur)
{
  const double fw = 16.67 * (ws < 5 ? 0.054 + 0.209 * ws : 1.1 + 0.715 * (ws - 5.0) * 0.844);
  /* NOTE: between [12, ~12.01754] the value for ws < 10 is greater than ws >= 10 */
  /* using 0.6838 instead would mean this is always less than ws >= 10 */
  /* mc < 23.9 because of check at start of function, so last expression is any ws >= 10 */
  const double fm = mc < 12
                    ? exp(-0.108 * mc)
                    : (mc < 20.0 && ws < 10.0
                         ? 0.684 - 0.0342 * mc
                         : (mc < 23.9 && ws >= 10.0
                              ? 0.547 - 0.0228 * mc
                              : 0.0));
  const double cf = cur > 20
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
  const double reff = 0.92 * rain_total - 1.27;
  /* This is the change in MC (moisturecontent)  from FULL DAY's rain  */
  return 1000.0 * reff / (48.77 + b * reff);
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
  return _max(0.0, (temp + 1.1) * (100.0 - rh) * 0.0001);
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
                          double* dmc_before_rain,
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
  double sunrise_start = _round(sunrise + OFFSET_SUNRISE, 0);
  double sunset_start = _round(sunset + OFFSET_SUNSET, 0);
  double dmc_hourly = (((hour >= sunrise_start) && (hour < sunset_start))
                         ? (HOURLY_K_DMC * dmc_drying_ratio(temp, rh))
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

double dc_drying_hourly(temp)
{
  return _max(0.0, HOURLY_K_DC * (temp + DC_OFFSET_TEMP));
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
                    double* dc_before_rain,
                    double rain_total_prev,
                    double rain_total)
{
  if (0 == rain_total)
  {
    *dc_before_rain = last_dc;
  }
  /* apply wetting since last period */
  double dc_wetting_hourly = dc_wetting_between(rain_total_prev, rain_total, *dc_before_rain);
  /* at most apply same wetting as current value (don't go below 0) */
  if (dc_wetting_hourly > last_dc)
  {
    dc_wetting_hourly = last_dc;
  }
  /* should be no way this is below 0 because we just made sure it wasn't > dc */
  double dc = last_dc - dc_wetting_hourly;
  double dc_hourly = dc_drying_hourly(temp);
  dc = dc + dc_hourly;
  return (dc);
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
                                struct rain_intercept* canopy)
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

int populate_row(FILE* inp, struct row* cur, double TZadjust)
{
  int err = read_row(inp, cur);
  cur->solrad = sun(cur->lat, cur->lon, cur->mon, cur->day, cur->hour, TZadjust, &(cur->sunrise), &(cur->sunset));
  double julian_day = julian(cur->mon, cur->day);
  /* assuming we want curing to change based on current day and not remain */
  /* the same across entire period based on start date */
  cur->percent_cured = seasonal_curing(julian_day);
  /* FIX: use a constant grass fuel load for now */
  cur->grass_fuel_load = DEFAULT_GRASS_FUEL_LOAD;
  return err;
}

void main(int argc, char* argv[])
{
  /*  CSV headers */
  static const char* header = "lat,long,yr,mon,day,hr,temp,rh,ws,prec";
  static const char* header_out = "lat,long,yr,mon,day,hr,temp,rh,ws,prec,solrad,ffmc,dmc,dc,isi,bui,fwi,dsr,gfmc,gsi,gfwi,mcffmc,mcgfmc,percent_cured,grass_fuel_load";
  if (7 != argc)
  {
    printf("Command line:   %s <local GMToffset> <starting FFMC> <starting DMC> <starting DC> <input file> <output file>\n\n", argv[0]);
    printf("<local GMToffset> is the off of Greenich mean time (for Eastern = -5  Central=-6   MT=-7  PT=-8 )  \n");
    printf("All times should be local standard time\n");
    printf("INPUT FILE format must be HOURLY weather data, comma seperated and take the form\n");
    printf("%s\n\n", header);
    exit(1);
  }

  FILE* inp = fopen(argv[5], "r");
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
  double ffmc = atof(argv[2]);
  if (ffmc > 101 || ffmc < 0)
  {
    printf(" /n/n *****   FFMC must be between 0 and 101 \n");
    exit(1);
  }
  const double dmc_startup = atof(argv[3]);
  if (dmc_startup < 0)
  {
    printf(" /n/n *****  starting DMC must be >=0  \n");
    exit(1);
  }
  double dc_startup = atof(argv[4]);
  if (dc_startup < 0)
  {
    printf(" /n/n *****   starting DC must be >=0\n");
    exit(1);
  }
  /* printf("TZ=%d    start ffmc=%f  dmc=%f\n", TZadjust, ffmc, dmc_startup); */
  double mcffmc = fine_fuel_moisture_from_code(ffmc);
  /* approximation for a start up*/
  /* double mcgfmc = 101.0 - ffmc; */
  /* assuming this is fine because swiss sfms uses it now */
  double mcgfmc = mcffmc;
  /*
  # FIX: implement this
  # HACK: always start from daily value at noon
  while (12 != r[1]$hr) {
    r <- r[2:nrow(r)]
  }
  cur <- r[1]
  dmc_old <- daily_duff_moisture_code(dmc_old, cur$temp, cur$rh, cur$prec, cur$lat, cur$mon)
  dc_old <- daily_drought_code(dc_old, cur$temp, cur$rh, cur$prec, cur$lat, cur$mon)
  # HACK: start from when daily value should be "accurate"
  prec_accum <- 0.0
  while (HOUR_TO_START_FROM != r[1]$hr) {
    # tally up precip between noon and whenever we're applying the indices
    prec_accum <- prec_accum + r[1]$prec
    r <- r[2:nrow(r)]
  }
  cur <- r[1]
  */
  /* check that the header matches what is expected */
  check_header(inp, header);
  struct row cur;
  int err = populate_row(inp, &cur, TZadjust);
  struct row old = cur;
  double dmc = dmc_startup;
  double dmc_before_rain = dmc_startup;
  double dc = dc_startup;
  double dc_before_rain = dc_startup;
  struct rain_intercept canopy = {0.0, 0.0, 0.0};
  FILE* out = fopen(argv[6], "w");
  fprintf(out, "%s\n", header_out);
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
    ffmc = fine_fuel_moisture_code(mcffmc);
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
    double gfmc = fine_fuel_moisture_code(mcgfmc);
    double gsi = grass_spread_index(cur.ws, mcgfmc, cur.percent_cured);
    double gfwi = grass_fire_weather_index(gsi, cur.grass_fuel_load);
    /* printf("\n"); */
    fprintf(out,
            "%.4f,%.4f,%4d,%02d,%02d,%02d,%.1f,%.0f,%.1f,%.1f,%.4f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.4f,%.4f,%.1f,%.2f\n",
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
    printf("%.4f,%.4f,%4d,%02d,%02d,%02d,%.1f,%.0f,%.1f,%.1f,%.4f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.4f,%.4f,%.1f,%.2f\n",
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
