#ifndef _NG_FWI_H
#define _NGFWI_H
#include "util.h"
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#endif



////// Variable Definitions
// Only change these if you know what you are doing, or
// reach out to the CFS Fire Danger Group for more info

// Startup moisture code values
static const double FFMC_DEFAULT = 85;
static const double DMC_DEFAULT = 6;
static const double DC_DEFAULT = 15;

// FFMC moisture content to code conversion factor
static const double MPCT_TO_MC = 250.0 * 59.5 / 101.0;

// Wetting variables
static const double DAILY_K_DMC_DRYING = 1.894;
static const double DAILY_K_DC_DRYING = 3.937;
static const double DC_DAILY_CONST = 0.36;
static const double DC_HOURLY_CONST = 0.36 / 3.397;  // DC_DAILY_CONST / DAILY_K_DC_DRYING;

// Precipitation intercept
static const double FFMC_INTERCEPT = 0.5;
static const double DMC_INTERCEPT = 1.5;
static const double DC_INTERCEPT = 2.8;

// Drying variables
static const double OFFSET_SUNRISE = 0.0;
static const double OFFSET_SUNSET = 0.0;
static const double HOURLY_K_DMC = 2.22;
static const double HOURLY_K_DC = 0.085;
static const double DMC_OFFSET_TEMP = 0.0;
static const double DC_OFFSET_TEMP = 0.0;

/* Fuel Load (kg/m^2) */
static const double DEFAULT_GRASS_FUEL_LOAD = 0.35;

// Start of grassland fuels curing (default March 12th)
static const int MON_CURING = 3;
static const int DAY_CURING = 12;

// Transition from matted to standing grass in a calendar year (default July 1st)
static const bool GRASS_TRANSITION = true;  // default True, False for GFMC to always be standing
static const int MON_STANDING = 7;
static const int DAY_STANDING = 1;

////// Function Declarations and Help
/**
 * @brief Convert to fine fuel moisture content (%)
 *
 * @param ffmc      Fine Fuel Moisture Code (FFMC)
 * @return          fine fuel moisture content (%)
 */
double mcffmc_to_ffmc(double ffmc);

/**
 * @brief Convert to FFMC
 *
 * @param mcffmc    fine fuel moisture content (%)
 * @return          FFMC
 */
double ffmc_to_mcffmc(double mcffmc);

/**
 * @brief Convert to duff moisture content (%)
 * 
 * @param dmc       Duff Moisture Code (DMC)
 * @return          duff moisture content (%)
 */
double dmc_to_mcdmc(double dmc);

/**
 * @brief Convert to duff moisture content (%)
 * 
 * @param mcdmc     duff moisture content (%)
 * @return          DMC
 */
double mcdmc_to_dmc(double mcdmc);

/**
 * @brief Convert to to DC moisture content (%)
 * @param dc        Drought Code (DC)
 * @return          DC moisture content (%)
 */
double dc_to_mcdc(double dc);

/**
 * @brief Convert to DC
 * @param mcdc      DC moisture content (%)
 * @return          DC
 */
double mcdc_to_dc(double mcdc);

/**
 * Calculate hourly Fine Fuel Moisture (percent) value
 *
 * @param lastmc          Previous Fine Fuel Moisture (percent)
 * @param temp            Temperature (Celcius)
 * @param rh              Relative Humidity (percent, 0-100)
 * @param ws              Wind Speed (km/h)
 * @param rain            Rainfall AFTER intercept (mm)
 * @param time_increment  Duration of timestep (hr, mainly 1.0)
 * @return                Hourly fine fuel moisture content (%)
 */
double hourly_fine_fuel_moisture(double lastmc, double temp, double rh, double ws,
  double rain, double time_increment);

/**
 * @brief Calculate duff moisture content
 * 
 * @param last_mcdmc              Previous duff moisture content (%)
 * @param hr                      Time of day (hr)
 * @param temp                    Temperature (Celcius)
 * @param rh                      Relative Humidity (%)
 * @param prec                    Hourly precipitation (mm)
 * @param sunrise                 Sunrise (hr)
 * @param sunset                  Sunset (hr)
 * @param prec_cumulative_prev    Cumulative precipitation since start of rain (mm)
 * @param time_increment          Duration of timestep (hr, mainly 1.0)
 * @return                        Hourly duff moisture content (%)
 */
double duff_moisture_code(double last_mcdmc, int hour, double temp, double rh,
  double prec, double sunrise, double sunset,
  double prec_cumulative_prev, double time_increment);

/**
 * @brief Calculate drought code moisture content
 * @param last_mcdc               Previous drought code moisture content (%)
 * @param hr                      Time of day (hr)
 * @param temp                    Temperature (Celcius)
 * @param prec                    Hourly precipitation (mm)
 * @param sunrise                 Sunrise (hr)
 * @param sunset                  Sunset (hr)
 * @param prec_cumulative_prev    Cumulative precipitation since start of rain (mm)
 * @param time_increment          Duration of timestep (hr, mainly 1.0)
 * @return                        Hourly drought code moisture content (%)
 */
double drought_code(double last_mcdc, int hour, double temp, double prec,
  double sunrise, double sunset,
  double prec_cumulative_prev, double time_increment);

/**
 * Calculate Initial Spread Index (ISI)
 *
 * @param ws              Wind Speed (km/h)
 * @param ffmc            Fine Fuel Moisure Code
 * @return                Initial Spread Index
 */
double initial_spread_index(double ws, double ffmc);

/**
 * Calculate Build-up Index (BUI)
 *
 * @param dmc             Duff Moisture Code
 * @param dc              Drought Code
 * @return                Build-up Index
 */
double buildup_index(double dmc, double dc);

/**
 * Calculate Fire Weather Index (FWI)
 *
 * @param isi             Initial Spread Index
 * @param bui             Build-up Index
 * @return                Fire Weather Index
 */
double fire_weather_index(double isi, double bui);

double daily_severity_rating(double fwi);

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
double hourly_grass_fuel_moisture(double temp, double rh, double ws, double rain,
  double solrad, double lastmc, double load);

double Pign(double mc, double wind2m, double Cint, double Cmc, double Cws);

double curing_factor(double cur);

double grass_moisture_code(double mc, double cur, double wind);

double matted_grass_spread_ROS(double ws, double mc, double cur);

double standing_grass_spread_ROS(double ws, double mc, double cur);

/**
 * Calculate Grass Spread Index (GSI)
 *
 * @param ws              10 metre OPEN Wind Speed (km/h)
 * @param mc              Grass moisture content (fully cured grass) (percent)
 * @param cur             Degree of curing (percent, 0-100)
 * @return                Grass Spread Index
 */
double grass_spread_index(double ws, double mc, double cur, bool standing);

/**
 * Calculate Grass Fire Weather Index
 *
 * @param gsi               Grass Spread Index
 * @param load              Fuel Load (kg/m^2)
 * @return                  Grass Fire Weather Index
 */
double grass_fire_weather_index(double gsi, double load);

/*
 * Calculate number of drying "units" this hour contributes
 */
double drying_units(double temp, double rh, double wind, double rain, double solrad);

/* HACK: use struct so it's closer to how R can return multiple values */
void rain_since_intercept_reset(double temp, double rh, double ws, double rain,
  int mon, int hour, double solrad, double sunrise, double sunset,
  struct rain_intercept *canopy);
