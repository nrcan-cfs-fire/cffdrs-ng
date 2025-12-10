#include "util.h"
#include "NG_FWI.h"
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>



int main(int argc, char *argv[])
{
  if (argc < 4) {
    printf("\n########\nhelp/usage:\n"
      "%s input output timezone\n"
      "[ffmc_old] [mcffmc_old] [dmc_old] [dc_old] [mcgfmc_matted_old] [mcgfmc_standing_old]\n"
      "[prec_cumulative] [canopy_drying]\n\n", argv[0]);
    // Help
    printf("positional arguments:\n"
      "input                 Input csv data file\n"
      "output                Output csv file name and location\n"
      "timezone              UTC offset (required in C version)\n"
      "ffmc_old              Starting value for FFMC (default 85, \"n\" for mcffmc_old)\n"
      "mcffmc_old            Starting value for mcffmc (default \"n\" for ffmc_old input)\n"
      "dmc_old               Starting DMC (default 6)\n"
      "dc_old                Starting DC (default 15)\n"
      "mcgfmc_matted_old     Starting mcgfmc for matted fuels (default 16.3075)\n"
      "mcgfmc_standing_old   Starting mcgfmc for standing fuels (default 16.3075)\n"
      "prec_cumulative       Cumulative precipitation of rain event (default 0)\n"
      "canopy_drying         Canopy drying, or consecutive hours of no prec (default 0)\n"
      "########\n\n");
    exit(1);
  }
  
  /*  CSV headers */
  static const char *header_req = "lat,long,yr,mon,day,hr,"
    "temp,rh,ws,prec";  // optional grass_fuel_load, percent_cured, and/or solrad;
  static const char *header_out = "lat,long,timezone,yr,mon,day,hr,"
    "temp,rh,ws,prec,"
    "grass_fuel_load,percent_cured,solrad,sunrise,sunset,sunlight_hours,"
    "mcffmc,ffmc,dmc,dc,isi,bui,fwi,dsr,"
    "mcgfmc_matted,mcgfmc_standing,gfmc,gsi,gfwi,"
    "prec_cumulative,canopy_drying";
  
  double TZadjust, ffmc_old, mcffmc_old, dmc_old, dc_old;
  double mcgfmc_matted_old, mcgfmc_standing_old, prec_cumulative, canopy_drying;
  bool silent = false;

  // open input file
  FILE *inp = fopen(argv[1], "r");
  if (inp == NULL) {
    printf("\n\n***** FILE %s does not exist\n", argv[1]);
    exit(1);
  }
  if (!silent) {
    printf("\nOpening input file >>> %s\n", argv[1]);
  }

  // load required timezone argument
  TZadjust = atof(argv[3]);

  // load optional arguments if provided, or set to default
  if (argc > 4) {
    if (*argv[4] == 'n') {
      ffmc_old = -1;
    } else {
      ffmc_old = atof(argv[4]);
    }
  } else {
    ffmc_old = FFMC_DEFAULT;
  }
  if (argc > 5) {
    if (*argv[5] == 'n') {
      mcffmc_old = -1;
    } else {
      mcffmc_old = atof(argv[5]);
    }
  } else {
    mcffmc_old = -1;
  }
  if (argc > 6) {
    dmc_old = atof(argv[6]);
  } else {
    dmc_old = DMC_DEFAULT;
  }
  if (argc > 7) {
    dc_old = atof(argv[7]);
  } else {
    dc_old = DC_DEFAULT;
  }
  if (argc > 8) {
    mcgfmc_matted_old = atof(argv[8]);
  } else {
    mcgfmc_matted_old = ffmc_to_mcffmc(85);
  }
  if (argc > 9) {
    mcgfmc_standing_old = atof(argv[9]);
  } else {
    mcgfmc_standing_old = ffmc_to_mcffmc(85);
  }
  if (argc > 10) {
    prec_cumulative = atof(argv[10]);
  } else {
    prec_cumulative = 0.0;
  }
  if (argc > 11) {
    canopy_drying = atof(argv[11]);
  } else {
    canopy_drying = 0.0;
  }
  if (argc > 12) {
    puts("Warning: too many arguments provided, some unused\n");
  }

  // check for values outside valid range
  if (mcffmc_old == -1) {
    if (ffmc_old == -1) {
      puts("\n\n*****   Either ffmc_old OR mcffmc_old should be \"n\", not both\n");
      exit(1);
    } else if (ffmc_old < 0 || ffmc_old > 101) {
      puts("\n\n*****   ffmc_old must be between 0 and 101\n");
      exit(1);
    }
  } else {
    if (ffmc_old == -1) {
      if (mcffmc_old < 0 || mcffmc_old > 250) {
        puts("\n\n*****   mcffmc_old must be between 0 and 250\n");
        exit(1);
      }
    } else {
      puts("\n\n*****   One of ffmc_old OR mcffmc_old should be \"n\", not neither\n");
      exit(1);
    }
  }
  if (dmc_old < 0) {
    printf("\n\n*****   dmc_old must be >= 0\n");
    exit(1);
  }
  if (dc_old < 0) {
    printf("\n\n*****   dc_old must be >= 0\n");
    exit(1);
  }
  if (mcgfmc_matted_old < 0) {
    printf("\n\n*****   mcgfmc_matted_old must be >= 0\n");
    exit(1);
  }
  if (mcgfmc_standing_old < 0) {
    printf("\n\n*****   mcgfmc_standing_old must be >= 0\n");
    exit(1);
  }
  if (prec_cumulative < 0) {
    printf("\n\n*****   prec_cumulative must be >= 0\n");
    exit(1);
  }
  if (canopy_drying < 0) {
    printf("\n\n*****   canopy_drying must be >= 0\n");
    exit(1);
  }

  // initialize parameters
  double mcffmc, mcdmc, mcdc, mcgfmc_standing, mcgfmc_matted;

  if (ffmc_old == -1) {
    mcffmc = mcffmc_old;
  } else {
    mcffmc = ffmc_to_mcffmc(ffmc_old);
  }
  mcdmc = dmc_to_mcdmc(dmc_old);
  mcdc = dc_to_mcdc(dc_old);
  mcgfmc_standing = mcgfmc_standing_old;
  mcgfmc_matted = mcgfmc_matted_old;

  // initialize input header and data
  struct flags flag_holder = 
    {false, false, false}; // {grass_fuel_load, percent_cured, solrad};
  struct row cur, old;
  struct rain_intercept canopy = {0.0, prec_cumulative, canopy_drying};
  int err;
  bool standing;

  check_header_FWI(inp, header_req, &flag_holder);
  cur.timezone = TZadjust;  // assign timezone before possibly calculating solrad
  err = read_row_inputs(inp, &cur, &flag_holder,
    DEFAULT_GRASS_FUEL_LOAD, MON_CURING, DAY_CURING);
  old.day = -1;  // ensure initial sunrise and sunset calculation

  // use first year in data for transition btwn matted and standing
  // does not change for fire seasons continuous across multiple years
  struct tm DATE_GRASS_STANDING = {
    .tm_year = cur.year - 1900,
    .tm_mon = MON_STANDING - 1,
    .tm_mday = DAY_STANDING,
    .tm_isdst = 0};
  
  // open output file
  if (strcmp(argv[1], argv[2]) == 0) {
    puts("Input and Output files have to be different!");
    exit(1);
  }
  
  FILE *out = fopen(argv[2], "w");
  if (out == NULL) {
    printf("\n\n***** FILE %s can not be opened\n", argv[2]);
    exit(1);
  }
  if (!silent) {
    printf("Saving outputs to file >>> %s\n", argv[2]);
  }
  fprintf(out, "%s\n", header_out);

  if (!silent) {
    printf("\n########\nStartup values used:\n");
    printf("FFMC = %.1f or mcffmc = %.1f %%\n", ffmc_old, mcffmc_old);
    printf("DMC = %.1f and DC = %.1f\n", dmc_old, dc_old);
    printf("mcgfmc matted = %.4f %% and standing = %.4f %%\n",
      mcgfmc_matted_old, mcgfmc_standing_old);
    printf("cumulative precipitation = %.1f mm and canopy drying = %.0f\n",
      prec_cumulative, canopy_drying);
  }

  // start calculation
  double rain_ffmc, ffmc, dmc, dc, isi, bui, fwi, dsr;
  double mcgfmc, gfmc, gsi, gfwi, sunlight_hours;

  while (err > 0)  // while there is a next row of data in input file
  {
    /* Only need to calculate sunrise/sunset once per day */
    if (cur.day != old.day || cur.mon != old.mon)
    {
      double suntime[2];
      sunrise_sunset(cur.lat, cur.lon, cur.timezone, cur.timestamp, suntime);
      cur.sunrise = suntime[0];
      cur.sunset = suntime[1];
    }

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
    rain_ffmc = canopy.rain_total <= FFMC_INTERCEPT ? 0.0 :
      ((canopy.rain_total - FFMC_INTERCEPT) > cur.rain ? cur.rain :
      canopy.rain_total - FFMC_INTERCEPT);
    mcffmc = hourly_fine_fuel_moisture(
      mcffmc,
      cur.temp,
      cur.rh,
      cur.ws,
      rain_ffmc,
      1.0);
    ffmc = mcffmc_to_ffmc(mcffmc);
    mcdmc = duff_moisture_code(
      mcdmc,
      cur.hour,
      cur.temp,
      cur.rh,
      cur.rain,
      cur.sunrise,
      cur.sunset,
      canopy.rain_total_prev,
      1.0);
    dmc = mcdmc_to_dmc(mcdmc);
    mcdc = drought_code(
      mcdc,
      cur.hour,
      cur.temp,
      cur.rain,
      cur.sunrise,
      cur.sunset,
      canopy.rain_total_prev,
      1.0);
    dc = mcdc_to_dc(mcdc);
    isi = initial_spread_index(cur.ws, ffmc);
    bui = buildup_index(dmc, dc);
    fwi = fire_weather_index(isi, bui);
    dsr = daily_severity_rating(fwi);
    
    mcgfmc_matted = hourly_grass_fuel_moisture(
      cur.temp,
      cur.rh,
      cur.ws,
      cur.rain,
      cur.solrad,
      mcgfmc_matted,
      cur.grass_fuel_load);
    
    mcgfmc_standing = hourly_grass_fuel_moisture(
      cur.temp,
      cur.rh,
      cur.ws,
      cur.rain * 0.06,
      0,  // no solar radiation felt when standing
      mcgfmc_standing,
      cur.grass_fuel_load);

    if (GRASS_TRANSITION &&
      difftime(mktime(&cur.timestamp), mktime(&DATE_GRASS_STANDING)) < 0) {
      standing = false;
      mcgfmc = mcgfmc_matted;
    } else {
      standing = true;
      mcgfmc = mcgfmc_standing;
    }

    gfmc = grass_moisture_code(mcgfmc, cur.percent_cured, cur.ws);
    gsi = grass_spread_index(cur.ws, mcgfmc, cur.percent_cured, standing);
    gfwi = grass_fire_weather_index(gsi, cur.grass_fuel_load);
    
    sunlight_hours = cur.sunset - cur.sunrise;

    save_csv(out,
      "%.4f,%.4f,%.2f,%d,%d,%d,%d,"
      "%.1f,%.0f,%.1f,%.2f,"
      "%.2f,%.2f,"
      "%.4f,%.4f,%.4f,%.4f,"
      "%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,"
      "%.4f,%.4f,%.4f,%.4f,%.4f,"
      "%.4f,%d\n",
      cur.lat, cur.lon, cur.timezone, cur.year, cur.mon, cur.day, cur.hour,
      cur.temp, cur.rh, cur.ws, cur.rain,
      cur.grass_fuel_load, cur.percent_cured,
      cur.solrad, cur.sunrise, cur.sunset, sunlight_hours,
      mcffmc, ffmc, dmc, dc, isi, bui, fwi, dsr,
      mcgfmc_matted, mcgfmc_standing, gfmc, gsi, gfwi,
      canopy.rain_total, canopy.drying_since_intercept);
    
    old = cur;
    err = read_row_inputs(inp, &cur, &flag_holder,
      DEFAULT_GRASS_FUEL_LOAD, MON_CURING, DAY_CURING);
    // err > 0 means a next row of data in input file was detected

    // check for continuity and if sequential by hour
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
  } /* end the main while (err > 0)  */

  fclose(inp);
  fclose(out);

  if (!silent) {
    printf("########\n\n");
  }

  return 0;
}
