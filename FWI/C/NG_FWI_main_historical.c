
#include "util.h"
#include "NG_FWI.h"
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>






int main(int argc, char *argv[])
{
  /*  CSV headers */
  static const char *header = "lat,long,yr,mon,day,hr,temp,rh,ws,prec,solrad,percent_cured,grass_fuel_load";
  static const char *header_out = "lat,long,yr,mon,day,hr,temp,rh,ws,prec,solrad,sunrise,sunset,ffmc,dmc,dc,isi,bui,fwi,dsr,gfmc,gsi,gfwi,mcffmc,mcgfmc,percent_cured,grass_fuel_load,mcgfmc_matted,mcgfmc_standing,dmc_before_rain,dc_before_rain,prec_cumulative,conpy_drying";
  if (7 != argc)
  {
    printf("Command line:   %s <local GMToffset> <starting FFMC> <starting DMC> <starting DC> <input file> <output file>\n\n", argv[0]);
    printf("<local GMToffset> is the off of Greenich mean time (for Eastern = -5  Central=-6   MT=-7  PT=-8 )  \n");
    printf("All times should be local standard time\n");
    //printf("One of <starting FFMC> and <Starting mcFFMC> needs to be \"n\"\n");
    printf("INPUT FILE format must be HOURLY weather data, comma seperated and take the form\n");
    printf("%s\n\n", header);
    exit(1);
  }

  //double temp_range = read_temp_range(argv[12], header);
 



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
  double mcffmc_old = -1;

  if(((mcffmc_old == -1) && (ffmc_old == -1)) || ((mcffmc_old >= 0) && (ffmc_old >= 0))){
      printf(" /n/n *****   One and only one of <starting FFMC> and <Starting mcFFMC> can be specified the other must be NULL\n");
      exit(1);
  }


   
  /* printf("TZ=%d    start ffmc=%f  dmc=%f\n", TZadjust, ffmc_old, dmc_old); */
  double mcffmc;
  if(ffmc_old==-1){
    mcffmc = mcffmc_old;
  }
  else{
    mcffmc = fine_fuel_moisture_from_code(ffmc_old);
  }
  /* assuming this is fine because swiss sfms uses it now */
  double mcgfmc = mcffmc;
  double mcgfmc_standing = mcffmc;
  double mcgfmc_matted = mcffmc;
  /* check that the header matches what is expected */
  struct flags flag_holder = {false,false};
  check_header(inp, header, &flag_holder);
  struct row cur;
  int err = read_row_inputs(inp, &cur, &flag_holder);
  struct row old = {0};
  double dmc = dmc_old;
  double dmc_before_rain = dmc;
  double dc = dc_old;
  double dc_before_rain = dc;
  double sunrise;
  double sunset;
  struct rain_intercept canopy = {0.0, 0.0, 0.0};
  struct double_24hr solrad_24hr;
  FILE *out = fopen(argv[6], "w");
  fprintf(out, "%s\n", header_out);

  while (err > 0)
  {
    if (cur.day != old.day || cur.mon != old.mon)
    {
      /* Only need to calculate sunrise/sunset once per day */
      sunrise_sunset(cur.lat, cur.lon, cur.mon, cur.day, TZadjust, &sunrise, &sunset); 
      // check if solrad needs to be added
      //if (flag_holder.solrad_flag){
        //solar_radiation(cur.lat, cur.lon, cur.mon, cur.day, TZadjust, temp_range, &solrad_24hr);
      //}
    }
    if(flag_holder.solrad_flag){
      cur.solrad = single_hour_solrad_estimation(cur.lat, cur.lon, julian(cur.mon, cur.day), TZadjust, cur.hour, cur.rh, cur. temp); //solrad_24hr.hour[cur.hour];
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
    
    mcgfmc_matted = hourly_grass_fuel_moisture(cur.temp, cur.rh, cur.ws, cur.rain, cur.solrad, mcgfmc_matted);
    mcgfmc_standing = hourly_grass_fuel_moisture(cur.temp, cur.rh, cur.ws, cur.rain*0.06, 0, mcgfmc_standing);
    mcgfmc = mcgfmc_standing;
    bool standing = true;
    if(julian(cur.mon, cur.day) < DATE_GRASS){
      standing = false;
      mcgfmc = mcgfmc_matted;
    }

    double gfmc = grass_moisture_code(mcgfmc, cur.percent_cured, cur.ws);
    double gsi = grass_spread_index(cur.ws, mcgfmc, cur.percent_cured, standing);
    double gfwi = grass_fire_weather_index(gsi, cur.grass_fuel_load);
    /* printf("\n"); */
    save_csv(out,
             "%.4f,%.4f,%4d,%02d,%02d,%02d,%.1f,%.0f,%.1f,%.2f,%.4f,%.4f,%.4f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.1f,%.4f,%.4f,%.1f,%.2f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f\n",
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
             cur.sunrise,
             cur.sunset,
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
             cur.grass_fuel_load,
             mcgfmc_matted,
             mcgfmc_standing,
             dmc_before_rain,
             dc_before_rain,
             canopy.rain_total,
             canopy.drying_since_intercept);
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
    err = read_row_inputs(inp, &cur, &flag_holder);
    printf("%d\n",err);
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