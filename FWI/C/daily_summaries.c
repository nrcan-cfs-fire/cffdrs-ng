#include "util.h"
#include "NG_FWI.h"
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <time.h>

struct pseudo_date{
  int year;
  int yday;
};

struct daily_summary{
  double ffmc, dmc, dc, isi, bui, fwi, dsr;
  double gfmc, gsi, gfwi;
  double ws_smooth, isi_smooth, gsi_smooth;
  int duration;
  double sunrise, sunset;
  int peak_hour, year, month, day;
};

struct hour_vals {
  // read in from input
  double lat, lon, timezone;
  int year, month, day, hour;
  double ws, percent_cured, sunrise, sunset;
  double ffmc, dmc, dc, isi, bui, fwi, dsr;
  double mcgfmc_matted, mcgfmc_standing, gfmc, gsi, gfwi;
  // variables to calculate
  double smooth_ws, smooth_isi;
};

struct day_vals {
  struct pseudo_date* p_day;
  struct hour_vals* hour[24];
  int hour_slots_filled;
};

void smooth_5pt(double *source, int source_len, double *dest){
  //binomial smoother  ... specifically for the 24 hour day
  //1pt = 1
  //3pt = (1 2 1) = 4
  //5pt = (1 4 6 4 1) = 16
  //7pt = (1 6 15 20 15 6 1) = 64

  //dest = (double*)malloc(sizeof(double)*source_len);
  dest[0] = source[0];
  dest[source_len-1] = source[source_len-1];


  int miss = 0;
  int i;
  for(i = 0; i <= 2; i++){
    if(source[i] < -90.0){
      miss = miss + 1;
    }
  }
  if(miss == 0){
    dest[1] = (0.25*(source[0])) + (0.5*(source[1])) +(0.25*(source[2]));
  }
  else{
    dest[1] = source[1];
  }
    
  int j;
  for(i = 2; i <= (source_len-3); i++){
    miss = 0;
    for(j = (i-2); j <= (i+2); j++){
      if(source[j] < -90.0){
        miss = miss + 1;
      }
    }
    if(miss == 0){
      dest[i] = ((1.0/16.0)*(source[i-2])) + ((4.0/16.0)*(source[i-1])) +
        ((6.0/16.0)*(source[i])) + ((4.0/16.0)*(source[i+1])) +
        ((1.0/16.0)*(source[i+2]));
    }
    else{
      dest[i] = source[i];  
    }
  }

  miss = 0;
  for(i = source_len - 3; i < source_len; i++){
    if((source[i]) < -90.0){
      miss = miss + 1;
    }
  }
  if(miss == 0){
    dest[source_len - 2] = (0.25*(source[source_len-3])) +
        (0.5*(source[source_len-2]))+ (0.25*(source[source_len-1]));
  }
  else{
    dest[source_len - 2] = source[source_len - 2];
  }
}

/**
 * Calculate a pseudo-date that changes not at midnight but forward at another hour
 * 
 * @param    yr        year
 * @param    mon       month number
 * @param    day       day of month
 * @param    hr        hour of day
 * @param    reset_hr  the new boundary hour instead of midnight (default 5)
 * @param    pd        pseudo_date structure pointer
 */
void pseudo_date(int yr, int mon, int day, int hr,
  int reset_hr, struct pseudo_date *pd)
{
  int adjusted_jd;
  int adjusted_year;

  // using julian function due to weird behaviour at 23h with struct tm and mktime()
  if (hr < reset_hr) {
    adjusted_jd = julian(yr, mon, day) - 1;
  } else {
    adjusted_jd = julian(yr, mon, day);
  }

  if (adjusted_jd == 0) {  // where Jan 1 shifts to 0, bump it to end of previous year
    adjusted_jd = julian(yr - 1, 12, 31);
    adjusted_year = yr - 1;
  } else {
    adjusted_year = yr;
  }

  // fill in struct pseudo_date pd;
  pd->yday = adjusted_jd;
  pd->year = adjusted_year;
}

/**
 * Loads and compares input file header to string
 * 
 * @param    inp        input file pointer
 * @param    header     string to compare to
 */
void check_header_daily_summaries(FILE *inp, const char *header){
  char intake[1];
  int err = fscanf(inp, "%c", &intake[0]);
  int pos = 0;

  bool matching = true;
  while((intake[0] != '\n') && matching){
    if(err == 0){
      printf("Error reading file\n");
      exit(1); 
    }

    if (header[pos] != intake[0]){
      matching = false;
    }

    pos += 1;
    err = fscanf(inp, "%c", &intake[0]);
  }

  if((pos != 223) || !matching){
    printf("Header does not match\n");
    exit(1); 
  }
}

/**
 * Load hourly data until pseudo-date changes
 * 
 * @param   inp         input file pointer
 * @param   by_date     day_vals structure to load hourly data into
 * @param   buffer      hour_vals structure to store next hour's data in
 * @param   reset_hour  new boundary to define pseudo-date summarize (default 5)
 * @return              number of arguments read by fscanf()
 */
int read_row_daily_summaries(FILE *inp, struct day_vals *by_date,
  struct hour_vals *buffer, int reset_hour)
{
  bool day_change_flag = false;
  int err = 0, round = 0, h;
  struct pseudo_date pd;

  while (!day_change_flag) {
    round++;
    if (buffer->year == -1) {  // if buffer was already written to by_date
      char waste_c[1];
      int waste_i, year, month, day, hour;
      double waste_f, ws, percent_cured, sunrise, sunset;
      double ffmc, dmc, dc, isi, bui, fwi, dsr;
      double mcgfmc_matted, mcgfmc_standing, gfmc, gsi, gfwi;

      err = fscanf(inp,
        "%lf%c%lf%c%lf%c"  // lat, long, timezone
        "%d%c%d%c%d%c%d%c"  // yr, mon, day, hr
        "%lf%c%lf%c%lf%c%lf%c"  // temp, rh, ws, prec
        "%lf%c%lf%c%lf%c"  // grass_fuel_load, percent_cured, solrad
        "%lf%c%lf%c%lf%c"  // sunrise, sunset, sunlight_hours
        "%lf%c%lf%c%lf%c%lf%c"  // mcffmc, ffmc, dmc, dc
        "%lf%c%lf%c%lf%c%lf%c"  // isi, bui, fwi, dsr
        "%lf%c%lf%c"  // mcgfmc_matted, mcgfmc_standing
        "%lf%c%lf%c%lf%c"  // gfmc, gsi, gfwi
        "%lf%c%d",  // prec_cumulative, canopy_drying
        &waste_f, waste_c, &waste_f, waste_c, &waste_f, waste_c,
        &year, waste_c, &month, waste_c, &day, waste_c, &hour, waste_c,
        &waste_f, waste_c, &waste_f, waste_c, &ws, waste_c, &waste_f, waste_c,
        &waste_f, waste_c, &percent_cured, waste_c, &waste_f, waste_c,
        &sunrise, waste_c, &sunset, waste_c, &waste_f, waste_c,
        &waste_f, waste_c, &ffmc, waste_c, &dmc, waste_c, &dc, waste_c,
        &isi, waste_c, &bui, waste_c, &fwi, waste_c, &dsr, waste_c,
        &mcgfmc_matted, waste_c, &mcgfmc_standing, waste_c,
        &gfmc, waste_c, &gsi, waste_c, &gfwi, waste_c,
        &waste_f, waste_c, &waste_i);

      buffer->year = year;
      buffer->month = month;
      buffer->day = day;
      buffer->hour = hour;
      buffer->ws = ws;
      buffer->percent_cured = percent_cured;
      buffer->sunrise = sunrise;
      buffer->sunset = sunset;
      buffer->ffmc = ffmc;
      buffer->dmc = dmc;
      buffer->dc = dc;
      buffer->isi = isi;
      buffer->bui = bui;
      buffer->fwi = fwi;
      buffer->dsr = dsr;
      buffer->mcgfmc_matted = mcgfmc_matted;
      buffer->mcgfmc_standing = mcgfmc_standing;
      buffer->gfmc = gfmc;
      buffer->gsi = gsi;
      buffer->gfwi = gfwi;
    }

    if (err == EOF) {  // reached end of input file
      return err;
    }

    pseudo_date(buffer->year, buffer->month, buffer->day, buffer->hour,
      reset_hour, &pd);
    
    if (by_date->hour_slots_filled == 0) {  // new pseudo-date
      // set pseudo-date of by_date
      by_date->p_day->yday = pd.yday;
      by_date->p_day->year = pd.year;
    }

    if ((pd.yday == by_date->p_day->yday) &&
      (pd.year == by_date->p_day->year)) {
      // copy buffer to corresponding hour in by_date
      h = by_date->hour_slots_filled;

      by_date->hour[h]->year = buffer->year;
      by_date->hour[h]->month = buffer->month;
      by_date->hour[h]->day = buffer->day;
      by_date->hour[h]->hour = buffer->hour;
      by_date->hour[h]->ws = buffer->ws;
      by_date->hour[h]->percent_cured = buffer->percent_cured;
      by_date->hour[h]->sunrise = buffer->sunrise;
      by_date->hour[h]->sunset = buffer->sunset;
      by_date->hour[h]->ffmc = buffer->ffmc;
      by_date->hour[h]->dmc = buffer->dmc;
      by_date->hour[h]->dc = buffer->dc;
      by_date->hour[h]->isi = buffer->isi;
      by_date->hour[h]->bui = buffer->bui;
      by_date->hour[h]->fwi = buffer->fwi;
      by_date->hour[h]->dsr = buffer->dsr;
      by_date->hour[h]->mcgfmc_matted = buffer->mcgfmc_matted;
      by_date->hour[h]->mcgfmc_standing = buffer->mcgfmc_standing;
      by_date->hour[h]->gfmc = buffer->gfmc;
      by_date->hour[h]->gfwi = buffer->gfwi;
      by_date->hour[h]->gsi = buffer->gsi;

      by_date->hour_slots_filled++;
      buffer->year = -1;  // set to read in next row
    } else {  // new pseudo-date, exit function but continue to hold buffer
      day_change_flag = true;
    }
  }
  return err;
}

/**
 * Calculate Daily Summaries given hourly FWI indices from a pseudo-date
 * 
 * @param    day        day_vals structure for a pseudo-date
 * @return              daily_summary structure of peak FWI conditions
 */
struct daily_summary generate_daily_summary(struct day_vals day){
  const double spread_threshold_isi = 5.0;
  struct daily_summary summary;

  int i;

  //calculate smoothed ws 
  double *ws_pt = (double*)malloc(sizeof(double)*day.hour_slots_filled);
  for(i = 0; i< day.hour_slots_filled; i++){
    ws_pt[i] = day.hour[i]->ws;
  }
  double *ws_smooth_pt= (double*)malloc(sizeof(double)*day.hour_slots_filled);
  smooth_5pt(ws_pt, day.hour_slots_filled, ws_smooth_pt);

  //calculate smoothed isi and find peak info
  int ffmc_max_spot = 0;
  int peak_time_spot = 0;
  int duration = 0;
    
  for(i = 0; i < day.hour_slots_filled; i++){
    day.hour[i]->smooth_ws = ws_smooth_pt[i];
    day.hour[i]->smooth_isi = initial_spread_index(
      day.hour[i]->smooth_ws, day.hour[i]->ffmc);

    if(day.hour[i]->smooth_isi > spread_threshold_isi){
      duration+=1;
    }
    if(day.hour[i]->ffmc > day.hour[ffmc_max_spot]->ffmc){
      ffmc_max_spot = i;
    }
    if(day.hour[i]->smooth_isi > day.hour[peak_time_spot]->smooth_isi){
      peak_time_spot = i;
    }
  }

  if(day.hour[ffmc_max_spot]->ffmc < 85.0){
    peak_time_spot =  12;
  }

  // find the rest of the values at peak
  summary.year = day.hour[0]->year;
  summary.month = day.hour[0]->month;
  summary.day = day.hour[0]->day;
  summary.sunrise = day.hour[peak_time_spot]->sunrise;
  summary.sunset = day.hour[peak_time_spot]->sunset;

  summary.peak_hour = day.hour[peak_time_spot]->hour;
  summary.duration = duration;

  summary.ffmc = day.hour[peak_time_spot]->ffmc;
  summary.dmc = day.hour[peak_time_spot]->dmc;
  summary.dc = day.hour[peak_time_spot]->dc;
  summary.isi = day.hour[peak_time_spot]->isi;
  summary.bui = day.hour[peak_time_spot]->bui;
  summary.fwi = day.hour[peak_time_spot]->fwi;
  summary.dsr = day.hour[peak_time_spot]->dsr;
  summary.gfmc = day.hour[peak_time_spot]->gfmc;
  summary.gsi = day.hour[peak_time_spot]->gsi;
  summary.gfwi = day.hour[peak_time_spot]->gfwi;
  summary.ws_smooth = day.hour[peak_time_spot]->smooth_ws;
  summary.isi_smooth = day.hour[peak_time_spot]->smooth_isi;

  // check for matted to standing transition, and calculate gsi_smooth
  bool standing;
  double mcgfmc;
  struct tm ts = {
    .tm_year = day.hour[peak_time_spot]->year - 1900,  // years since 1900
    .tm_mon = day.hour[peak_time_spot]->month - 1,  // 0-indexed month
    .tm_mday = day.hour[peak_time_spot]->day,
    .tm_hour = 0,
    .tm_isdst = 0
  };
  struct tm DATE_GRASS_STANDING = {
    .tm_year = day.hour[peak_time_spot]->year - 1900,
    .tm_mon = MON_STANDING - 1,
    .tm_mday = DAY_STANDING,
    .tm_isdst = 0};

  if (GRASS_TRANSITION &&
    difftime(mktime(&ts), mktime(&DATE_GRASS_STANDING)) < 0) {
    standing = false;
    mcgfmc = day.hour[peak_time_spot]->mcgfmc_matted;
  } else {
    standing = true;
    mcgfmc = day.hour[peak_time_spot]->mcgfmc_standing;
  }
  summary.gsi_smooth = grass_spread_index(
    summary.ws_smooth, mcgfmc, day.hour[peak_time_spot]->percent_cured, standing);

  free(ws_pt);
  free(ws_smooth_pt);
  return summary;
}

int main(int argc, char *argv[]) {
  if (argc < 3) {
    printf("\n########\nhelp/usage:\n"
      "%s input output [reset_hr]\n\n", argv[0]);
    puts("positional arguments:\n"
      "input                 Input csv data file\n"
      "output                Output csv file name and location\n"
      "reset_hr              New boundary to define day to summarize (default 5)\n"
      "########\n\n");
    exit(1);
  }
    
  // headers for csvs
  static const char *header_in = "lat,long,timezone,yr,mon,day,hr,"
    "temp,rh,ws,prec,grass_fuel_load,percent_cured,"
    "solrad,sunrise,sunset,sunlight_hours,"
    "mcffmc,ffmc,dmc,dc,isi,bui,fwi,dsr,"
    "mcgfmc_matted,mcgfmc_standing,gfmc,gsi,gfwi,"
    "prec_cumulative,canopy_drying";
  static const char *header_out = "yr,mon,day,"
    "sunrise,sunset,"
    "peak_hr,duration,"
    "ffmc,dmc,dc,isi,bui,fwi,dsr,"
    "gfmc,gsi,gfwi,"
    "ws_smooth,isi_smooth,gsi_smooth";

  int reset_hr;
    
  // open input file
  FILE *inp = fopen(argv[1], "r");
  printf("Opening input file >>> %s   \n", argv[1]);
  if (inp == NULL) {
    printf("\n\n ***** FILE  %s  does not exist\n", argv[1]);
    exit(1);
  }

  // load optional argument if provided, or set to default
  if (argc > 3) {
    reset_hr = atof(argv[3]);
  } else {
    reset_hr = 5;
  }

  if (argc > 4) {
    puts("Warning: too many arguments provided, some unused\n");
  }

  // check input header
  check_header_daily_summaries(inp, header_in);
    
  // open output file
  FILE *out = fopen(argv[2], "w");
  if (out == NULL) {
    printf("\n\n***** FILE %s can not be opened\n", argv[2]);
    exit(1);
  }
  printf("Saving outputs to file >>> %s\n", argv[2]);
  fprintf(out, "%s\n", header_out);

  // start calculation
  struct hour_vals *buffer;
  buffer = (struct hour_vals*)malloc(sizeof(struct hour_vals));
  buffer->year = -1;
  int err = 1;
   
  while (err > 0) {  // while there is a next row of data in input file
    // allocate storage in by_date for all data for a pseudo-date
    struct day_vals *by_date = (struct day_vals*)malloc(sizeof(struct day_vals));
    by_date->p_day = (struct pseudo_date*)malloc(sizeof(struct pseudo_date));
    for(int i = 0; i< 24; i++){
      by_date->hour[i] = (struct hour_vals*)malloc(sizeof(struct hour_vals));
    }
    by_date->hour_slots_filled = 0;

    err = read_row_daily_summaries(inp, by_date, buffer, reset_hr);
    
    // run summary function if there are at least 12h in pseudo-date
    if (by_date->hour_slots_filled > 12) {
      struct daily_summary summary = generate_daily_summary(*by_date);
      fprintf(out,
        "%d,%d,%d,"
        "%.4f,%.4f,"
        "%d,%d,"
        "%.4f,%.4f,%.4f,"
        "%.4f,%.4f,%.4f,%.4f,"
        "%.4f,%.4f,%.4f,"
        "%.4f,%.4f,%.4f\n",
        summary.year, summary.month, summary.day,
        summary.sunrise, summary.sunset,
        summary.peak_hour, summary.duration,
        summary.ffmc, summary.dmc, summary.dc,
        summary.isi, summary.bui, summary.fwi, summary.dsr,
        summary.gfmc, summary.gsi, summary.gfwi,
        summary.ws_smooth, summary.isi_smooth, summary.gsi_smooth);
    }

    free(by_date->p_day);
    for(int i = 0; i< 24; i++){
      free(by_date->hour);
    }
    free(by_date);
    by_date=NULL;
  }

  free(buffer);
  fclose(inp);
  fclose(out);

  return 0;
}
