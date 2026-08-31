/*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/.
*/


/*
Utility functions for FWI2025.
*/


/*** Import packages *********************************************************/

#include "util.h"
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#define NDEBUG


/*** Functions ***************************************************************/

char* version()
{
    // update this and CHANGELOG.md before merging to GitHub main branch
    char *version = "2026-08-13 + DEV";
    return version;
}

void check_header_FWI(FILE *input, const char *header_req,
                      struct need_optionals *f)
{
    // Template combining required and optional columns.
    char header_full[200];
    strcpy(header_full, header_req);
    strcat(header_full, ",grass_fuel_load,percent_cured,solrad");
    // Limit input CSV header to an arbitrary 200 characters.
    char in_buffer[200];
    // Read first line of file.
    fscanf(input, "%200s", in_buffer);
    int in_buffer_len = strlen(in_buffer);
    if (in_buffer_len == 200) {
        puts("Input CSV header has 200 or more characters. "
             "Remove extra columns or increase limit");
        exit(1);
    }
    if (strncmp(header_req, in_buffer, strlen(header_req)) != 0) {
        puts("Error: Missing required columns in CSV file or it was not "
             "provided in this order:");
        printf("%s\n", header_req);
        exit(1);
    }
    if (in_buffer_len > strlen(header_full)) {
        puts("Error: Input CSV file header too long. Maximum possible is:");
        printf("%s\n", header_full);
        exit(1);
    }
    // Create all combinations of optional columns:
    // grass_fuel_load, percent_cured, solrad
    char header_no_s[200];
    strcpy(header_no_s, header_req);
    strcat(header_no_s, ",grass_fuel_load,percent_cured");
    char header_no_p[200];
    strcpy(header_no_p, header_req);
    strcat(header_no_p, ",grass_fuel_load,solrad");
    char header_no_g[200];
    strcpy(header_no_g, header_req);
    strcat(header_no_g, ",percent_cured,solrad");
    char header_no_ps[200];
    strcpy(header_no_ps, header_req);
    strcat(header_no_ps, ",grass_fuel_load");
    char header_no_gs[200];
    strcpy(header_no_gs, header_req);
    strcat(header_no_gs, ",percent_cured");
    char header_no_gp[200];
    strcpy(header_no_gp, header_req);
    strcat(header_no_gp, ",solrad");
    // Check if in_buffer matches any combination of optional headers.
    if (strcmp(header_full, in_buffer) == 0) {
        // All optional columns provided. Leave all flags to default false.
    }
    else if (strcmp(header_no_s, in_buffer) == 0) {
        f->need_solrad = true;
    }
    else if (strcmp(header_no_p, in_buffer) == 0) {
        f->need_percent_cured = true;
    }
    else if (strcmp(header_no_g, in_buffer) == 0) {
        f->need_grass_fuel_load = true;
    }
    else if (strcmp(header_no_ps, in_buffer) == 0) {
        f->need_percent_cured = true;
        f->need_solrad = true;
    }
    else if (strcmp(header_no_gs, in_buffer) == 0) {
        f->need_grass_fuel_load = true;
        f->need_solrad = true;
    }
    else if (strcmp(header_no_gp, in_buffer) == 0) {
        f->need_grass_fuel_load = true;
        f->need_percent_cured = true;
    }
    else if (strcmp(header_req, in_buffer) == 0) {
        f->need_grass_fuel_load = true;
        f->need_percent_cured = true;
        f->need_solrad = true;
    }
    else {
        puts("Error: Provided optional columns still need to be ordered "
             "'grass_fuel_load' before 'percent_cured' before 'solrad'");
        exit(1);
    }
}

void check_header_match(FILE *input, const char *header)
{
    // Check that the first line in input matches header.
    char a[1];
    const int n = strlen(header);
    int i;
    // Doing this one character at a time to avoid predefining character array
    // limit if we fscanf() with %s.
    for (i = 0; i < n + 1; i++) {
        int err = fscanf(input, "%c", a);
        if (0 == err) {
        puts("Error reading file");
        exit(1);
        }
        // Need a newline at end or else it's not really a match.
        if ((i == n && '\n' != a[0]) || (i < n && a[0] != header[i])) {
        printf("Expected columns in input CSV file to be:\n'%s'\n", header);
        exit(1);
        }
    }
}

void check_weather(double temp, double rh, double wind, double rain)
{
    // Do basic weather variable checks explicitly.
    if (rh < 0 || rh > 100) {
        printf("RH must be between 0 and 100, but got %f\n", rh);
        exit(1);
    }
    if (wind < 0) {
        printf("Wind must be positive, but got %f\n", wind);
        exit(1);
    }
    if (rain < 0) {
        printf("Precipitation must be positive, but got %f\n", rain);
        exit(1);
    }
}

void check_inputs(double temp, double rh, double wind, double rain,
                  double grass_fuel_load, double percent_cured, double solrad)
{
    check_weather(temp, rh, wind, rain);
    // Do basic variable checks explicitly.
    if (solrad < 0) {
        printf("Solar radiation must be positive, but got %f\n", solrad);
        exit(1);
    }
    if (percent_cured < 0 || percent_cured > 100) {
        printf("Percent cured must be between 0 and 100, but got %f\n",
               percent_cured);
        exit(1);
    }
    if (grass_fuel_load < 0) {
        printf("Grassland fuel load must be positive, but got %f\n",
               grass_fuel_load);
        exit(1);
    }
}

int read_row_inputs(FILE *inp, struct wx_hr *r, struct need_optionals *f,
                    float def_grass_fuel_load, int def_mon_curing,
                    int def_day_curing)
{
    // Limit input data rows to an arbitrary 500 characters.
    char line[500];
    int err;
    // Read in the next line from the input CSV.
    err = fscanf(inp, "%500s", line);
    if (strlen(line) == 500) {
        puts("Input data row has 500 or more characters, remove columns or "
             "increase limit");
        exit(1);
    }
    // Check if the end of the CSV file has been reached.
    if (err == EOF) {
        return err;
    }
    // Parse the input line until the next comma. This section uses and
    // requires the fact that required inputs are ordered and complete.
    r->lat = atof(strtok(line, ","));
    r->lon = atof(strtok(NULL, ","));
    r->year = atoi(strtok(NULL, ","));
    r->mon = atoi(strtok(NULL, ","));
    r->day = atoi(strtok(NULL, ","));
    r->hour = atoi(strtok(NULL, ","));
    r->temp = atof(strtok(NULL, ","));
    r->rh = atof(strtok(NULL, ","));
    r->ws = atof(strtok(NULL, ","));
    r->rain = atof(strtok(NULL, ","));
    // Make the timestamp.
    struct tm ts = {
        .tm_year = r->year - 1900,  // Years since 1900.
        .tm_mon = r->mon - 1,  // 0-indexed month (i.e. Jan = 0).
        .tm_mday = r->day,
        .tm_hour = r->hour,
        .tm_isdst = 0
    };
    mktime(&ts);
    r->timestamp = ts;
    // Parse the line for optional inputs relying on flags from
    // check_header_FWI() to read in value or calculate default instead.
    if (f->need_grass_fuel_load) {
        r->grass_fuel_load = def_grass_fuel_load;
    }
    else {
        r->grass_fuel_load = atof(strtok(NULL, ","));
    }
    if (f->need_percent_cured) {
        r->percent_cured = seasonal_curing(r->year, r->mon, r->day,
                                           def_mon_curing, def_day_curing);
    }
    else {
        r->percent_cured = atof(strtok(NULL, ","));
    }
    if (f->need_solrad) {
        r->solrad = solar_radiation(r);
    }
    else {
        r->solrad = atof(strtok(NULL, ","));
    }
    check_inputs(r->temp, r->rh, r->ws, r->rain, r->grass_fuel_load,
                 r->percent_cured, r->solrad);
    return err;
}

int read_row_daily(FILE *inp, struct wx_day *r)
{
    char a[1];
    int err = fscanf(inp,
                     "%lf%c%lf%c"
                     "%d%c%d%c%d%c"
                     "%lf%c%lf%c%lf%c%lf",
                     &r->lat, a, &r->lon, a,
                     &r->year, a, &r->mon, a, &r->day, a,
                     &r->temp, a, &r->rh, a, &r->ws, a, &r->rain);
    if (err > 0) {
        check_weather(r->temp, r->rh, r->ws, r->rain);
    }
    return err;
}

int read_row_minmax(FILE *inp, struct wx_minmax *r)
{
    char a[1];
    int err = fscanf(inp,
                     "%lf%c%lf%c"
                     "%d%c%d%c%d%c"
                     "%lf%c%lf%c"
                     "%lf%c%lf%c"
                     "%lf%c%lf%c%lf",
                     &r->lat, a, &r->lon, a,
                     &r->year, a, &r->mon, a, &r->day, a,
                     &r->temp_min, a, &r->temp_max, a,
                     &r->rh_min, a, &r->rh_max, a,
                     &r->ws_min, a, &r->ws_max, a, &r->rain);
    if (err > 0) {
        check_weather(r->temp_min, r->rh_min, r->ws_min, r->rain);
        check_weather(r->temp_max, r->rh_max, r->ws_max, r->rain);
    }
    // Make the timestamp.
    struct tm date = {
        .tm_year = r->year - 1900,  // Years since 1900.
        .tm_mon = r->mon - 1,  // 0-indexed month (i.e. Jan = 0).
        .tm_mday = r->day,
        .tm_isdst = 0
    };
    mktime(&date);
    r->date = date;
    return err;
}

double _max(double x, double y)
{
    return (x > y ? x : y);
}

double _min(double x, double y)
{
    return (x < y ? x : y);
}

bool is_leap(int yr) {
  return (yr % 4 == 0 && yr % 100 != 0 || yr % 400 == 0);
}

int julian(int yr, int mon, int day)
{
    // Sum up days for Jan and Feb.
    if (mon == 1) {
        return day;
    }
    else if (mon == 2) {
        return day + 31;
    }
    // Mar-Dec depends on leap year, based on Zeller algorithm.
    else {
        int i;
        if (is_leap(yr)) {
            i = 2;
        }
        else {
            i = 3;
        }
        return (30*(mon-1) + floor(0.6*(mon+1)) - i + day);
    }
}

double solar_radiation(struct wx_hr *wx)
{
    // t_yr is the day of the year as a fraction, in radians. Accounts for leap
    // years. tm_yday is already 0-indexed (i.e. Jan 1st = 0).
    double t_yr = (is_leap(wx->timestamp.tm_year + 1900) ?
                   2.0 * M_PI * wx->timestamp.tm_yday / 366.0 :
                   2.0 * M_PI * wx->timestamp.tm_yday / 365.0);
    // eot is the equation of time correction, in minutes.
    double eot = 229.18 * (
        7.5e-5 + 1.868e-3*cos(t_yr) - 3.2077e-2*sin(t_yr)
        - 1.4615e-2*cos(2.0*t_yr) - 4.0849e-2*sin(2.0*t_yr)
    );
    // decl is the solar declination angle, in radians.
    double decl = (6.918e-3 - 0.399912*cos(t_yr) + 7.0257e-2*sin(t_yr)
                   - 6.758e-3*cos(2.0*t_yr) + 9.07e-4*sin(2.0*t_yr)
                   - 2.697e-3*cos(3.0*t_yr) + 1.48e-3*sin(3.0*t_yr));
    // ha is the solar hour angle, in radians.
    double ha = M_PI/180.0*(
        15*((double)wx->hour-wx->timezone) + wx->lon + eot/4
    ) - M_PI;
    // cos_z is the cosine of the solar zenith angle.
    double cos_z = (sin(M_PI*wx->lat/180.0)*sin(decl)
                    + cos(M_PI*wx->lat/180.0)*cos(decl)*cos(ha));
    // vpd is vapour pressure deficit.
    double vpd = (6.11 * (1.0-wx->rh/100.0)
                  * exp(17.29*wx->temp/(wx->temp+237.3)));
    // Coefficients for cos_z and vpd from a regression analysis using data
    // from the 2007 season at the Petawawa Research Forest.
    double solrad = 0.92 * cos_z * (1.0-exp(-0.22*vpd));
    // Set negative and really small solar radiation values to 0.
    if (solrad < 1e-4) {
        solrad = 0.0;
    }
    return solrad;
}

void sun_times(double lat, double lon, double timezone, struct tm timestamp,
               double *suntime)
{
    // t_yr is the day of the year as a fraction, in radians. Accounts for leap
    // years. tm_yday is already 0-indexed (i.e. Jan 1st = 0).
    double t_yr = (is_leap(timestamp.tm_year + 1900) ?
                   2.0 * M_PI * timestamp.tm_yday / 366.0 :
                   2.0 * M_PI * timestamp.tm_yday / 365.0);
    // eot is the equation of time correction, in minutes.
    double eot = 229.18 * (
        7.5e-5 + 1.868e-3*cos(t_yr) - 3.2077e-2*sin(t_yr)
        - 1.4615e-2*cos(2.0*t_yr) - 4.0849e-2*sin(2.0*t_yr)
    );
    // decl is the solar declination angle, in radians.
    double decl = (6.918e-3 - 0.399912*cos(t_yr) + 7.0257e-2*sin(t_yr)
                   - 6.758e-3*cos(2.0*t_yr) + 9.07e-4*sin(2.0*t_yr)
                   - 2.697e-3*cos(3.0*t_yr) + 1.48e-3*sin(3.0*t_yr));
    // z_max is the solar zenith angle at the start and end of the drying day,
    // in radians (90.833° for standard sunrise and sunset).
    double z_max = 90.833 * M_PI / 180.0;
    // cos_ha_z is the cosine of the solar hour angle at z_max.
    double cos_ha_z = (cos(z_max)/(cos(M_PI*lat/180.0)*cos(decl))
                       - tan(M_PI*lat/180.0)*tan(decl));
    // Keep cos_ha_z between -1 and 1.
    cos_ha_z = _max(-1.0, _min(1.0, cos_ha_z));
    // ha_z is the solar hour angle at z_max, in DD.
    double ha_z = 180.0 * acos(cos_ha_z) / M_PI;
    suntime[0] = (720.0-4.0*(lon+ha_z)-eot)/60.0 + timezone;
    suntime[1] = (720.0-4.0*(lon-ha_z)-eot)/60.0 + timezone;
}

double grassland_curing(int yr, int mon, int day, int start_mon, int start_day)
{
    // Default grassland curing values for every 10 days in a growing season.
    // Start and end with identical "winter" cured value.
    static double PERCENT_CURED[] = {96.0, 95.0, 93.0, 92.0, 90.5, 88.4, 84.4,
                                     78.1, 68.7, 50.3, 32.9, 23.0, 22.0, 21.0,
                                     20.0, 25.7, 35.0, 43.0, 49.8, 60.0, 68.0,
                                     72.0, 75.0, 78.9, 86.0, 96.0};
    // Find previous green-up start date (either current or prior year).
    struct tm date = {.tm_year = yr - 1900,
                      .tm_mon = mon - 1,
                      .tm_mday = day,
                      .tm_isdst = 0};
    struct tm greenup = {.tm_year = yr - 1900,
                         .tm_mon = start_mon - 1,
                         .tm_mday = start_day,
                         .tm_isdst = 0};
    // difftime() outputs difference in seconds, so divide by 86400s/day.
    int delta_t = difftime(mktime(&date), mktime(&greenup)) / 86400;
    if (delta_t < 0) {
        greenup.tm_year = yr - 1 - 1900;
        delta_t = difftime(mktime(&date), mktime(&greenup)) / 86400;
    }
    // Green-up start date is the first non-winter value (not 0th).
    int t_days = delta_t + 1;
    // Check if date is in growing season or winter (cured) season.
    if (t_days < (sizeof(PERCENT_CURED)/sizeof(PERCENT_CURED[0])-1) * 10) {
        // Linearly interpolate between every 10-day value.
        double pc_0 = PERCENT_CURED[t_days / 10];
        double pc_1 = PERCENT_CURED[t_days / 10 + 1];
        double t_10day = (t_days%10) / 10.0;
        return pc_0 + (pc_1-pc_0)*t_10day;
    }
    else {
        return PERCENT_CURED[sizeof(PERCENT_CURED)/sizeof(PERCENT_CURED[0])-1];
    }
}

int save_rounded(FILE *file, const char *fmt, const double value)
{
    // Limit output data rows to an arbitrary 1000 characters.
    char buffer[1000];
    const size_t len = snprintf(buffer, sizeof(buffer), fmt, value);
    if (len >= sizeof(buffer)) {
        puts("Error: arbitrary limit in save_rounded() is too small");
        exit(-1);
    }
    if (0 == strcmp(buffer, "-0.0")) {
#ifndef NDEBUG
        printf("Converting %s with %f to 0.0\n", fmt, value);
#endif
        fprintf(file, "0.0");
    }
    else {
#ifndef NDEBUG
        printf("Converting %s with %f to %s\n", fmt, value, buffer);
#endif
        fprintf(file, "%s", buffer);
    }
    return len;
}

void save_csv(FILE *file, const char *fmt_all, ...)
{
    char *buffer = (char *)malloc(sizeof(char) * (strlen(fmt_all) + 1));
    strcpy(buffer, fmt_all);
    va_list args;
#ifndef NDEBUG
    va_start(args, fmt_all);
    // can't reuse args after vprintf()
    printf("Called save_csv() with %s\n", fmt_all);
    vprintf(fmt_all, args);
    va_end(args);
#endif
    va_start(args, fmt_all);
    int j = 0;
    char delim_buffer[2];
    delim_buffer[1] = '\0';
    while (buffer[j] != '\0') {
        int k = j;
        // Find next delimiter or end of format.
        while (buffer[k] != '\n' && buffer[k] != '\0' && buffer[k] != ',') {
            ++k;
        }
        delim_buffer[0] = fmt_all[k];
        // Replace with '\0' and then undo so we can use printf on substring.
        buffer[k] = '\0';
        if (k == j) {
            // Should be done.
            break;
        }
#ifndef NDEBUG
        printf("Checking for %s\n", &(buffer[j]));
#endif
        switch (buffer[k - 1]) {
        case 'd': ;
            int value_int = va_arg(args, int);
            // No need to guard against "-0.0".
#ifndef NDEBUG
        printf("Converting %s with %d to ", &(buffer[j]), value_int);
        printf(&(buffer[j]), value_int);
        printf("\n");
#endif
            fprintf(file, &(buffer[j]), value_int);
            break;
        case 'f':
            // Fall through.
        case 'g': ;
            double value_double = va_arg(args, double);
#ifndef NDEBUG
        printf("formatting %s with %f\n", &(buffer[j]), value_double);
#endif
            save_rounded(file, &(buffer[j]), value_double);
            break;
        default:
            printf("***** ERROR: invalid format string %s", buffer);
            exit(-1);
        }
        buffer[k] = delim_buffer[0];
        if (delim_buffer[0] != '\0') {
            fprintf(file, "%s", delim_buffer);
        }
        j = k + 1;
    }
    va_end(args);
    free(buffer);
}
