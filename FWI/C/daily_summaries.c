/*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/.
*/


/*
Summarize hourly FWI outputs into daily peak burn metrics.
*/


/*** Import packages *********************************************************/

#include <math.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "NG_FWI.h"
#include "util.h"


/*** Structures **************************************************************/

struct pseudo_date {
    int year;
    int yday;
};

struct daily_summary {
    double ffmc, dmc, dc, isi, bui, fwi, dsr;
    double gfmc, gsi, gfwi;
    double ws_smooth, isi_smooth, gsi_smooth;
    int duration;
    char sunrise[6], sunset[6];
    int peak_hour, year, month, day;
};

struct hour_values {
    double lat, lon, timezone;
    int year, month, day, hour;
    double ws, percent_cured, sunrise, sunset;
    double ffmc, dmc, dc, isi, bui, fwi, dsr;
    double mcgfmc_matted, mcgfmc_standing, gfmc, gsi, gfwi;
    double smooth_ws, smooth_isi;
    // Track when struct_hr is new or has been placed into struct_date (used).
    bool flag;
};

struct day_values {
    struct pseudo_date* p_day;
    struct hour_values* hour[24];
    int n_hrs;
};


/*** Functions ***************************************************************/

/**
* Calculate a smoothed vector weighted by binomial coefficients
* @param    source      Pointer to a double array of values
* @param    n           Length of source array
* @param    smoothed    Pointer to a new double array
*/
void smooth_binomial_5pt(double *source, int n, double *smoothed){
    int i, j;
    bool flag;
    // Calculate interior (cells with 2 neighbours either side) with binomial
    // coefficient weighting: [1, 4, 6, 4, 1].
    if (n >= 5) {
        for (i = 2; i <= n - 3; i++) {
            flag = true;
            for (j = i - 2; j <= i + 2; j++) {
                if (source[j] < -90.0) {
                    flag = false;
                    break;
                }
            }
            if (flag) {
                smoothed[i] = (source[i - 2]
                               +4.0*source[i - 1]
                               +6.0*source[i]
                               +4.0*source[i + 1]
                               +source[i + 2]) / 16.0;
            }
            else {
                smoothed[i] = source[i];
            }
        }
    }
    // Calculate inner edges (only 1 neighbour each side) weighted: [1, 2, 1].
    if (n >= 3) {
        flag = true;
        for (i = 0; i <= 2; i++) {
            if (source[i] < -90.0) {
                flag = false;
                break;
            }
        }
        if (flag) {
            smoothed[1] = (source[0]+2.0*source[1]+source[2]) / 4.0;
        }
        else {
            smoothed[1] = source[1];
        }
        if (n > 3) {
            flag = true;
            for (i = n - 3; i < n; i++) {
                if (source[i] < -90.0) {
                    flag = false;
                    break;
                }
            }
            if (flag) {
                smoothed[n - 2] = (source[n - 3]
                                   +2.0*source[n - 2]
                                   +source[n - 1]) / 4.0;
            }
            else {
                smoothed[n - 2] = source[n - 2];
            }
        }
    }
    // Copy start and end values of array.
    smoothed[0] = source[0];
    smoothed[n - 1] = source[n - 1];
}

/**
 * Calculate a pseudo date that changes at another hour past midnight
 * 
 * @param    yr        Year
 * @param    mon       Month number
 * @param    day       Day of month
 * @param    hr        Hour of day
 * @param    reset_hr  The new boundary hour instead of midnight (default 5)
 * @param    pd        Pointer to a pseudo_date structure
 */
void pseudo_date(
    int yr,
    int mon,
    int day,
    int hr,
    int reset_hr,
    struct pseudo_date *pd
) {
    int adjusted_jd;
    int adjusted_year;
    // Using julian function due to weird behaviour at 23:00 with
    // struct tm and mktime().
    if (hr < reset_hr) {
        adjusted_jd = julian(yr, mon, day) - 1;
    } else {
        adjusted_jd = julian(yr, mon, day);
    }
    // When ordinal day 1 (Jan 1) shifts to 0, bump it to end of previous year.
    if (adjusted_jd == 0) {
        adjusted_jd = julian(yr - 1, 12, 31);
        adjusted_year = yr - 1;
    } else {
        adjusted_year = yr;
    }
    // Fill in pseudo date structure.
    pd->yday = adjusted_jd;
    pd->year = adjusted_year;
}

/**
 * Load hourly data until pseudo-date changes
 * 
 * @param    inp            Input file pointer
 * @param    struct_date    day_values structure to load hourly data into
 * @param    struct_hr      hour_values structure to store next hour's data in
 * @param    reset_hour     The new boundary hour instead of midnight
 *                              (default 5)
 * @return                  Number of arguments read by fscanf()
 */
int read_row_daily_summaries(
    FILE *inp,
    struct day_values *struct_date,
    struct hour_values *struct_hr,
    int reset_hour
) {
    int err = 0;
    char comma[1];
    int i, waste_i, year, month, day, hour, h;
    double waste_f, ws, percent_cured, sunrise, sunset;
    double ffmc, dmc, dc, isi, bui, fwi, dsr;
    double mcgfmc_matted, mcgfmc_standing, gfmc, gsi, gfwi;
    struct pseudo_date p_date;
    for (i = 0; true; i++) {
        if (struct_hr->flag) {
            // Don't read in another line when previous struct_hr has not been
            // placed into struct_date yet (when p_date just changed).
            err = fscanf(
                inp,
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
                &waste_f, comma, &waste_f, comma, &waste_f, comma,
                &year, comma, &month, comma, &day, comma, &hour, comma,
                &waste_f, comma, &waste_f, comma, &ws, comma, &waste_f, comma,
                &waste_f, comma, &percent_cured, comma, &waste_f, comma,
                &sunrise, comma, &sunset, comma, &waste_f, comma,
                &waste_f, comma, &ffmc, comma, &dmc, comma, &dc, comma,
                &isi, comma, &bui, comma, &fwi, comma, &dsr, comma,
                &mcgfmc_matted, comma, &mcgfmc_standing, comma,
                &gfmc, comma, &gsi, comma, &gfwi, comma,
                &waste_f, comma, &waste_i
            );
            if (err == EOF) {
                // Reached end of input file, exit.
                return err;
            }
            struct_hr->year = year;
            struct_hr->month = month;
            struct_hr->day = day;
            struct_hr->hour = hour;
            struct_hr->ws = ws;
            struct_hr->percent_cured = percent_cured;
            struct_hr->sunrise = sunrise;
            struct_hr->sunset = sunset;
            struct_hr->ffmc = ffmc;
            struct_hr->dmc = dmc;
            struct_hr->dc = dc;
            struct_hr->isi = isi;
            struct_hr->bui = bui;
            struct_hr->fwi = fwi;
            struct_hr->dsr = dsr;
            struct_hr->mcgfmc_matted = mcgfmc_matted;
            struct_hr->mcgfmc_standing = mcgfmc_standing;
            struct_hr->gfmc = gfmc;
            struct_hr->gsi = gsi;
            struct_hr->gfwi = gfwi;
        }
        // Calculate pseudo date from new hour or previously held hour.
        pseudo_date(
            struct_hr->year,
            struct_hr->month,
            struct_hr->day,
            struct_hr->hour,
            reset_hour,
            &p_date
        );
        if (struct_date->n_hrs == 0) {
            // Update p_date if struct_date is new.
            struct_date->p_day->yday = p_date.yday;
            struct_date->p_day->year = p_date.year;
        }
        if (p_date.yday == struct_date->p_day->yday &&
            p_date.year == struct_date->p_day->year) {
            // Copy struct_hr values to corresponding hour in struct_date.
            h = struct_date->n_hrs;
            struct_date->hour[h]->year = struct_hr->year;
            struct_date->hour[h]->month = struct_hr->month;
            struct_date->hour[h]->day = struct_hr->day;
            struct_date->hour[h]->hour = struct_hr->hour;
            struct_date->hour[h]->ws = struct_hr->ws;
            struct_date->hour[h]->percent_cured = struct_hr->percent_cured;
            struct_date->hour[h]->sunrise = struct_hr->sunrise;
            struct_date->hour[h]->sunset = struct_hr->sunset;
            struct_date->hour[h]->ffmc = struct_hr->ffmc;
            struct_date->hour[h]->dmc = struct_hr->dmc;
            struct_date->hour[h]->dc = struct_hr->dc;
            struct_date->hour[h]->isi = struct_hr->isi;
            struct_date->hour[h]->bui = struct_hr->bui;
            struct_date->hour[h]->fwi = struct_hr->fwi;
            struct_date->hour[h]->dsr = struct_hr->dsr;
            struct_date->hour[h]->mcgfmc_matted = struct_hr->mcgfmc_matted;
            struct_date->hour[h]->mcgfmc_standing = struct_hr->mcgfmc_standing;
            struct_date->hour[h]->gfmc = struct_hr->gfmc;
            struct_date->hour[h]->gfwi = struct_hr->gfwi;
            struct_date->hour[h]->gsi = struct_hr->gsi;
            struct_date->n_hrs++;
            // Flag when struct_hr has been placed into struct_date (used).
            struct_hr->flag = true;
        }
        else {
            // New pseudo-date, exit function but still hold updated struct_hr!
            struct_hr->flag = false;
            return err;
        }
    }
    return err;
}

/**
 * Calculate daily summaries given hourly FWI indices from a pseudo date
 * 
 * @param    day             day_values structure for a pseudo-date
 * @param    bw_threshold    isi_smooth threshold for active burning
 *                               (default 5)
 * @return                   daily_summary structure of peak FWI conditions
 */
struct daily_summary generate_daily_summary(
    struct day_values day,
    double bw_threshold
) {
    struct daily_summary struct_ds;
    int i;
    /*** Copy date to output ***/
    struct_ds.year = day.hour[0]->year;
    struct_ds.month = day.hour[0]->month;
    struct_ds.day = day.hour[0]->day;
    // Format sunrise and sunset as hh:mm instead of decimal hours.
    double sr, ss;
    sr = day.hour[0]->sunrise;
    ss = day.hour[0]->sunset;
    sprintf(struct_ds.sunrise, "%02d:%02d", (int)sr, (int)(60 * (sr-(int)sr)));
    sprintf(struct_ds.sunset, "%02d:%02d", (int)ss, (int)(60 * (ss-(int)ss)));
    /*** Find daily peak active burning hour ***/
    int peak_time = 0;
    double ffmc_max = 0;
    bool ab_flag = false;
    struct tm t_ab0 = {
        .tm_year = day.hour[0]->year - 1900,
        .tm_mon = day.hour[0]->month - 1,
        .tm_mday = day.hour[0]->day,
        .tm_hour = day.hour[0]->hour,
        .tm_isdst = 0
    };
    struct tm t_ab1 = t_ab0;
    double *ws_pt = (double*)malloc(sizeof(double) * day.n_hrs);
    for (i = 0; i < day.n_hrs; i++) {
        ws_pt[i] = day.hour[i]->ws;
    }
    double *ws_smooth_pt = (double*)malloc(sizeof(double) * day.n_hrs);
    smooth_binomial_5pt(ws_pt, day.n_hrs, ws_smooth_pt);
    for (i = 0; i < day.n_hrs; i++) {
        day.hour[i]->smooth_ws = ws_smooth_pt[i];
        day.hour[i]->smooth_isi = initial_spread_index(
            day.hour[i]->smooth_ws,
            day.hour[i]->ffmc
        );
        // Find first and last hours of active burning.
        if (day.hour[i]->smooth_isi >= bw_threshold) {
            if (!ab_flag) {
                t_ab0.tm_year = day.hour[i]->year - 1900;
                t_ab0.tm_mon = day.hour[i]->month - 1;
                t_ab0.tm_mday = day.hour[i]->day;
                t_ab0.tm_hour = day.hour[i]->hour;
                ab_flag = true;
            }
            t_ab1.tm_year = day.hour[i]->year - 1900;
            t_ab1.tm_mon = day.hour[i]->month - 1;
            t_ab1.tm_mday = day.hour[i]->day;
            t_ab1.tm_hour = day.hour[i]->hour;
        }
        if (day.hour[i]->ffmc > ffmc_max) {
            ffmc_max = day.hour[i]->ffmc;
        }
        if (day.hour[i]->smooth_isi > day.hour[peak_time]->smooth_isi) {
            peak_time = i;
        }
    }
    if (ffmc_max < 85.0) {
        // 12 hours into pseudo date.
        peak_time =  12;
    }
    struct_ds.peak_hour = day.hour[peak_time]->hour;
    if (ab_flag) {
        struct_ds.duration = (int)difftime(mktime(&t_ab1), mktime(&t_ab0))
                             /3600 + 1;
    }
    else {
        struct_ds.duration = 0;
    }
    /*** Add FWI components at peak burning hour ***/
    struct_ds.ffmc = day.hour[peak_time]->ffmc;
    struct_ds.dmc = day.hour[peak_time]->dmc;
    struct_ds.dc = day.hour[peak_time]->dc;
    struct_ds.isi = day.hour[peak_time]->isi;
    struct_ds.bui = day.hour[peak_time]->bui;
    struct_ds.fwi = day.hour[peak_time]->fwi;
    struct_ds.dsr = day.hour[peak_time]->dsr;
    struct_ds.gfmc = day.hour[peak_time]->gfmc;
    struct_ds.gsi = day.hour[peak_time]->gsi;
    struct_ds.gfwi = day.hour[peak_time]->gfwi;
    struct_ds.ws_smooth = day.hour[peak_time]->smooth_ws;
    struct_ds.isi_smooth = day.hour[peak_time]->smooth_isi;
    // Recalculate if grassland fuel matted or standing.
    bool standing;
    double mcgfmc;
    struct tm ts = {
        .tm_year = day.hour[peak_time]->year - 1900,  // Years since 1900.
        .tm_mon = day.hour[peak_time]->month - 1,  // 0-indexed month.
        .tm_mday = day.hour[peak_time]->day,
        .tm_hour = 0,
        .tm_isdst = 0
    };
    struct tm DATE_GRASS_STANDING = {
        .tm_year = day.hour[peak_time]->year - 1900,
        .tm_mon = MON_STANDING - 1,
        .tm_mday = DAY_STANDING,
        .tm_isdst = 0
    };
    if (GRASS_TRANSITION &&
            difftime(mktime(&ts), mktime(&DATE_GRASS_STANDING)) < 0) {
        standing = false;
        mcgfmc = day.hour[peak_time]->mcgfmc_matted;
    }
    else {
        standing = true;
        mcgfmc = day.hour[peak_time]->mcgfmc_standing;
    }
    struct_ds.gsi_smooth = grass_spread_index(
        struct_ds.ws_smooth,
        mcgfmc,
        day.hour[peak_time]->percent_cured,
        standing
    );
    // Deallocate memory.
    free(ws_pt);
    free(ws_smooth_pt);
    return struct_ds;
}


/*** Command line file execution *********************************************/

int main(int argc, char *argv[]) {
    if (argc < 3) {
        printf("\n########\nhelp/usage:\n%s input output [reset_hr] "
               "[bw_threshold] [silent]\n\n", argv[0]);
        puts("argument descriptions:\n"
             "input           Input csv data file\n"
             "output          Output csv file name and location\n"
             "reset_hr        Hour defining new day to summarize (default 5)\n"
             "bw_threshold    isi_smooth threshold for active burning "
                                "(default 5)\n"
             "silent          Suppress print statements (default False)\n"
             "########\n\n");
        exit(1);
    }
    // Headers for CSV files.
    static const char *header_in = "lat,long,timezone,yr,mon,day,hr,temp,rh,"
                                   "ws,prec,grass_fuel_load,percent_cured,"
                                   "solrad,sunrise,sunset,sunlight_hours,"
                                   "mcffmc,ffmc,dmc,dc,isi,bui,fwi,dsr,"
                                   "mcgfmc_matted,mcgfmc_standing,gfmc,gsi,"
                                   "gfwi,prec_cumulative,canopy_drying";
    static const char *header_out = "yr,mon,day,sunrise,sunset,peak_hr,"
                                    "duration,ffmc,dmc,dc,isi,bui,fwi,dsr,"
                                    "gfmc,gsi,gfwi,ws_smooth,isi_smooth,"
                                    "gsi_smooth";
    int reset_hr;
    double bw_threshold;
    bool silent;
    /*** Load optional argument if provided, or set to default ***/
    if (argc > 3) {
        reset_hr = atoi(argv[3]);
    }
    else {
        reset_hr = 5;
    }
    if (argc > 4) {
        bw_threshold = atof(argv[4]);
    }
    else {
        bw_threshold = 5.0;
    }
    if (argc > 5) {
        if (strcmp(argv[5], "true") == 0) {
            silent = true;
        }
        else if (strcmp(argv[5], "false") == 0) {
            silent = false;
        }
        else {
            puts("\n'silent' can only be [true], [false], or blank "
                 "(default false)");
            exit(1);
        }
    }
    else {
        silent = false;
    }
    if (argc > 6) {
        puts("Warning: too many arguments provided, some unused");
    }
    if (!silent) {
        printf("\n########\nFWI2025: Daily Summaries (%s)\n\n", version());
    }
    /*** Open input and output files ***/
    FILE *inp = fopen(argv[1], "r");
    if (!silent) {
        printf("Opening input file >>> %s\n", argv[1]);
    }
    if (inp == NULL) {
        printf("\n\n ***** FILE  %s  does not exist\n", argv[1]);
        exit(1);
    }
    check_header_match(inp, header_in);
    FILE *out = fopen(argv[2], "w");
    if (out == NULL) {
        printf("\n\n***** FILE %s can not be opened\n", argv[2]);
        exit(1);
    }
    if (!silent) {
        printf("Saving outputs to file >>> %s\n\n", argv[2]);
    }
    fprintf(out, "%s\n", header_out);
    /*** run generate_daily_summaries() ***/
    if (!silent) {
        puts("Summarizing to daily");
    }
    struct hour_values *struct_hr;
    struct_hr = (struct hour_values*)malloc(sizeof(struct hour_values));
    // Flag for when struct_hr is new or has been placed into struct_date.
    struct_hr->flag = true;
    int err = 1;
    while (err > 0) {  // While there is a next row of data in input file.
        // Allocate storage in struct_date including all internal structures.
        struct day_values *struct_date = (struct day_values*)malloc(
            sizeof(struct day_values)
        );
        struct_date->p_day = (struct pseudo_date*)malloc(
            sizeof(struct pseudo_date)
        );
        for (int i = 0; i< 24; i++) {
            struct_date->hour[i] = (struct hour_values*)malloc(
                sizeof(struct hour_values)
            );
        }
        struct_date->n_hrs = 0;
        err = read_row_daily_summaries(inp, struct_date, struct_hr, reset_hr);
        // Run summaries only when there are at least 12 hours in pseudo date.
        if (struct_date->n_hrs > 12) {
            struct daily_summary struct_ds = generate_daily_summary(
                *struct_date,
                bw_threshold
            );
            fprintf(
                out,
                "%d,%d,%d,"
                "%s,%s,"
                "%d,%d,"
                "%.4f,%.4f,%.4f,"
                "%.4f,%.4f,%.4f,%.4f,"
                "%.4f,%.4f,%.4f,"
                "%.4f,%.4f,%.4f\n",
                struct_ds.year, struct_ds.month, struct_ds.day,
                struct_ds.sunrise, struct_ds.sunset,
                struct_ds.peak_hour, struct_ds.duration,
                struct_ds.ffmc, struct_ds.dmc, struct_ds.dc,
                struct_ds.isi, struct_ds.bui, struct_ds.fwi, struct_ds.dsr,
                struct_ds.gfmc, struct_ds.gsi, struct_ds.gfwi,
                struct_ds.ws_smooth, struct_ds.isi_smooth, struct_ds.gsi_smooth
            );
        }
        // Deallocate memory.
        free(struct_date->p_day);
        for (int i = 0; i < 24; i++) {
            free(struct_date->hour[i]);
        }
        free(struct_date);
    }
    free(struct_hr);
    fclose(inp);
    fclose(out);
    if (!silent) {
        puts("########\n");
    }
    return 0;
}
