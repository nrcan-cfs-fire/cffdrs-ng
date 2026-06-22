/*
Compute daily minimum and maximum (minmax) weather from traditional
daily values (13:00 Local Daylight Time or 12:00 Local Standard Time)
*/


/*** Import packages *********************************************************/

#include "make_minmax.h"
#include "util.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/*** Functions ***************************************************************/

void temp_min_max(double temp_day, double rh_day,
                  double *temp_min, double *temp_max)
{
    double temp_range = 0.22*temp_day - 0.16*rh_day + 17;
    if (temp_range <= 2) {
        *temp_max = temp_day + 1;
        *temp_min = temp_day - 1;
    }
    else {
        *temp_max = temp_day + 2;
        *temp_min = *temp_max - temp_range;
    }
}

double find_q(double temp, double rh)
{
    double svp, vp, q;
    svp = 6.108 * exp(17.27*temp/(temp+237.3));
    vp = svp * rh / 100.0;
    q = 217.0 * vp / (273.17+temp);
    return(q);
}

double find_rh(double q, double temp)
{
    double cur_vp, rh;
    cur_vp = (273.17+temp) * q / 217.0;
    rh = 100.0 * cur_vp / (6.108*exp(17.27*temp/(temp+237.3)));
    return(rh);
}


/*** Command line file execution *********************************************/

/**
 * Convert daily weather at 13:00 LDT / 12:00 LST to daily minmax statistically
 */
int main(int argc, char *argv[])
{
    /*** Parse arguments ***/
    if (argc < 3) {
        printf("\n########\nhelp/usage:\n"
          "%s input output [silent]\n\n", argv[0]);
        printf("argument descriptions:\n"
          "input     Input csv data file\n"
          "output    Output csv file name and location\n"
          "silent    Suppresses informative print statements (default false)\n"
          "########\n\n");
        exit(1);
    }
    bool silent;
    if (argc > 3) {
        if (strcmp(argv[3], "true") == 0) {
            silent = true;
        }
        else if (strcmp(argv[3], "false") == 0) {
            silent = false;
        }
        else {
            puts("\nERROR: 'silent' can only be [true], [false], or blank "
                 "(default false)");
            exit(1);
        }
    }
    else {
        silent = false;
    }
    if (argc > 4) {
        puts("Warning: too many arguments provided, some unused");
    }
    if (!silent) {
        printf("\n########\nFWI2025: Make Min/Max Inputs (%s)\n\n", version());
    }
    /*** Open input and output file connections ***/
    static const char *header_in = "lat,long,yr,mon,day,temp,rh,ws,prec";
    static const char *header_out = "lat,long,yr,mon,day,temp_min,temp_max,"
                                    "rh_min,rh_max,ws_min,ws_max,prec";
    FILE *inp = fopen(argv[1], "r");
    if (NULL == inp) {
        printf("\nERROR: Input file does not exist\n%s\n", argv[1]);
        exit(1);
    }
    if (!silent) {
        printf("Opening input file >>> %s\n", argv[1]);
    }
    check_header_match(inp, header_in);
    FILE *out = fopen(argv[2], "w");
    if (out == NULL) {
        printf("\nERROR: Output file can not be opened\n%s\n", argv[2]);
        exit(1);
    }
    if (!silent) {
        printf("Saving outputs to file >>> %s\n\n", argv[2]);
    }
    fprintf(out, "%s\n", header_out);
    /*** Start calculation ***/
    struct row_daily cur;
    double temp_min, temp_max, q, rh_min, rh_max, ws_min, ws_max;
    if (!silent) {
        puts("Predicting daily min/max weather");
    }
    int err = read_row_daily(inp, &cur);
    while (err > 0) {
        /*** Calculate minmax temperature ***/
        temp_min_max(cur.temp, cur.rh, &temp_min, &temp_max);
        /*** Calculate minmax relative humidity ***/
        // Calculate specific humidity, assume it is the same at minmax temp.
        q = find_q(cur.temp, cur.rh);
        // Assume min relative humidity happens at max temp and vice versa.
        rh_min = fmin(100.0, find_rh(q, temp_max));
        rh_max = fmin(100.0, find_rh(q, temp_min));
        /*** Calculate minmax wind speed ***/
        ws_min = 0.15 * cur.wind;
        ws_max = 1.25 * cur.wind;
        save_csv(out, "%.4f,%.4f,%d,%d,%d,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%f\n",
                 cur.lat, cur.lon, cur.year, cur.mon, cur.day,
                 temp_min, temp_max, rh_min, rh_max, ws_min, ws_max, cur.rain);
        err = read_row_daily(inp, &cur);
    }
    /*** Close connections and return ***/
    fclose(inp);
    fclose(out);
    if (!silent) {
        puts("########\n");
    }
    return(0);
}
