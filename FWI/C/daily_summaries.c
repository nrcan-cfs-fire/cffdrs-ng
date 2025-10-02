#include "util.h"
#include "NG_FWI.h"
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

struct pseudo_date{
    int year;
    int jd;
};

struct daily_summary{
    double ffmc;
    double dmc;
    double dc;
    double isi;
    double bui;
    double fwi;
    double dsr;
    double gfmc;
    double gsi;
    double gfwi;
    double ws_smooth;
    double isi_smooth;
    double gsi_smooth;
    int duration;
    double sunrise;
    double sunset;
    int peak_hour;
    int year;
    int month;
    int day;
};

struct hour_vals {
    int hour;
    double ffmc;
    double dmc;
    double dc;
    double isi;
    double bui;
    double fwi;
    double dsr;
    double gfmc;
    double gsi;
    double gfwi;
    double ws;

    double smooth_ws;
    double smooth_isi;

    double mcgfmc_matted;
    double mcgfmc_standing;
    double perc_cured;

    double sunrise;
    double sunset;

    int year;
    int month;
    int day;
};

struct day_vals {
    struct pseudo_date* p_day;
    struct hour_vals* day[24];
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
            dest[i] = ((1.0/16.0)*(source[i-2])) + ((4.0/16.0)*(source[i-1])) + ((6.0/16.0)*(source[i])) + ((4.0/16.0)*(source[i+1])) + ((1.0/16.0)*(source[i+2]));
        }
        else{
          dest[i] = source[i];  
        }
    }

    miss = 0;
    for(i = (source_len); i < source_len; i++){
        if(*(source + i) < -90.0){
            miss = miss + 1;
        }
    }
    if(miss == 0){
        dest[source_len - 2] = (0.25*(source[source_len-3])) + (0.5*(source[source_len-2]))+ (0.25*(source[source_len-1]));
    }
    else{
        dest[source_len - 2] = source[source_len - 2];

    }
}


//this function calculates a fake date based on 5am-5am instead of midnight to midnight
//used for generating daily summaries to make data look at 5am-5am instead
//output form is "year-julian_day", with the julian day rounded back if the time is before 5am
//for Jan 1st where the julian day rollback would make it zero it gets bumped to the last day of the previous year
void pseudo_date(int year, int month, int day, int hour, int reset_hour, struct pseudo_date *pd){
    int adjusted_jd;
    int adjusted_year;

    if(hour < reset_hour){
        adjusted_jd = julian(month, day) - 1;
    }
    else{
        adjusted_jd = julian(month, day);
    }

    if(adjusted_jd == 0){
        adjusted_jd = julian(12,31);
        adjusted_year = year -1;
    }
    else{
        adjusted_year = year;
    }

    //struct pseudo_date pd;
    pd->jd = adjusted_jd;
    pd->year = adjusted_year;
    //return pd;
}


//daily summary for a specific pseudo-date
struct daily_summary generate_daily_summary(struct day_vals day, int reset_hour){
    //printf("start generate daily summaries\n");

    printf("hour: %d\n", day.day[0]->hour);

    const double spread_threshold_isi = 5.0;
    struct daily_summary summary;

    int i;

    //calculate smoothed ws 
    double *ws_pt = (double*)malloc(sizeof(double)*day.hour_slots_filled);
    for(i = 0; i< day.hour_slots_filled; i++){
        ws_pt[i] = day.day[i]->ws;
    }
    double *ws_smmoth_pt= (double*)malloc(sizeof(double)*day.hour_slots_filled);
    smooth_5pt(ws_pt, day.hour_slots_filled, ws_smmoth_pt);
    //printf("smoothing function called\n");

    //calculate smoothed isi and find peak info
    int ffmc_max_spot = 0;
    int peak_time_spot = 0;
    int duration = 0;
    printf("\t hours filled: %d\n",day.hour_slots_filled);
    for(i = 0; i < day.hour_slots_filled; i++){
        day.day[i]->smooth_ws = ws_smmoth_pt[i];
        day.day[i]->smooth_isi = initial_spread_index(day.day[i]->smooth_ws, day.day[i]->ffmc);

        if(day.day[i]->smooth_isi > spread_threshold_isi){
            duration+=1;
        }
        if(day.day[i]->ffmc > day.day[ffmc_max_spot]->ffmc){
            ffmc_max_spot = i;
        }
        if(day.day[i]->smooth_isi > day.day[peak_time_spot]->smooth_isi){
            peak_time_spot = i;
        }
    }

    if(day.day[ffmc_max_spot]->ffmc < 85.0){
        peak_time_spot =  12;
    }
    //printf("starting summary struct building\n");
    printf("\t\t\t peak time spot: %d",peak_time_spot);
    summary.bui = day.day[peak_time_spot]->bui;
    summary.dc = day.day[peak_time_spot]->dc;
    summary.dmc = day.day[peak_time_spot]->dmc;
    summary.dsr = day.day[peak_time_spot]->dsr;
    summary.ffmc = day.day[peak_time_spot]->ffmc;
    summary.fwi = day.day[peak_time_spot]->fwi;
    summary.gfmc = day.day[peak_time_spot]->gfmc;
    summary.gfwi = day.day[peak_time_spot]->gfwi;
    summary.gsi = day.day[peak_time_spot]->gsi;
    summary.isi = day.day[peak_time_spot]->isi;
    summary.ws_smooth = day.day[peak_time_spot]->smooth_ws;
    summary.isi_smooth = day.day[peak_time_spot]->smooth_isi;
    summary.duration = duration;
    summary.sunrise = day.day[peak_time_spot]->sunrise;
    summary.sunset = day.day[peak_time_spot]->sunset;
    summary.peak_hour = day.day[peak_time_spot]->hour;
    summary.year = day.day[0]->year;
    summary.month = day.day[0]->month;
    summary.day = day.day[0]->day;

    bool standing;
    double mcgfmc;
    if(julian(day.day[peak_time_spot]->month,day.day[peak_time_spot]->day) < DATE_GRASS){
        standing = false;
        mcgfmc = day.day[peak_time_spot]->mcgfmc_matted;
    }
    else{
        standing = true;
        mcgfmc = day.day[peak_time_spot]->mcgfmc_standing;
    }
    summary.gsi_smooth = grass_spread_index(summary.ws_smooth, mcgfmc, day.day[peak_time_spot]->perc_cured, standing);

    free(ws_pt);
    free(ws_smmoth_pt);
    return summary;

}

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

    if((pos != 236) || !matching){
        printf("Header does not match\n");
        exit(1); 
    }
}

int read_row_daily_summaries(FILE *inp, struct day_vals *day, struct hour_vals *buffer, int reset_hour){
    //printf("read in function start\n");
    bool day_change_flag = false;
    int err = 0;
    int round = 0;
    while(!day_change_flag){
        printf("round: %d\n", round);
        round+=1;
        if(buffer->year == -1){
            //printf("need read in of line\n");
            //struct hour_vals hv;
            char waste_c[1];
            double waste_lf;
            int year, month, day, hour;
            double ws, ffmc, dmc, dc, isi, bui, fwi, dsr, gfmc, gsi, gfwi, mcgfmc_matted, mcgfmc_standing, sunrise, sunset, perc_cured;
            err = fscanf(inp,
                        "%lf%c%lf%c%d%c%d%c%d%c%d%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf%c%lf\n",
                        &waste_lf,
                        waste_c,
                        &waste_lf,
                        waste_c,
                        &year,
                        waste_c,
                        &month,
                        waste_c,
                        &day,
                        waste_c,
                        &hour,
                        waste_c,
                        &waste_lf,
                        waste_c,
                        &waste_lf,
                        waste_c,
                        &ws,
                        waste_c,
                        &waste_lf,
                        waste_c,
                        &waste_lf,
                        waste_c,
                        &sunrise,
                        waste_c,
                        &sunset,
                        waste_c,
                        &ffmc,
                        waste_c,
                        &dmc,
                        waste_c,
                        &dc,
                        waste_c,
                        &isi,
                        waste_c,
                        &bui,
                        waste_c,
                        &fwi,
                        waste_c,
                        &dsr,
                        waste_c,
                        &gfmc,
                        waste_c,
                        &gsi,
                        waste_c,
                        &gfwi,
                        waste_c,
                        &waste_lf,
                        waste_c,
                        &waste_lf,
                        waste_c,
                        &perc_cured,
                        waste_c,
                        &waste_lf,
                        waste_c,
                        &mcgfmc_matted,
                        waste_c,
                        &mcgfmc_standing,
                        waste_c,
                        &waste_lf,
                        waste_c,
                        &waste_lf,
                        waste_c,
                        &waste_lf,
                        waste_c,
                        &waste_lf);
            //printf("scanned\n");
            //buffer = malloc(sizeof(struct hour_vals));   
            printf("trasnfering to buffer, err = %d\n",err);
            buffer->year = year;
            buffer->month = month;
            buffer->day = day;
            buffer->hour = hour;
            buffer->ws = ws;
            buffer->ffmc = ffmc;
            buffer->dmc = dmc;
            buffer->dc = dc;
            buffer->isi = isi;
            buffer->bui = bui;
            buffer->fwi = fwi;
            buffer->dsr = dsr;
            buffer->gfmc = gfmc;
            buffer->gsi = gsi;
            buffer->gfwi = gfwi;
            buffer->mcgfmc_matted = mcgfmc_matted;
            buffer->mcgfmc_standing = mcgfmc_standing;
            buffer->sunrise = sunrise;
            buffer->sunset = sunset;
            buffer->perc_cured = perc_cured;
            //printf("exiting null if\n");             
        }
        if(err == EOF){
            break;
        }
        struct pseudo_date *pd = (struct pseudo_date*)malloc(sizeof(struct pseudo_date));
        pseudo_date(buffer->year, buffer->month, buffer->day, buffer->hour, reset_hour, pd);
        //printf("step2 read in: %d %d %d\n",pd.year, pd.jd, buffer->hour);
        if(day->hour_slots_filled == 0){
            printf("\ttrigger 1 read in  %d %d\n",pd->jd,pd->year);
            //printf("\t\t%d %d",day->p_day->jd, day->p_day->jd);
            day->day[0]->bui = buffer->bui;
            day->day[0]->day = buffer->day;
            day->day[0]->dc = buffer->dc;
            day->day[0]->dmc = buffer->dmc;
            day->day[0]->dsr = buffer->dsr;
            printf("\tstep 1    %d %d\n",pd->jd,pd->year);
            day->day[0]->ffmc = buffer->ffmc;
            day->day[0]->fwi = buffer->fwi;
            day->day[0]->gfmc = buffer->gfmc;
            day->day[0]->gfwi = buffer->gfwi;
            day->day[0]->gsi = buffer->gsi;
            day->day[0]->hour = buffer->hour;
            day->day[0]->isi = buffer->isi;
            printf("\tstep 2   %d %d\n",pd->jd,pd->year);
            day->day[0]->mcgfmc_matted = buffer->mcgfmc_matted;
            day->day[0]->mcgfmc_standing = buffer->mcgfmc_standing;
            day->day[0]->month = buffer->month;
            day->day[0]->perc_cured = buffer->perc_cured;
            day->day[0]->smooth_isi = buffer->smooth_isi;
            day->day[0]->smooth_ws = buffer->smooth_ws;
            day->day[0]->sunrise = buffer->sunrise;
            day->day[0]->sunset = buffer->sunset;
            printf("\tstep 3   %d %d\n",pd->jd,pd->year);
            day->day[0]->ws = buffer->ws;
            day->day[0]->year = buffer->year;
            day->hour_slots_filled = 1;
            //struct pseudo_date pd_lock;
            //pd_lock.jd = pd.jd;
            //pd_lock.year = pd.year;
            day->p_day->jd = pd->jd;
            day->p_day->year = pd->year;
            //printf("\t\t\t\t\t1  %d\n", day->day[0]->year);
            buffer->year = -1;
            printf("\tstep 4   %d %d\n",pd->jd,pd->year);
            //printf("\t\t\t\t\t2  %d\n", day->day[0]->year);
        }
        else if((pd->jd == day->p_day->jd) && (pd->year == day->p_day->year)){
            printf("\ttrigger 2 read in %d %d %d %d     -   %d\n",pd->jd,day->p_day->jd,pd->year,day->p_day->year, day->hour_slots_filled);
            day->day[day->hour_slots_filled]->bui = buffer->bui;
            day->day[day->hour_slots_filled]->day = buffer->day;
            day->day[day->hour_slots_filled]->dc = buffer->dc;
            day->day[day->hour_slots_filled]->dmc = buffer->dmc;
            day->day[day->hour_slots_filled]->dsr = buffer->dsr;
            printf("\tstep 1\n");
            day->day[day->hour_slots_filled]->ffmc = buffer->ffmc;
            day->day[day->hour_slots_filled]->fwi = buffer->fwi;
            day->day[day->hour_slots_filled]->gfmc = buffer->gfmc;
            day->day[day->hour_slots_filled]->gfwi = buffer->gfwi;
            day->day[day->hour_slots_filled]->gsi = buffer->gsi;
            day->day[day->hour_slots_filled]->hour = buffer->hour;
            printf("\tstep 2\n");
            day->day[day->hour_slots_filled]->isi = buffer->isi;
            day->day[day->hour_slots_filled]->mcgfmc_matted = buffer->mcgfmc_matted;
            day->day[day->hour_slots_filled]->mcgfmc_standing = buffer->mcgfmc_standing;
            day->day[day->hour_slots_filled]->month = buffer->month;
            day->day[day->hour_slots_filled]->perc_cured = buffer->perc_cured;
            day->day[day->hour_slots_filled]->smooth_isi = buffer->smooth_isi;
            printf("\tstep 3\n");
            day->day[day->hour_slots_filled]->smooth_ws = buffer->smooth_ws;
            day->day[day->hour_slots_filled]->sunrise = buffer->sunrise;
            day->day[day->hour_slots_filled]->sunset = buffer->sunset;
            day->day[day->hour_slots_filled]->ws = buffer->ws;
            day->day[day->hour_slots_filled]->year = buffer->year;
            //day->day[day->hour_slots_filled] = buffer;
            day->hour_slots_filled += 1;
            buffer->year = -1;
            printf("\tstep 4\n");
        }
        else{
            printf("\ttrigger 3 read in   -  %d %d %d %d\n",day->p_day->jd, pd->jd, day->p_day->year, pd->year);
            day_change_flag = true;
        }
        printf("%d %d\n",pd->jd, pd->year);
        free(pd);
        pd=NULL;
        printf("iteration done\n");
    }
    printf("exiting read in function: %d\n", day->hour_slots_filled);
    return err;
}

int main(int argc, char *argv[]){

    //################# args are --input file --output file

    //headers for csvs
    static const char *header_in = "lat,long,yr,mon,day,hr,temp,rh,ws,prec,solrad,sunrise,sunset,ffmc,dmc,dc,isi,bui,fwi,dsr,gfmc,gsi,gfwi,mcffmc,mcgfmc,percent_cured,grass_fuel_load,mcgfmc_matted,mcgfmc_standing,dmc_before_rain,dc_before_rain,prec_cumulative,conpy_drying";
    static const char *header_out = "yr,mon,day,sunrise,sunset,peak_hr,duration,ffmc,dmc,dc,isi,bui,fwi,dsr,gfmc,gsi,gfwi,ws_smooth,isi_smooth,gsi_smooth";

    if(argc != 4){
        printf("Command Line:    %s <input file> <output file> <reset hour>", argv[0]);
        printf("input file should be an output file from NG_FWI with all columns\n");
        printf("rounding must be an int for number of decimals or \"n\" for no rounding\n");
        exit(1);
    }


    int reset_hour = atof(argv[3]);
    //int rounding = NULL;
    //if (argv[4] != "n"){
    //    rounding = atof(argv[4]);
    //}


    //check input header
    FILE *inp = fopen(argv[1], "r");
    printf("Opening input file >>> %s   \n", argv[1]);
    if (inp == NULL){
        printf("\n\n ***** FILE  %s  does not exist\n", argv[1]);
        exit(1);
    }


    check_header_daily_summaries(inp, header_in);
    //printf("header good\n");
    struct hour_vals *buffer;
    buffer = (struct hour_vals*)malloc(sizeof(struct hour_vals));
    buffer->year=-1;
    //int *buffer_len = (int*)malloc(sizeof(int));
    //buffer_len = 0;
    FILE *out = fopen(argv[2], "w");
    fprintf(out, "%s\n", header_out);
    int err = 1;
    //printf("into loop\n");
    while(err>0){
        //run summary function for pseudo day
        struct day_vals *day = (struct day_vals*)malloc(sizeof(struct day_vals));
        day->p_day = (struct pseudo_date*)malloc(sizeof(struct pseudo_date));
        for(int i = 0; i< 24; i++){
            day->day[i] = (struct hour_vals*)malloc(sizeof(struct hour_vals));
        }
        day->hour_slots_filled=0;
        err = read_row_daily_summaries(inp, day, buffer, reset_hour);
        //printf("row read in %d\n",err);
        printf("buffer mem %d", buffer);
        printf("buffer: %d %d %d %d\n", buffer->year, buffer->month, buffer->day, buffer->hour);
        struct daily_summary summary = generate_daily_summary(*day,reset_hour);
        //printf("!!!!!!!!!!\n");

        //output
        fprintf(out, "%d,%d,%d,%f,%f,%d,%d,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f\n",
                summary.year,
                summary.month,
                summary.day,
                summary.sunrise,
                summary.sunset,
                summary.peak_hour,
                summary.duration,
                summary.ffmc,
                summary.dmc,
                summary.dc,
                summary.isi,
                summary.bui,
                summary.fwi,
                summary.dsr,
                summary.gfmc,
                summary.gsi,
                summary.gfwi,
                summary.ws_smooth,
                summary.isi_smooth,
                summary.gsi_smooth);

        free(day->p_day);
        for(int i = 0; i< 24; i++){
            free(day->day);
        }
        free(day);
        day=NULL;

    }

    free(buffer);
    fclose(inp);
    fclose(out);

    return 0;
}


