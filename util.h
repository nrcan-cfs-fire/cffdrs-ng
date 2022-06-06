#ifndef _UTIL_H
#define _UTIL_H
#include <stdio.h>
#include <math.h>
#include <float.h>
#include <stdlib.h>
#include <string.h>

const float pi;

float findQ(float temp, float rh);
float findrh(float q, float temp);
float sun(float lat,float lon, int mon,int day,int hour, int timezone, float *sunrise, float *sunset);
float sun_julian(float lat,float lon, int jd,int hour,int timezone, float *sunrise, float *sunset);
int julian(int mon, int day);
#endif