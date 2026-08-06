/*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/.
*/


/*
Compute daily minimum and maximum (minmax) weather from traditional
daily values (13:00 Local Daylight Time or 12:00 Local Standard Time).
Relationship between minmax and daily values determined statistically from
historical Canadian provincial and territorial weather station data.
*/


/*** Include guard and import libraries **************************************/

#ifndef MAKE_MINMAX_H
#define MAKE_MINMAX_H

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"


/*** Function declarations ***************************************************/

/**
 * Convert daily temperature at 13:00 LDT or 12:00 LST to daily minmax
 * 
 * @param temp_day  Daily temperature at 13:00 LDT or 12:00 LST [°C]
 * @param rh_day    Daily relative humidity at 13:00 LDT or 12:00 LST [%]
 * @param temp_min  Pointer to minimum temperature [°C]
 * @param temp_max  Pointer to maximum temperature [°C]
 */
void temp_min_max(double temp_day, double rh_day,
                  double *temp_min, double *temp_max);

/**
 * Find specific humidity, the mass ratio of water vapour (g) to all air (kg)
 *
 * @param temp        Temperature (°C)
 * @param rh          Relative humidity (%)
 * @return            Specific humidity (g/kg)
 */
double find_q(double temp, double rh);


/**
 * Find relative humidity
 *
 * @param q           Specific humidity (g/kg)
 * @param temp        Temperature (°C)
 * @return            Relative humidity (%)
 */
double find_rh(double q, double temp);

#endif
