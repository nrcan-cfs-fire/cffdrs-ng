/* BMW---just my own prototyping for testing*/
#include <math.h>
double standing_grass_spread_ROS(double ws, double mc, double cur);
double grass_spread_index(double ws, double mc, double cur );
 double matted_grass_spread_ROS(double ws, double mc, double cur);
 double standing_grass_spread_ROS(double ws, double mc, double cur);

 double curing_factor(double cur);

 /**
 * Calculate Grass Spread Index (GSI)
 *
 * @param ws              10 metre OPEN Wind Speed (km/h)
 * @param mc              Grass moisture content (fully cured grass) (percent)
 * @param cur             Degree of curing (percent, 0-100)
 * @return                Grass Spread Index
 */

double grass_spread_index(double ws, double mc, double cur )
/*
   So we don't have to transition midseason between standing and matted grass spread rate models
   We will simply scale   GSI   by the average of the   matted and standing spread rates

*/

{

  const double ros = ( matted_grass_spread_ROS(ws,  mc,  cur) + standing_grass_spread_ROS(ws,  mc,  cur)  ) / 2.0;
  return 1.11 * ros;
}



/* BMW note to be deleted: .  I am seperating out these pieces.....so they are functions we can use more than once.....
   we use them in a few places
*/



 double matted_grass_spread_ROS(double ws, double mc, double cur)
 /*  CUT grass  Rate  of spread from cheney 1998  (and new CSIRO grassland code
  We use this for MATTED grass in our post-winter context
  --ws=10 m open wind km/h
  --mc = moisture content in  cured grass  (%)
  --cur = percentage of grassland cured  (%)
  output should be ROS in m/min   */

{
  const double fw = 16.67 * (ws < 5 ? 0.054 + 0.209 * ws : 1.1 + 0.715 * (ws - 5.0) * 0.844);

  /* NOTE: between [12, ~12.01754] the value for ws < 10 is greater than ws >= 10 */
  /* using 0.6838 instead would mean this is always less than ws >= 10
  ........this is fine with me   BMW*/
  /* mc < 23.9 because of check at start of function, so last expression is any ws >= 10 */

  const double fm = mc < 12
                    ? exp(-0.108 * mc)
                    : (mc < 20.0 && ws < 10.0
                         ? 0.6838 - 0.0342 * mc
                         : (mc < 23.9 && ws >= 10.0
                              ? 0.547 - 0.0228 * mc
                              : 0.0));

  const double cf = curing_factor(cur);


  return  (fw * fm * cf);
}


 double standing_grass_spread_ROS(double ws, double mc, double cur)
 /*  standing grass  Rate  of spread from cheney 1998  (and new CSIRO grassland code)
  We use this for standing grass in our post-winter context
  ITS only the WIND function that chnges here between cut and standing

  --ws=10 m open wind km/h
  --mc = moisture content in grass  (%)
  --cur = percentage of grassland cured  (%)
  output should be ROS in m/min   */

{
  const double fw = 16.67 * (ws < 5 ? 0.054 + 0.269 * ws : 1.4 + 0.838 * (ws - 5.0) * 0.844);

  /* NOTE: between [12, ~12.01754] the value for ws < 10 is greater than ws >= 10 */
  /* using 0.6838 instead would mean this is always less than ws >= 10
  ........this is fine with me   BMW*/
  /* mc < 23.9 because of check at start of function, so last expression is any ws >= 10 */

  const double fm = mc < 12
                    ? exp(-0.108 * mc)
                    : (mc < 20.0 && ws < 10.0
                         ? 0.6838 - 0.0342 * mc
                         : (mc < 23.9 && ws >= 10.0
                              ? 0.547 - 0.0228 * mc
                              : 0.0));

  const double cf = curing_factor(cur);


  return  fw * fm * cf;
}


 double curing_factor(double cur)
/*  cur is the percentage cure of the grass fuel complex.  100= fully cured
  ....The OPPOSITE (100-x) of greenness...

   This is the Cruz et al (2015) model with the original precision of the coefficent estimates
   and as in CSIRO code:https://research.csiro.au/spark/resources/model-library/csiro-grassland-models/

 */
 {
   double cf;
   if ( cur >= 20.0) cf=1.036 / (1 + 103.989 * exp(-0.0996 * (cur - 20)));
   else cf=0.0;

   return  cf;
 }
