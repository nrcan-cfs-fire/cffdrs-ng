#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
/*  update ...changing all to DOUBLES    jan/24  bmw*/

double grass_moisture_code( double mc,  double PC,  double wind);
double Pign(double mc,  double wind2m, double Cint, double Cmc, double Cws);
double curing_factor(double PC);

void main(){
/* this is just a main program scrap to test the code making a simple table of MC by Precent cure  */

  double  gmc, per_cur;
  double   wind10m=15.0;
  gmc=2.0;
  printf("GMC  100   90   80   70   60   50   40   30   20   \n");
  while(gmc<35.0)
  {
      printf("%3.0f :",gmc);
      per_cur=100.0;
      while(per_cur>10)
      {
          printf("%4.1f ",grass_moisture_code(gmc,per_cur, wind10m) );
          per_cur-=10.0;
      }
      gmc+=1.0;
      printf("\n");
  }

}


double grass_moisture_code( double mc,  double cur,  double wind)
{
  /* THIS is the way to get the CODE value from cured grassland moisture
    IT takes fully cured grass moisture  (from the grass moisture model (100% cured)  (from the FMS...updated version of Wotton 2009)
       and a estimate of the fuel complex curing (as percent cured)
       and estimated wind speed (necessary for a calc
    and calculated the probability of sustainable flaming ignition  (a funciton of MC  and wind)
    THEN it accounts for curing effect on probability of fire spread sustainability, using the curing factor from Cruz et al (2015) for grass
    THEN from this calcuates an 'effective moisture content' which is the moisture content that would be required to achieve
       the curing adjusted probabiltiy of sustained flaming if one were calcuating it directly through the standard Pign equation.
    and THEN converts this effective moisture content to a CODE value via the FF-scale the FFMC uses for consistency

    relies on models of:
       Prob of sustained flaming for grass model (PsusF(grass)
       and  the curing_factor  function
       AND and estiamte of open 10 m to 2 m wind reduction (0.75)...hardcoded in here now.....

MC is moisture content (%)
cur=percent curing of the grassland  (%)
wind=  10 m open wind (km/h)

    currently (NOv 2023) the coefficients for the PsusF(grass) models are hardcoded into the GFMC function
  */

    const double wind2m_open_factor=0.75;
    double probign,wind2m,newPign, egmc;

    const double  Intercept=1.49;
    const double  Cmoisture=-0.11;
    const double  Cwind=0.075;
    /* GRASS: these coefficients (above) could change down the road .....explicitly coded in above*/

    wind2m=wind2m_open_factor * wind;   /* convert from 10 m wind in open to 2 m wind in open COULD be updated */

    probign = Pign(mc,wind2m, Intercept, Cmoisture, Cwind);

    /* adjust ignition diretctly with the curing function on ROS */
    newPign=curing_factor(cur)*probign;

    /* now to back calc effective moisture - algebraically reverse the Pign equation*/
    if (newPign>0.0)egmc= (log(newPign/(1.0-newPign))-Intercept-Cwind*wind2m)/Cmoisture;
    else egmc=250;  /* a saturation value just a check*/

    return 59.5*(250.0-egmc)/(147.2772773+egmc);   /*   convert to code with FF-scale */
 }


 double Pign(double mc,  double wind2m, double Cint, double Cmc, double Cws)
 /* Thisd is the general standard form for the probability of sustained flaming models for each FF cover type
    here :
      mc is cured moisture (%) in the litter fuels being ignited
      wind2m (km/h)  is the estimated 2 metre standard height for wind at hte site of the fire ignition
      Cint, Cmc and Cws   are coefficients for the standard Pign model form for a given FF cover type

      return >> is the Probability of Sustained flaming from a single small flaming ember/brand
 */
 {
     const double Prob = 1.0/(1.0 + exp(-1.0*(Cint + Cmc*mc + Cws*wind2m)));

     return Prob;
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

