# Computes hourly FWI indices for an input hourly weather stream
import datetime
import logging
import argparse
from math import exp, log, pow, sqrt

import pandas as pd

import util


logger = logging.getLogger("cffdrs")
logger.setLevel(logging.WARNING)

DAILY_K_DMC_DRYING = 1.894
DAILY_K_DC_DRYING = 3.937

HOURLY_K_DMC = 2.22
HOURLY_K_DC = 0.085
DMC_OFFSET_TEMP = 0.0
DC_OFFSET_TEMP = 0.0

DC_DAILY_CONST = 0.36
DC_HOURLY_CONST = DC_DAILY_CONST / DAILY_K_DC_DRYING

OFFSET_SUNRISE = 0 #2.5
OFFSET_SUNSET = 0 #0.5

# Fuel Load (kg/m^2)
DEFAULT_GRASS_FUEL_LOAD = 0.35

# default startup values
FFMC_DEFAULT = 85.0
DMC_DEFAULT = 6.0
DC_DEFAULT = 15.0

MPCT_TO_MC = 250.0 * 59.5 / 101.0
FFMC_INTERCEPT = 0.5
DMC_INTERCEPT = 1.5
DC_INTERCEPT = 2.8

DATE_GRASS = 181

##
# Convert to fine fuel moisture content (%)
# @param ffmc       Fine Fuel Moisture Code (FFMC)
# @return           fine fuel moisture content (%)
def ffmc_to_mcffmc(ffmc):
    return MPCT_TO_MC * (101 - ffmc) / (59.5 + ffmc)

##
# Convert to FFMC
# @param mcffmc     fine fuel moisture content (%)
# @return           FFMC
def mcffmc_to_ffmc(mcffmc):
    return 59.5 * (250 - mcffmc) / (MPCT_TO_MC + mcffmc)

##
# Convert to duff moisture content (%)
# @param dmc        Duff Moisture Code (DMC)
# @return           duff moisture content (%)
def dmc_to_mcdmc(dmc):
   return (280 / exp(dmc / 43.43)) + 20

##
# Convert to DMC
# @param mcdmc      duff moisture content (%)
# @return           DMC
def mcdmc_to_dmc(mcdmc):
   return 43.43 * log(280 / (mcdmc - 20))

##
# Convert to DC moisture content (%)
# @param dc         Drought Code (DC)
# @return           DC moisture content (%)
def dc_to_mcdc(dc):
   return 400 * exp(-dc / 400)

##
# Convert to DC
# @param mcdc       DC moisture content (%)
# @return           DC
def mcdc_to_dc(mcdc):
   return 400 * log(400 / mcdc)

##
# Calculate hourly fine fuel moisture content. Needs to be converted to get FFMC
#
# @param lastmc          Previous fine fuel moisture content (%)
# @param temp            Temperature (Celcius)
# @param rh              Relative Humidity (percent, 0-100)
# @param ws              Wind Speed (km/h)
# @param rain            Rainfall AFTER intercept (mm)
# @param time_increment  Duration of timestep (hr, default 1.0)
# @return                Hourly fine fuel moisture content (%)
def hourly_fine_fuel_moisture(lastmc, temp, rh, ws, rain, time_increment = 1.0):
    rf = 42.5
    drf = 0.0579
    # use moisture directly instead of converting to/from ffmc
    # expects any rain intercept to already be applied
    mo = lastmc
    if rain != 0.0:
        # duplicated in both formulas, so calculate once
        # lastmc == mo, but use lastmc since mo changes after first equation
        mo += rf * rain * exp(-100.0 / (251 - lastmc)) * (1.0 - exp(-6.93 / rain))
        if lastmc > 150:
            mo += 0.0015 * pow(lastmc - 150, 2) * sqrt(rain)
        if mo > 250.0: 
            mo = 250.0
    # duplicated in both formulas, so calculate once
    e1 = 0.18 * (21.1 - temp) * (1.0 - (1.0 / exp(0.115 * rh)))
    ed = 0.942 * pow(rh, 0.679) + (11.0 * exp((rh - 100) / 10.0)) + e1
    ew = 0.618 * pow(rh, 0.753) + (10.0 * exp((rh - 100) / 10.0)) + e1
    m = ew if (mo < ed) else ed
    if mo != ed:
        # these are the same formulas with a different value for a1
        a1 = (rh / 100.0) if (mo > ed) else ((100.0 - rh) / 100.0)
        k0_or_k1 = 0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(ws) * (1 - pow(a1, 8)))
        kd_or_kw = (1.0/0.50)*drf * k0_or_k1 * exp(0.0365 * temp)
        m += (mo - m) * pow(10, (-kd_or_kw * time_increment))
    return m

##
# Calculate duff moisture content
#
# @param last_mcdmc             Previous duff moisture content (%)
# @param hr                     Time of day (hr)
# @param temp                   Temperature (Celcius)
# @param rh                     Relative Humidity (%)
# @param prec                   Hourly precipitation (mm)
# @param sunrise                Sunrise (hr)
# @param sunset                 Sunset (hr)
# @param prec_cumulative_prev   Cumulative precipitation since start of rain (mm)
# @param time_increment         Duration of timestep (hr, default 1.0)
# @return                       Hourly duff moisture content (%)
def duff_moisture_code(
    last_mcdmc,
    hr,
    temp,
    rh,
    prec,
    sunrise,
    sunset,
    prec_cumulative_prev,
    time_increment = 1.0  # duration of timestep, in hours
):
    # wetting
    if prec_cumulative_prev + prec > DMC_INTERCEPT:  # prec_cumulative above threshold
        if prec_cumulative_prev < DMC_INTERCEPT:  # just passed threshold
            rw = (prec_cumulative_prev + prec) * 0.92 - 1.27
        else:  # previously passed threshold
            rw = prec * 0.92
        
        last_dmc = mcdmc_to_dmc(last_mcdmc)
        if last_dmc <= 33:
            b = 100.0 / (0.3 * last_dmc + 0.5)
        elif last_dmc <= 65:
            b = -1.3 * log(last_dmc) + 14.0
        else:
            b = 6.2 * log(last_dmc) - 17.2
        
        mr = last_mcdmc + (1e3 * rw) / (b * rw + 48.77)
    else:  # prec_cumulative below threshold
        mr = last_mcdmc
    
    if mr > 300.0:
        mr = 300.0
    
    # drying
    sunrise_start = sunrise + OFFSET_SUNRISE
    sunset_start = sunset + OFFSET_SUNSET
    if (sunrise_start <= hr <= sunset_start):  # daytime
        if temp < 0:
            temp = 0.0
        rk = HOURLY_K_DMC * 1e-4 * (temp + DMC_OFFSET_TEMP) * (100.0 - rh)
        invtau = rk / 43.43
        mcdmc = (mr - 20.0) * exp(-time_increment * invtau) + 20.0
    else:  # nighttime
        mcdmc = mr
    
    if mcdmc > 300.0:
        mcdmc = 300.0
    
    return(mcdmc)

##
# Calculate drought code moisture content
#
# @param last_mcdc              Previous drought code moisture content (%)
# @param hr                     Time of day (hr)
# @param temp                   Temperature (Celcius)
# @param prec                   Hourly precipitation (mm)
# @param sunrise                Sunrise (hr)
# @param sunset                 Sunset (hr)
# @param prec_cumulative_prev   Cumulative precipitation since start of rain (mm)
# @param time_increment         Duration of timestep (hr, default 1.0)
# @return                       Hourly drought code moisture content (%)
def drought_code(
    last_mcdc,
    hr,
    temp,
    prec,
    sunrise,
    sunset,
    prec_cumulative_prev,
    time_increment = 1.0):
    # wetting
    if prec_cumulative_prev + prec > DC_INTERCEPT:  # prec_cumulative above threshold
        if prec_cumulative_prev <= DC_INTERCEPT:  # just passed threshold
            rw = (prec_cumulative_prev + prec) * 0.83 - 1.27
        else:  # previously passed threshold
            rw = prec * 0.83
        mr = last_mcdc + 3.937 * rw / 2.0
    else:
        mr = last_mcdc
    
    if mr > 400.0:
        mr = 400.0
    
    # drying
    sunrise_start = sunrise + OFFSET_SUNRISE
    sunset_start = sunset + OFFSET_SUNSET
    if (sunrise_start <= hr <= sunset_start):  # daytime
        offset = 3.0
        mult = 0.015
        if temp > 0:
            pe = mult * temp + offset / 16.0
        else:
            pe = 0
        invtau = pe / 400.0
        mcdc = mr * exp(-time_increment * invtau)
    else:  # nighttime
        mcdc = mr

    if mcdc > 400.0:
        mcdc = 400.0
    
    return(mcdc)

##
# Calculate Initial Spread Index (ISI)
#
# @param wind            Wind Speed (km/h)
# @param ffmc            Fine Fuel Moisure Code
# @return                Initial Spread Index
def initial_spread_index(ws, ffmc):
    fm = ffmc_to_mcffmc(ffmc)
    fw = (12 * (1 - exp(-0.0818 * (ws - 28)))) if (40 <= ws) else exp(0.05039 * ws)
    ff = 91.9 * exp(-0.1386 * fm) * (1.0 + fm**5.31 / 4.93e07)
    isi = 0.208 * fw * ff
    return isi

##
# Calculate Build-up Index (BUI)
#
# @param dmc             Duff Moisture Code
# @param dc              Drought Code
# @return                Build-up Index
def buildup_index(dmc, dc):
    bui = 0.0 if (0 == dmc and 0 == dc) else (0.8 * dc * dmc / (dmc + 0.4 * dc))
    if bui < dmc:
        p = (dmc - bui) / dmc
        cc = 0.92 + pow(0.0114 * dmc, 1.7)
        bui = dmc - cc * p
        if bui <= 0:
            bui = 0.0
    return bui

##
# Calculate Fire Weather Index (FWI)
#
# @param isi             Initial Spread Index
# @param bui             Build-up Index
# @return                Fire Weather Index
def fire_weather_index(isi, bui):
    bb = (
        0.1
        * isi
        * (
            ((1000 / (25 + 108.64 / exp(0.023 * bui))))
            if bui > 80
            else ((0.626 * pow(bui, 0.809) + 2))
        )
    )
    fwi = bb if bb <= 1 else exp(2.72 * pow(0.434 * log(bb), 0.647))
    return fwi


def daily_severity_rating(fwi):
    return 0.0272 * pow(fwi, 1.77)

##
# Calculate hourly grassland fuel moisture content. Needs to be converted to get GFMC.
#
# @param lastmc          Previous grassland fuel moisture content (percent)
# @param temp            Temperature (Celcius)
# @param rh              Relative Humidity (percent, 0-100)
# @param ws              Wind Speed (km/h)
# @param rain            Rainfall (mm)
# @param solrad          Solar radiation (kW/m^2)
# @param load            Grassland Fuel Load (kg/m^2)
# @param time_increment  Duration of timestep (hr, default 1.0)
# @return                Grassland fuel moisture content (percent)
def hourly_grass_fuel_moisture(
    lastmc,
    temp,
    rh,
    ws,
    rain,
    solrad,
    load,
    time_increment = 1.0):

    rf = 0.27
    drf = 0.389633
    # use moisture directly instead of converting to/from ffmc
    # expects any rain intercept to already be applied
    mo = lastmc
    if rain != 0.0:
        mo += rain / load * 100.0
        if mo > 250:
            mo = 250.0
    # fuel temp from CEVW
    tf = temp + 17.9 * solrad * exp(-0.034 * ws)
    # fuel humidity
    if tf > temp:
        rhf = (rh * 6.107 * pow(10.0, 7.5 * temp / (temp + 237.0)) /
            (6.107 * pow(10.0, 7.5 * tf / (tf + 237.0))))
    else:
        rhf = rh
    # 18.85749879,18.85749879,7.77659602,21.24361786,19.22479551,19.22479551
    # duplicated in both formulas, so calculate once
    e1 = rf * (26.7 - tf) * (1.0 - (1.0 / exp(0.115 * rhf)))
    # GRASS EMC
    ed = 1.62 * pow(rhf, 0.532) + (13.7 * exp((rhf - 100) / 13.0)) + e1
    ew = 1.42 * pow(rhf, 0.512) + (12.0 * exp((rhf - 100) / 18.0)) + e1
    
    moed = mo - ed
    moew = mo - ew
    
    e = None
    a1 = None
    m = None
    moe = None
    
    if (moed == 0) or (moew >= 0 and moed < 0):
        m = mo
        if (moed == 0):
            e = ed
        if moew >= 0:
            e = ew
    else:
        if moed > 0:
            a1 = rhf / 100.0
            e = ed
            moe = moed
        else:
            a1 = (100.0 - rhf) / 100.0
            e = ew
            moe = moew
        if (a1 < 0):
            # avoids complex number in a1^1.7 xkd calculation
            a1 = 0
        xkd = (0.424 * (1 - a1 ** 1.7) + (0.0694 * sqrt(ws) * (1 - a1 ** 8)))
        xkd = xkd * drf * exp(0.0365 * tf)
        m = e + moe * exp(-1.0 * log(10.0) * xkd * time_increment)
    return m

def Pign(mc, wind2m, Cint, Cmc, Cws):
    #  Thisd is the general standard form for the probability of sustained flaming models for each FF cover type
    #     here :
    #       mc is cured moisture (%) in the litter fuels being ignited
    #       wind2m (km/h)  is the estimated 2 metre standard height for wind at hte site of the fire ignition
    #       Cint, Cmc and Cws   are coefficients for the standard Pign model form for a given FF cover type

    #       return >> is the Probability of Sustained flaming from a single small flaming ember/brand
    Prob = 1.0 / (1.0 + exp(-1.0 * (Cint + Cmc * mc + Cws * wind2m)))
    return Prob

def curing_factor(cur):
    # cur is the percentage cure of the grass fuel complex.  100= fully cured
    #   ....The OPPOSITE (100-x) of greenness...

    #    This is the Cruz et al (2015) model with the original precision of the coefficent estimates
    #    and as in CSIRO code:https://research.csiro.au/spark/resources/model-library/csiro-grassland-models/
    cf = (1.036 / (1 + 103.989 * exp(-0.0996 * (cur - 20)))) if (cur >= 20.0) else 0.0
    return cf

def mcgfmc_to_gfmc(mc, cur, wind):
    #   THIS is the way to get the CODE value from cured grassland moisture
    #     IT takes fully cured grass moisture  (from the grass moisture model (100% cured)  (from the FMS...updated version of Wotton 2009)
    #        and a estimate of the fuel complex curing (as percent cured)
    #        and estimated wind speed (necessary for a calc
    #     and calculated the probability of sustainable flaming ignition  (a funciton of MC  and wind)
    #     THEN it accounts for curing effect on probability of fire spread sustainability, using the curing factor from Cruz et al (2015) for grass
    #     THEN from this calcuates an 'effective moisture content' which is the moisture content that would be required to achieve
    #        the curing adjusted probabiltiy of sustained flaming if one were calcuating it directly through the standard Pign equation.
    #     and THEN converts this effective moisture content to a CODE value via the FF-scale the FFMC uses for consistency

    #     relies on models of:
    #        Prob of sustained flaming for grass model (PsusF(grass)
    #        and  the curing_factor  function
    #        AND and estiamte of open 10 m to 2 m wind reduction (0.75)...hardcoded in here now.....

    # MC is moisture content (%)
    # cur=percent curing of the grassland  (%)
    # wind=  10 m open wind (km/h)

    #     currently (NOv 2023) the coefficients for the PsusF(grass) models are hardcoded into the GFMC function

    wind2m_open_factor = 0.75

    Intercept = 1.49
    Cmoisture = -0.11
    Cwind = 0.075
    # GRASS: these coefficients (above) could change down the road .....explicitly coded in above*/
    # /* convert from 10 m wind in open to 2 m wind in open COULD be updated */
    wind2m = wind2m_open_factor * wind

    probign = Pign(mc, wind2m, Intercept, Cmoisture, Cwind)

    # /* adjust ignition diretctly with the curing function on ROS */
    newPign = curing_factor(cur) * probign

    # /* now to back calc effective moisture - algebraically reverse the Pign equation*/
    # /* 250 is a saturation value just a check*/
    egmc = (
        ((log(newPign / (1.0 - newPign)) - Intercept - Cwind * wind2m) / Cmoisture)
        if (newPign > 0.0)
        else 250
    )
    # /*   convert to code with FF-scale */
    # return (59.5*(250.0-egmc)/(MPCT_TO_MC + egmc))
    if egmc > 250.0:
      egmc = 250.0
    return mcffmc_to_ffmc(egmc)

def matted_grass_spread_ROS(ws, mc, cur):
    #  /*  CUT grass  Rate  of spread from cheney 1998  (and new CSIRO grassland code
    #   We use this for MATTED grass in our post-winter context
    #   --ws=10 m open wind km/h
    #   --mc = moisture content in  cured grass  (%)
    #   --cur = percentage of grassland cured  (%)
    #   output should be ROS in m/min   */
    fw = 16.67 * (
        (0.054 + 0.209 * ws) if (ws < 5) else (1.1 + 0.715 * (ws - 5.0) ** 0.844)
    )
    fm = (
        exp(-0.108 * mc)
        if mc < 12
        else (
            0.6838 - 0.0342 * mc
            if (mc < 20.0 and ws < 10.0)
            else 0.547 - 0.0228 * mc
            if (mc < 23.9 and ws >= 10.0)
            else 0.0
        )
    )
    if (fm < 0):
      fm = 0.0
    cf = curing_factor(cur)
    return fw * fm * cf

def standing_grass_spread_ROS(ws, mc, cur):
    #  /*  standing grass  Rate  of spread from cheney 1998  (and new CSIRO grassland code)
    #   We use this for standing grass in our post-winter context
    #   ITS only the WIND function that chnges here between cut and standing
    #   --ws=10 m open wind km/h
    #   --mc = moisture content in grass  (%)
    #   --cur = percentage of grassland cured  (%)
    #   output should be ROS in m/min   */
    fw = 16.67 * (
        (0.054 + 0.269 * ws) if (ws < 5) else (1.4 + 0.838 * (ws - 5.0) ** 0.844)
    )
    fm = (
        exp(-0.108 * mc)
        if mc < 12
        else (
            0.6838 - 0.0342 * mc
            if (mc < 20.0 and ws < 10.0)
            else 0.547 - 0.0228 * mc
            if (mc < 23.9 and ws >= 10.0)
            else 0.0
        )
    )
    if (fm < 0):
      fm = 0.0
    cf = curing_factor(cur)
    return fw * fm * cf

##
# Calculate Grassland Spread Index (GSI)
#
# @param ws              Wind Speed (km/h)
# @param mc              Grass moisture content (percent)
# @param cur             Degree of curing (percent, 0-100)
# @param standing        Grass standing (True/False)
# @return                Grassland Spread Index
def grass_spread_index(ws, mc, cur, standing):
    #  So we don't have to transition midseason between standing and matted grass spread rate models
    #  We will simply scale   GSI   by the average of the   matted and standing spread rates
    
    #now allowing switch between standing and matted grass
    ros = None
    if (standing):
      #standing
      ros = standing_grass_spread_ROS(ws, mc, cur)
    
    else:
      #matted
      ros = matted_grass_spread_ROS(ws, mc, cur)
    
    
    return 1.11 * ros

##
# Calculate Grassland Fire Weather Index
#
# @param gsi               Grassland Spread Index
# @param load              Grassland Fuel Load (kg/m^2)
# @return                  Grassland Fire Weather Index
def grass_fire_weather_index(gsi, load):
    # this just converts back to ROS in m/min
    ros = gsi / 1.11
    Fint = 300.0 * load * ros
    if Fint > 100:
        return(log(Fint / 60.0) / 0.14)
    else:
        return(Fint / 25.0)

# Calculate number of drying "units" this hour contributes
def drying_units():  # temp, rh, ws, rain, solrad
    # for now, just add 1 drying "unit" per hour
    return 1.0

def rain_since_intercept_reset(rain, canopy):
    # for now, want 5 "units" of drying (which is 1 per hour to start)
    TARGET_DRYING_SINCE_INTERCEPT = 5.0
    if rain > 0 or canopy["rain_total_prev"] == 0:  # if raining, reset drying
        canopy["drying_since_intercept"] = 0.0
    else:
        canopy["drying_since_intercept"] += drying_units()
        if canopy["drying_since_intercept"] >= TARGET_DRYING_SINCE_INTERCEPT:
            # reset rain if intercept reset criteria met
            canopy["rain_total_prev"] = 0.0
            canopy["drying_since_intercept"] = 0.0
    return canopy

##
# Calculate hourly FWI indices from hourly weather stream for a single station
#
# @param    w                   hourly values weather stream
# @param    ffmc_old            previous value FFMC (this or mcffmc_old should be None)
# @param    mcffmc_old          previous value mcffmc (this or ffmc_old should be None)
# @param    dmc_old             previous value for DMC
# @param    dc_old              previous value for DC
# @param    mcgfmc_matted_old   previous value for matted mcgfmc
# @param    mcgfmc_standing_old previous value for standing mcgfmc
# @param    prec_cumulative     cumulative precipitation this rainfall
# @param    canopy_drying       consecutive hours of no rain
# @return                       hourly values FWI and weather stream
def _stnHFWI(
    w,
    ffmc_old,
    mcffmc_old,
    dmc_old,
    dc_old,
    mcgfmc_matted_old,
    mcgfmc_standing_old,
    prec_cumulative,
    canopy_drying):
    if not util.is_sequential_hours(w):
        raise RuntimeError("Expected hourly weather input to be sequential")
    if len(w["id"].unique()) != 1:
        raise RuntimeError("_stnHFWI() function only accepts a single station ID")
    if len(w["lat"].unique()) != 1:
        raise RuntimeError("Expected a single latitude (lat) each station year")
    if len(w["long"].unique()) != 1:
        raise RuntimeError("Expected a single longitude (long) each station year")
    if len(w["timezone"].unique()) != 1:
        raise RuntimeError("Expected a single UTC offset (timezone) each station year")
    if len(w["grass_fuel_load"].unique()) != 1:
        raise RuntimeError("Expected a single grass_fuel_load value each station year")
    r = w.loc[:]
    if mcffmc_old == None or mcffmc_old == "None":
        if ffmc_old == None or ffmc_old == "None":
            raise ValueError("Either ffmc_old OR mcffmc_old should be NA, not both")
        else:
            mcffmc = ffmc_to_mcffmc(ffmc_old)
    else:
        if ffmc_old == None or ffmc_old == "None":
            mcffmc = mcffmc_old
        else:
            raise ValueError("One of ffmc_old OR mcffmc_old should be NA, not neither")
    mcgfmc_matted = mcgfmc_matted_old
    mcgfmc_standing = mcgfmc_standing_old
    mcdmc = dmc_to_mcdmc(dmc_old)
    mcdc = dc_to_mcdc(dc_old)
    # FIX: just use loop for now so it matches C code
    canopy = {"rain_total_prev": prec_cumulative,
        "drying_since_intercept": canopy_drying}
    results = []
    for i in range(len(r)):
        cur = r.iloc[i].to_dict()
        canopy = rain_since_intercept_reset(cur["prec"], canopy)
        # determine rain for ffmc and whether or not intercept should happen now
        if canopy["rain_total_prev"] + cur["prec"] <= FFMC_INTERCEPT:  # not enough rain
            rain_ffmc = 0.0
        elif canopy["rain_total_prev"] > FFMC_INTERCEPT:  # already saturated canopy
            rain_ffmc = cur["prec"]
        else:
            rain_ffmc = canopy["rain_total_prev"] + cur["prec"] - FFMC_INTERCEPT
        mcffmc = hourly_fine_fuel_moisture(
            mcffmc,
            cur["temp"],
            cur["rh"],
            cur["ws"],
            rain_ffmc
        )
        cur["mcffmc"] = mcffmc
        # convert to code for output, but keep using moisture % for precision
        cur["ffmc"] = mcffmc_to_ffmc(mcffmc)
        # not ideal, but at least encapsulates the code for each index
        mcdmc = duff_moisture_code(
            mcdmc,
            cur["hr"],
            cur["temp"],
            cur["rh"],
            cur["prec"],
            cur["sunrise"],
            cur["sunset"],
            canopy["rain_total_prev"]
        )
        cur["dmc"] = mcdmc_to_dmc(mcdmc)
        mcdc = drought_code(
            mcdc,
            cur["hr"],
            cur["temp"],
            cur["prec"],
            cur["sunrise"],
            cur["sunset"],
            canopy["rain_total_prev"]
        )
        cur["dc"] = mcdc_to_dc(mcdc)
        cur["isi"] = initial_spread_index(cur["ws"], cur["ffmc"])
        cur["bui"] = buildup_index(cur["dmc"], cur["dc"])
        cur["fwi"] = fire_weather_index(cur["isi"], cur["bui"])
        cur["dsr"] = daily_severity_rating(cur["fwi"])
        # done using canopy, can update for next step
        canopy["rain_total_prev"] += cur["prec"]
        # grass updates
        mcgfmc_matted = hourly_grass_fuel_moisture(
            mcgfmc_matted,
            cur["temp"],
            cur["rh"],
            cur["ws"],
            cur["prec"],
            cur["solrad"],
            cur["grass_fuel_load"]
        )
        #for standing grass we make a come very simplifying assumptions based on obs from the field (echo bay study):
        #standing not really affected by rain -- to introduce some effect we introduce just a simplification of the FFMC Rain absorption function
        #which averages 6% or so for rains  (<5mm...between 7% and 5%,    lower for larger rains)(NO intercept)
        #AND the solar radiation exposure is less, and the cooling from the wind is stronger.  SO we assume there is effectively no extra
        #heating of the grass from solar
        #working at the margin like this should make a nice bracket for moisture between the matted and standing that users can use
        #...reality will be in between the matt and stand
        mcgfmc_standing = hourly_grass_fuel_moisture(
            mcgfmc_standing,
            cur["temp"],
            cur["rh"],
            cur["ws"],
            cur["prec"] * 0.06,
            0.0,
            cur["grass_fuel_load"]
        )        
        
        if (util.julian(cur["mon"], cur["day"]) < DATE_GRASS):
            standing = False
            mcgfmc = mcgfmc_matted
        else:
            standing = True
            mcgfmc = mcgfmc_standing
        
        cur["mcgfmc_matted"] = mcgfmc_matted
        cur["mcgfmc_standing"] = mcgfmc_standing
        cur["gfmc"] = mcgfmc_to_gfmc(mcgfmc, cur["percent_cured"], cur["ws"])
        cur["gsi"] = grass_spread_index(cur["ws"], mcgfmc, cur["percent_cured"], standing)
        cur["gfwi"] = grass_fire_weather_index(cur["gsi"], cur["grass_fuel_load"])
        # save wetting variables for timestep-by-timestep runs
        cur["prec_cumulative"] = canopy["rain_total_prev"]
        cur["canopy_drying"] = canopy["drying_since_intercept"]
        # append results for this row
        results.append(cur)
    r = pd.DataFrame(results)
    del r["index"]
    return r

##
# Calculate hourly FWI indices from hourly weather stream.
#
# @param    df_wx               hourly values weather stream
# @param    ffmc_old            previous value for FFMC (startup 85, None for mcffmc_old)
# @param    mcffmc_old          previous value mcffmc (default None for ffmc_old input)
# @param    dmc_old             previous value for DMC (startup 6)
# @param    dc_old              previous value for DC (startup 15)
# @param    mcgfmc_matted_old   previous value for matted mcgfmc (startup FFMC = 85)
# @param    mcgfmc_standing_old previous value for standing mcgfmc (startup FFMC = 85)
# @param    prec_cumulative     cumulative precipitation this rainfall (default 0)
# @param    canopy_drying       consecutive hours of no rain (default 0)
# @param    silent              suppresses informative print statements (default False)
# @param    round_out           decimals to truncate output to, None for none (default 4)
# @return                       hourly values FWI and weather stream
def hFWI(
    df_wx,
    ffmc_old = FFMC_DEFAULT,
    mcffmc_old = None,
    dmc_old = DMC_DEFAULT,
    dc_old = DC_DEFAULT,
    mcgfmc_matted_old = ffmc_to_mcffmc(FFMC_DEFAULT),
    mcgfmc_standing_old = ffmc_to_mcffmc(FFMC_DEFAULT),
    prec_cumulative = 0.0,
    canopy_drying = 0.0,
    silent = False,
    round_out = 4):
    wx = df_wx.copy()
    # make all column names lower case
    wx.columns = map(str.lower, wx.columns)
    og_names = wx.columns
    # check for required columns
    if not all(x in wx.columns for x in 
        ['lat', 'long', 'timezone', 'yr', 'mon', 'day', 'hr',
        'temp', 'rh', 'ws', 'prec']):
        raise RuntimeError("Missing required input column(s)")
    # check for one hour run and startup moisture all set to default
    if (df_wx.shape[0] == 1 and
        ffmc_old == FFMC_DEFAULT and mcffmc_old == None and
        dmc_old == DMC_DEFAULT and dc_old == DC_DEFAULT and
        mcgfmc_matted_old == ffmc_to_mcffmc(FFMC_DEFAULT) and
        mcgfmc_standing_old == ffmc_to_mcffmc(FFMC_DEFAULT)):
        logger.warning("Warning:\nStartup moisture values set to default" +
            " (instead of previous) in a one hour run")
    # check for optional columns that have a default
    had_stn = "id" in og_names
    had_minute = "minute" in og_names
    if not had_stn:
        wx["id"] = "STN"
    if not had_minute:
        wx["minute"] = 0
    # check for optional columns that can be calculated
    had_date = "date" in og_names
    had_timestamp = "timestamp" in og_names
    if not had_date:
        wx["date"] = wx.apply(
            lambda row: f'{row["yr"]:04d}-{row["mon"]:02d}-{row["day"]:02d}', axis=1
        )
    if not had_timestamp:
        wx["timestamp"] = wx.apply(
            lambda row: datetime.datetime(
                row["yr"], row["mon"], row["day"], row["hr"], row["minute"]
                ), axis=1
            )
    if not "grass_fuel_load" in og_names:
        wx["grass_fuel_load"] = DEFAULT_GRASS_FUEL_LOAD
    if not "percent_cured" in og_names:
        wx["percent_cured"] = wx.apply(lambda row: util.seasonal_curing(
            util.julian(row["mon"], row["day"])), axis=1)
    if not "solrad" in wx.columns:
        if not silent:
            print("Solar Radiation not provided so will be calculated")
        needs_solrad = True
    else:
        needs_solrad = False
    # check for values outside valid ranges
    if any(isinstance(tz, str) for tz in wx["timezone"]):
        raise ValueError("UTC offset (timezone) should be a number, not a string")
    if not (all(wx["rh"] >= 0) and all(wx["rh"] <= 100)):
        raise ValueError("All relative humidity (rh) must be between 0-100%")
    if not all(wx["ws"] >= 0):
        raise ValueError("All wind speed (ws) must be >= 0")
    if not all(wx["prec"] >= 0):
        raise ValueError("All precipitation (prec) must be >= 0")
    if not (all(wx["mon"] >= 1) and all(wx["mon"] <= 12)):
        raise ValueError("All months (mon) must be between 1-12")
    if (not needs_solrad) and (not all(wx['solrad'] >= 0)):
        raise ValueError("All solar radiation (solrad) must be >= 0")
    if ("percent_cured" in og_names) and (not (
        all(wx["percent_cured"] >= 0) and all(wx["percent_cured"] <= 100))):
        raise ValueError("All percent_cured must be between 0-100%")
    if ("grass_fuel_load" in og_names) and (not (all(wx["grass_fuel_load"] > 0))):
        raise ValueError("All grass_fuel_load must be > 0")
    if not (all(wx["day"] >= 1) and all(wx["day"] <= 31)):
        raise ValueError("All day must be 1-31")
    if mcffmc_old == None or mcffmc_old == "None":
        if ffmc_old == None or ffmc_old == "None":
            raise ValueError("Either ffmc_old OR mcffmc_old should be None, not both")
        elif not (0 <= ffmc_old <= 101):
            raise ValueError("ffmc_old must be between 0-101")
    else:
        if ffmc_old == None or ffmc_old == "None":
            if not (0 <= mcffmc_old <= 250):
                raise ValueError("mcffmc_old must be between 0-250%")
        else:
            raise ValueError("One of ffmc_old OR mcffmc_old should be None, not neither")
    if not (dmc_old >= 0):
        raise ValueError("dmc_old must be >= 0")
    if not (dc_old >= 0):
        raise ValueError("dc_old must be >= 0")
    
    # loop over every station year
    results = None
    for idx, by_year in wx.groupby(["id", "yr"], sort = False):
        if not silent:
            print("Running " + str(idx[0]) + " for " + str(idx[1]))
        logger.debug(f"Running for {idx}")
        w = by_year.reset_index()
        w = util.get_sunlight(w, get_solrad = needs_solrad)
        r = _stnHFWI(w, ffmc_old, mcffmc_old, dmc_old, dc_old,
            mcgfmc_matted_old, mcgfmc_standing_old,
            prec_cumulative, canopy_drying)
        results = pd.concat([results, r])
    
    # remove optional variables that we added
    if not had_stn:
        results = results.drop(columns = "id")
    if not had_minute:
        results = results.drop(columns = "minute")
    if not had_date:
        results = results.drop(columns = "date")
    if not had_timestamp:
        results = results.drop(columns = "timestamp")

    # round decimal places of output columns
    if not (round_out == None or round_out == "None"):
        outcols = ["sunrise", "sunset", "sunlight_hours",
            "mcffmc", "ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr",
            "mcgfmc_matted", "mcgfmc_standing", "gfmc", "gsi", "gfwi",
            "prec_cumulative", "canopy_drying"]
        if "solrad" not in og_names:
            outcols.insert(0, "solrad")
        results[outcols] = results[outcols].map(round, ndigits = round_out)

    return results

if __name__ == "__main__":
    # run hFWI by command line. run with option -h or --help to see usage
    parser = argparse.ArgumentParser(prog = "NG_FWI")
    # add all inputs to hFWI
    parser.add_argument("input", help = "Input csv data file")
    parser.add_argument("output", help = "Output csv file name and location")
    parser.add_argument("ffmc_old", nargs = "?", default = FFMC_DEFAULT,
        help = "Starting value for FFMC (startup 85, None for mcffmc_old)")
    parser.add_argument("mcffmc_old", nargs = "?", default = None,
        help = "Starting value for mcffmc (default None for ffmc_old input)")
    parser.add_argument("dmc_old", nargs = "?", default = DMC_DEFAULT, type = float,
        help = "Starting DMC (default 6)")
    parser.add_argument("dc_old", nargs = "?", default = DC_DEFAULT, type = float,
        help = "Starting DC (default 15)")
    parser.add_argument("mcgfmc_matted_old", nargs = "?",
        default = ffmc_to_mcffmc(FFMC_DEFAULT), type = float,
        help = "Starting mcgfmc for matted fuels (default mcffmc when FFMC = 85)")
    parser.add_argument("mcgfmc_standing_old", nargs = "?",
        default = ffmc_to_mcffmc(FFMC_DEFAULT), type = float,
        help = "Starting mcgfmc for standing fuels (default mcffmc when FFMC = 85)")
    parser.add_argument("prec_cumulative", nargs = "?", default = 0.0, type = float,
        help = "Cumulative precipitation of rain event (default 0)")
    parser.add_argument("canopy_drying", nargs = "?", default = 0.0, type = float,
        help = "Canopy drying, or consecutive hours of no prec (default 0)")
    parser.add_argument("-s", "--silent", action = "store_true")
    parser.add_argument("-r", "--round_out", default = 4, nargs = "?",
        help = "Decimal places to truncate outputs to, None for no rounding (default 4)")

    args = parser.parse_args()
    df_in = pd.read_csv(args.input)
    df_out = hFWI(df_in, args.ffmc_old, args.mcffmc_old,
        args.dmc_old, args.dc_old, args.mcgfmc_matted_old, args.mcgfmc_standing_old,
        args.prec_cumulative, args.canopy_drying, args.silent, args.round_out)
    df_out.to_csv(args.output, index = False)
