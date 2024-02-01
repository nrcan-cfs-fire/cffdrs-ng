import datetime
import logging
import os.path
import sys
from math import exp, log, pow, sqrt

import pandas as pd

import util
from old_cffdrs import daily_drought_code, daily_duff_moisture_code
from util import save_csv

logger = logging.getLogger("cffdrs")
logger.setLevel(logging.WARNING)

# HOUR_TO_START_FROM = 12

HOURLY_K_DMC = 2.10
HOURLY_K_DC = 0.017
DMC_OFFSET_TEMP = 1.1
DC_OFFSET_TEMP = 0.0

OFFSET_SUNRISE = 2.5
OFFSET_SUNSET = 0.5

# Fuel Load (kg/m^2)
DEFAULT_GRASS_FUEL_LOAD = 0.35
MAX_SOLAR_PROPAGATION = 0.85

# default startup values
FFMC_DEFAULT = 85
DMC_DEFAULT = 6
DC_DEFAULT = 15

# FIX: figure out what this should be
DEFAULT_LATITUDE = 55.0
DEFAULT_LONGITUDE = -120.0

MPCT_TO_MC = 147.2772277
FFMC_INTERCEPT = 0.5
DMC_INTERCEPT = 1.5
DC_INTERCEPT = 2.8


# Fine Fuel Moisture Code (FFMC) from moisture %
def fine_fuel_moisture_code(moisture_percent):
    return 59.5 * (250 - moisture_percent) / (MPCT_TO_MC + moisture_percent)


# Fine Fuel Moisture (%) from FFMC
def fine_fuel_moisture_from_code(moisture_code):
    return MPCT_TO_MC * (101 - moisture_code) / (59.5 + moisture_code)


# Calculate hourly Fine Fuel Moisture Code (FFMC)
#
# @param temp            Temperature (Celcius)
# @param rh              Relative Humidity (percent, 0-100)
# @param ws              Wind Speed (km/h)
# @param rain            Rainfall (mm)
# @param lastmc          Previous Fine Fuel Moisture (%)
# @return                Hourly Fine Fuel Moisture (%)
def hourly_fine_fuel_moisture(temp, rh, ws, rain, lastmc):
    rf = 42.5
    drf = 0.0579
    # Time since last observation (hours)
    time = 1.0
    # use moisture directly instead of converting to/from ffmc
    # expects any rain intercept to already be applied
    mo = lastmc
    if rain != 0.0:
        # duplicated in both formulas, so calculate once
        # lastmc == mo, but use lastmc since mo changes after first equation
        mo += rf * rain * exp(-100.0 / (251 - lastmc)) * (1.0 - exp(-6.93 / rain))
        if lastmc > 150:
            mo += 0.0015 * pow(lastmc - 150, 2) * sqrt(rain)
        if mo > 250:
            mo = 250
    # duplicated in both formulas, so calculate once
    e1 = 0.18 * (21.1 - temp) * (1.0 - (1.0 / exp(0.115 * rh)))
    ed = 0.942 * pow(rh, 0.679) + (11.0 * exp((rh - 100) / 10.0)) + e1
    ew = 0.618 * pow(rh, 0.753) + (10.0 * exp((rh - 100) / 10.0)) + e1
    # m = ed if mo >= ed else ew
    m = ew if (mo < ed) else ed
    if mo != ed:
        # these are the same formulas with a different value for a1
        a1 = (rh / 100.0) if (mo > ed) else ((100.0 - rh) / 100.0)
        k0_or_k1 = 0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(ws) * (1 - pow(a1, 8)))
        kd_or_kw = drf * k0_or_k1 * exp(0.0365 * temp)
        m += (mo - m) * pow(10, (-kd_or_kw * time))
    return m


##
# Calculate Initial Spread Index (ISI)
#
# @param wind            Wind Speed (km/h)
# @param ffmc            Fine Fuel Moisure Code
# @return                Initial Spread Index
def initial_spread_index(ws, ffmc):
    fm = fine_fuel_moisture_from_code(ffmc)
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


# Calculate Hourly Grass Fuel Moisture. Needs to be converted to get GFMC.
#
# @param temp            Temperature (Celcius)
# @param rh              Relative Humidity (percent, 0-100)
# @param ws              Wind Speed (km/h)
# @param rain            Rainfall (mm)
# @param lastmc          Previous grass fuel moisture (percent)
# @param solrad          Solar radiation (kW/m^2)
# @return                Grass Fuel Moisture (percent)
def hourly_grass_fuel_moisture(temp, rh, ws, rain, solrad, lastmc):
    rf = 0.27
    drf = 0.389633
    # Time since last observation (hours)
    time = 1.0
    # use moisture directly instead of converting to/from ffmc
    # expects any rain intercept to already be applied
    mo = lastmc
    if rain != 0.0:
        #     mo+=rain*rf*exp(-100.0/(251.0-mo))*(1.0-exp(-6.93/rain))*/ # old routine*/
        # this new routine assumes layer is 0.3 kg/m2 so 0.3mm of rain adds +100%MC*/
        # *100 to convert to %...  *1/.3 because of 0.3mm=100%
        mo += rain / 0.3 * 100.0
        if mo > 250:
            mo = 250
    # fuel temp from CEVW*/
    tf = temp + 17.9 * solrad * exp(-0.034 * ws)
    # fuel humidity
    rhf = (
        (
            rh
            * 6.107
            * pow(10.0, 7.5 * temp / (temp + 237.0))
            / (6.107 * pow(10.0, 7.5 * tf / (tf + 237.0)))
        )
        if (tf > temp)
        else rh
    )
    # 18.85749879,18.85749879,7.77659602,21.24361786,19.22479551,19.22479551
    # duplicated in both formulas, so calculate once
    e1 = rf * (26.7 - tf) * (1.0 - (1.0 / exp(0.115 * rhf)))
    # GRASS EMC
    ed = 1.62 * pow(rhf, 0.532) + (13.7 * exp((rhf - 100) / 13.0)) + e1
    ew = 1.42 * pow(rhf, 0.512) + (12.0 * exp((rhf - 100) / 18.0)) + e1
    m = ew if (mo < ed and mo < ew) else ed
    if mo > ed or (mo < ed and mo < ew):
        # these are the same formulas with a different value for a1
        a1 = (rhf / 100.0) if (mo > ed) else ((100.0 - rhf) / 100.0)
        k0_or_k1 = 0.424 * (1 - pow(a1, 1.7)) + (0.0694 * sqrt(ws) * (1 - pow(a1, 8)))
        kd_or_kw = drf * k0_or_k1 * exp(0.0365 * tf)
        m += (mo - m) * pow(10, -kd_or_kw * time)
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


def grass_moisture_code(mc, cur, wind):
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
    return fine_fuel_moisture_code(egmc)


def matted_grass_spread_ROS(ws, mc, cur):
    #  /*  CUT grass  Rate  of spread from cheney 1998  (and new CSIRO grassland code
    #   We use this for MATTED grass in our post-winter context
    #   --ws=10 m open wind km/h
    #   --mc = moisture content in  cured grass  (%)
    #   --cur = percentage of grassland cured  (%)
    #   output should be ROS in m/min   */
    fw = 16.67 * (
        (0.054 + 0.209 * ws) if (ws < 5) else (1.1 + 0.715 * (ws - 5.0) * 0.844)
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
        (0.054 + 0.269 * ws) if (ws < 5) else (1.4 + 0.838 * (ws - 5.0) * 0.844)
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
    cf = curing_factor(cur)
    return fw * fm * cf


#' Calculate Grass Spread Index (GSI)
#'
#' @param ws              Wind Speed (km/h)
#' @param mc              Grass moisture content (percent)
#' @param cur             Degree of curing (percent, 0-100)
#' @return                Grass Spread Index
def grass_spread_index(ws, mc, cur):
    #  So we don't have to transition midseason between standing and matted grass spread rate models
    #  We will simply scale   GSI   by the average of the   matted and standing spread rates
    ros = (
        matted_grass_spread_ROS(ws, mc, cur) + standing_grass_spread_ROS(ws, mc, cur)
    ) / 2.0
    return 1.11 * ros


##
# Calculate Grass Fire Weather Index
#
# @param gsi               Grass Spread Index
# @param load              Fuel Load (kg/m^2)
# @return                  Grass Fire Weather Index
def grass_fire_weather_index(gsi, load):
    # this just converts back to ROS in m/min
    ros = gsi / 1.11
    Fint = 300.0 * load * ros
    return (log(Fint / 60.0) / 0.14) if Fint > 100 else (Fint / 25.0)


def dmc_wetting(rain_total, lastdmc):
    # compare floats by using tolerance
    if rain_total <= DMC_INTERCEPT:
        return 0.0
    b = (
        100.0 / (0.5 + 0.3 * lastdmc)
        if (lastdmc <= 33)
        else (
            14.0 - 1.3 * log(lastdmc)
            if (lastdmc <= 65)
            else (6.2 * log(lastdmc) - 17.2)
        )
    )
    rw = 0.92 * rain_total - 1.27
    wmi = 20 + 280 / exp(0.023 * lastdmc)
    # This is the change in MC (moisturecontent)  from FULL DAY's rain
    wmr = wmi + 1000 * rw / (48.77 + b * rw)
    dmc = 43.43 * (5.6348 - log(wmr - 20))
    if dmc <= 0.0:
        dmc = 0.0
    # total amount of wetting since lastdmc
    w = lastdmc - dmc
    return w


def dc_wetting(rain_total, lastdc):
    # compare floats by using tolerance
    if rain_total <= DC_INTERCEPT:
        return 0.0
    rw = 0.83 * rain_total - 1.27
    smi = 800 * exp(-lastdc / 400)
    # TOTAL change for the TOTAL 24 hour rain from FWI1970 model
    return 400.0 * log(1.0 + 3.937 * rw / smi)


def dmc_wetting_between(rain_total_previous, rain_total, lastdmc):
    if rain_total_previous >= rain_total:
        return 0.0
    # wetting is calculated based on initial dmc when rain started and rain since
    current = dmc_wetting(rain_total, lastdmc)
    # recalculate instead of storing so we don't need to reset this too
    # NOTE: rain_total_previous != (rain_total - cur["prec"]) due to floating point math
    previous = dmc_wetting(rain_total_previous, lastdmc)
    return current - previous


def dc_wetting_between(rain_total_previous, rain_total, lastdc):
    if rain_total_previous >= rain_total:
        return 0.0
    # wetting is calculated based on initial dc when rain started and rain since
    current = dc_wetting(rain_total, lastdc)
    # recalculate instead of storing so we don't need to reset this too
    # NOTE: rain_total_previous != (rain_total - cur["prec"]) due to floating point math
    previous = dc_wetting(rain_total_previous, lastdc)
    return current - previous


def dmc_drying_ratio(temp, rh):
    return max(0.0, (temp + 1.1) * (100.0 - rh) * 0.0001)


def duff_moisture_code(
    last_dmc,
    temp,
    rh,
    ws,
    rain,
    mon,
    hour,
    solrad,
    sunrise,
    sunset,
    dmc_before_rain,
    rain_total_prev,
    rain_total,
):
    if 0 == rain_total:
        dmc_before_rain = last_dmc
    # apply wetting since last period
    dmc_wetting_hourly = dmc_wetting_between(
        rain_total_prev, rain_total, dmc_before_rain
    )
    assert 0 <= dmc_wetting_hourly
    # at most apply same wetting as current value (don't go below 0)
    dmc = max(0.0, last_dmc - dmc_wetting_hourly)
    sunrise_start = round(sunrise + OFFSET_SUNRISE)
    sunset_start = round(sunset + OFFSET_SUNSET)
    dmc_hourly = (
        HOURLY_K_DMC * dmc_drying_ratio(temp, rh)
        if (hour >= sunrise_start and hour < sunset_start)
        else 0.0
    )
    dmc = dmc + dmc_hourly
    # HACK: return two values since C uses a pointer to assign a value
    return (dmc, dmc_before_rain)


def dc_drying_hourly(temp):
    return max(0.0, HOURLY_K_DC * (temp + DC_OFFSET_TEMP))


def drought_code(
    last_dc,
    temp,
    rh,
    ws,
    rain,
    mon,
    hour,
    solrad,
    sunrise,
    sunset,
    dc_before_rain,
    rain_total_prev,
    rain_total,
):
    if 0 == rain_total:
        dc_before_rain = last_dc
    # apply wetting since last period
    dc_wetting_hourly = dc_wetting_between(rain_total_prev, rain_total, dc_before_rain)
    assert 0 <= dc_wetting_hourly
    # at most apply same wetting as current value (don't go below 0)
    dc = max(0.0, last_dc - dc_wetting_hourly)
    dc_hourly = dc_drying_hourly(temp)
    # print(
    #     "last_dc={:0.2f}, dc_wetting_hourly={:0.2f}, dc={:0.2f}, dc_hourly={:0.2f}".format(
    #         last_dc, dc_wetting_hourly, dc, dc_hourly
    #     )
    # )
    dc = dc + dc_hourly
    # HACK: return two values since C uses a pointer to assign a value
    return (dc, dc_before_rain)


# Calculate number of drying "units" this hour contributes
def drying_units(temp, rh, ws, rain, solrad):
    # for now, just add 1 drying "unit" per hour
    return 1.0


def rain_since_intercept_reset(
    temp, rh, ws, rain, mon, hour, solrad, sunrise, sunset, canopy
):
    # for now, want 5 "units" of drying (which is 1 per hour to start)
    TARGET_DRYING_SINCE_INTERCEPT = 5.0
    if 0 < rain:
        # no drying if still raining
        canopy["drying_since_intercept"] = 0.0
    else:
        canopy["drying_since_intercept"] += drying_units(temp, rh, ws, rain, solrad)
        if canopy["drying_since_intercept"] >= TARGET_DRYING_SINCE_INTERCEPT:
            # reset rain if intercept reset criteria met
            canopy["rain_total"] = 0.0
            canopy["drying_since_intercept"] = 0.0
    canopy["rain_total_prev"] = canopy["rain_total"]
    canopy["rain_total"] += rain
    return canopy


##
# Calculate hourly FWI indices from hourly weather stream for a single station.
#
# @param     w               hourly values weather stream
# @param     ffmc_old        previous value for Fine Fuel Moisture Code
# @param     dmc_old         previous value for Duff Moisture Code
# @param     dc_old          previous value for Drought Code
# @return                    hourly values FWI and weather stream
def _stnHFWI(w, ffmc_old, dmc_old, dc_old, silent=False):
    if not util.is_sequential_hours(w):
        raise RuntimeError("Expected input to sequential hourly weather")
    if 1 != len(w["ID"].unique()):
        raise RuntimeError("Expected a single ID value for input weather")
    if 1 != len(w["LAT"].unique()):
        raise RuntimeError("Expected a single LAT value for input weather")
    if 1 != len(w["LONG"].unique()):
        raise RuntimeError("Expected a single LONG value for input weather")
    r = w.loc[:]
    r.columns = map(str.lower, r.columns)
    mcffmc = fine_fuel_moisture_from_code(ffmc_old)
    mcgfmc = mcffmc
    # just use previous index values from current hour regardless of time
    # # HACK: always start from daily value at noon
    # while 12 != r.iloc[0]["hr"]:
    #     r = r.iloc[1:]
    # cur = r.iloc[0]
    # dmc_old = daily_duff_moisture_code(
    #     dmc_old, cur["temp"], cur["rh"], cur["prec"], cur["lat"], int(cur["mon"])
    # )
    # dc_old = daily_drought_code(
    #     dc_old, cur["temp"], cur["rh"], cur["prec"], cur["lat"], int(cur["mon"])
    # )
    # # HACK: start from when daily value should be "accurate"
    # prec_accum = 0.0
    # while HOUR_TO_START_FROM != r.iloc[0]["hr"]:
    #     # tally up precip between noon and whenever we're applying the indices
    #     prec_accum = prec_accum + r.iloc[0]["prec"]
    #     r = r.iloc[1:]
    # r.iloc[0, list(r.columns).index("prec")] += prec_accum
    dmc = dmc_old
    dmc_before_rain = dmc_old
    dc = dc_old
    dc_before_rain = dc_old
    # FIX: just use loop for now so it matches C code
    canopy = {
        "rain_total": 0.0,
        "rain_total_prev": 0.0,
        "drying_since_intercept": 0.0,
    }
    results = []
    for i in range(len(r)):
        cur = r.iloc[i].to_dict()
        canopy = rain_since_intercept_reset(
            cur["temp"],
            cur["rh"],
            cur["ws"],
            cur["prec"],
            cur["mon"],
            cur["hr"],
            cur["solrad"],
            cur["sunrise"],
            cur["sunset"],
            canopy,
        )
        # use lesser of remaining intercept and current hour's rain
        rain_ffmc = (
            0.0
            if (canopy["rain_total"] <= FFMC_INTERCEPT)
            else (
                cur["prec"]
                if ((canopy["rain_total"] - FFMC_INTERCEPT) > cur["prec"])
                else canopy["rain_total"] - FFMC_INTERCEPT
            )
        )
        mcffmc = hourly_fine_fuel_moisture(
            cur["temp"], cur["rh"], cur["ws"], rain_ffmc, mcffmc
        )
        cur["mcffmc"] = mcffmc
        #  convert to code for output, but keep using moisture % for precision
        cur["ffmc"] = fine_fuel_moisture_code(mcffmc)
        # not ideal, but at least encapsulates the code for each index
        dmc, dmc_before_rain = duff_moisture_code(
            dmc,
            cur["temp"],
            cur["rh"],
            cur["ws"],
            cur["prec"],
            cur["mon"],
            cur["hr"],
            cur["solrad"],
            cur["sunrise"],
            cur["sunset"],
            dmc_before_rain,
            canopy["rain_total_prev"],
            canopy["rain_total"],
        )
        cur["dmc"] = dmc
        dc, dc_before_rain = drought_code(
            dc,
            cur["temp"],
            cur["rh"],
            cur["ws"],
            cur["prec"],
            cur["mon"],
            cur["hr"],
            cur["solrad"],
            cur["sunrise"],
            cur["sunset"],
            dc_before_rain,
            canopy["rain_total_prev"],
            canopy["rain_total"],
        )
        cur["dc"] = dc
        cur["isi"] = initial_spread_index(cur["ws"], cur["ffmc"])
        cur["bui"] = buildup_index(dmc, dc)
        cur["fwi"] = fire_weather_index(cur["isi"], cur["bui"])
        cur["dsr"] = daily_severity_rating(cur["fwi"])
        mcgfmc = hourly_grass_fuel_moisture(
            cur["temp"], cur["rh"], cur["ws"], cur["prec"], cur["solrad"], mcgfmc
        )
        cur["mcgfmc"] = mcgfmc
        cur["gfmc"] = grass_moisture_code(mcgfmc, cur["percent_cured"], cur["ws"])
        cur["gsi"] = grass_spread_index(cur["ws"], mcgfmc, cur["percent_cured"])
        cur["gfwi"] = grass_fire_weather_index(cur["gsi"], cur["grass_fuel_load"])
        results.append(cur)
    r = pd.DataFrame(results)
    del r["index"]
    return r


##
# Calculate hourly FWI indices from hourly weather stream.
#
# @param     weatherstream   hourly values weather stream
# @param     timezone        integer offset from GMT to use for sun calculations
# @param     ffmc_old        previous value for Fine Fuel Moisture Code
# @param     dmc_old         previous value for Duff Moisture Code
# @param     dc_old          previous value for Drought Code
# @return                    hourly values FWI and weather stream
def hFWI(
    df_wx,
    timezone,
    ffmc_old=FFMC_DEFAULT,
    dmc_old=DMC_DEFAULT,
    dc_old=DC_DEFAULT,
    silent=False,
):
    wx = df_wx.loc[:]
    old_names = wx.columns
    wx.columns = map(str.upper, wx.columns)
    new_names = wx.columns
    # print(old_names, new_names)
    if not (0 <= ffmc_old <= 101):
        raise RuntimeError("ffmc_old must be 0-101")
    if not (0 <= dmc_old):
        raise RuntimeError("dmc_old must be >= 0")
    if not (0 <= dc_old):
        raise RuntimeError("dc_old must be >= 0")
    had_stn = "ID" in new_names
    had_minute = "MINUTE" in new_names
    had_date = "DATE" in new_names
    had_latitude = "LAT" in new_names
    had_longitude = "LONG" in new_names
    had_timestamp = "TIMESTAMP" in new_names
    was_wind = "WIND" in new_names
    was_rain = "RAIN" in new_names
    was_year = "YEAR" in new_names
    was_hour = "HOUR" in new_names
    if not had_stn:
        wx["ID"] = "STN"
    if not had_minute:
        wx["MINUTE"] = 0
    if not had_latitude:
        logger.warning(f"Using default latitude of {DEFAULT_LATITUDE}")
        wx["LAT"] = DEFAULT_LATITUDE
    if not had_longitude:
        logger.warning(f"Using default longitude of {DEFAULT_LONGITUDE}")
        wx["LONG"] = DEFAULT_LONGITUDE
    COLUMN_SYNONYMS = {"WIND": "WS", "YEAR": "YR", "HOUR": "HR"}
    wx = wx.rename(columns=COLUMN_SYNONYMS)
    if not ((0 <= wx["RH"]).all() and (100 >= wx["RH"]).all()):
        raise RuntimeError("RH must be 0-100")
    if not (0 <= wx["WS"]).all():
        raise RuntimeError("WS must be >= 0")
    if not (0 <= wx["PREC"]).all():
        raise RuntimeError("PREC must be >= 0")
    if not ((1 <= wx["MON"]).all() and (12 >= wx["MON"]).all()):
        raise RuntimeError("MON must be 1-12")
    # FIX: make this actually check proper dates
    if not ((1 <= wx["DAY"]).all() and (31 >= wx["DAY"]).all()):
        raise RuntimeError("DAY must be 1-31")
    if not had_date:
        wx["DATE"] = wx.apply(
            lambda row: f'{row["YR"]:04d}-{row["MON"]:02d}-{row["DAY"]:02d}', axis=1
        )
    if not had_timestamp:
        wx["TIMESTAMP"] = wx.apply(
            lambda row: datetime.datetime(
                row["YR"], row["MON"], row["DAY"], row["HR"], row["MINUTE"]
            ),
            axis=1,
        )
    #########################################
    # PROCESS HERE
    #########################################
    results = None
    for idx, by_year in wx.groupby(["ID", "YR"]):
        logger.debug(f"Running for {idx}")
        w = by_year.reset_index()
        w.loc[:, "TIMEZONE"] = timezone
        w = util.getSunlight(w, with_solrad=False)
        r = _stnHFWI(
            w,
            ffmc_old,
            dmc_old,
            dc_old,
            silent,
        )
        results = pd.concat([results, r])
    # reorganize columns
    colnames_out = [
        "lat",
        "long",
        "yr",
        "mon",
        "day",
        "hr",
        "temp",
        "rh",
        "ws",
        "prec",
        "solrad",
        "ffmc",
        "dmc",
        "dc",
        "isi",
        "bui",
        "fwi",
        "dsr",
        "gfmc",
        "gsi",
        "gfwi",
        "mcffmc",
        "mcgfmc",
        "percent_cured",
        "grass_fuel_load",
    ]
    if "id" in results.columns:
        colnames_out = ["id"] + colnames_out
    results = results[colnames_out]
    return results


if "__main__" == __name__:
    args = sys.argv[1:]
    if len(args) != 6:
        logger.fatal(
            "\n".join(
                [
                    f"Command line:   {sys.argv[0]}  <local GMToffset> <starting FFMC>  <starting DMC> starting <DC> <input file>  <output file>\n",
                    "<local GMToffset> is the off of Greenich mean time (for Eastern = -5  Central=-6   MT=-7  PT=-8 )",
                    "INPUT FILE format must be HOURLY weather data, comma separated and take the form",
                    "All times should be local standard time",
                    "Latitude,Longitude,YEAR,MONTH,DAY,HOUR,Temperature(C),Relative_humiditiy(%%),Wind_speed(km/h),Rainfall(mm)\n",
                ]
            )
        )
        sys.exit(1)
    outfile = args[5]
    infile = args[4]
    if not os.path.exists(infile):
        logger.fatal(f"/n/n ***** FILE  {infile}  does not exist\n")
        sys.exit(1)
    timezone = int(args[0])
    if timezone < -9 or timezone > -2:
        logger.fatal(
            "/n *****   Local time zone adjustment must be vaguely in CAnada so between -9 and -2"
        )
        sys.exit(1)
    ffmc_old = float(args[1])
    if ffmc_old > 101 or ffmc_old < 0:
        logger.fatal(" /n/n *****   FFMC must be between 0 and 101")
        sys.exit(1)
    dmc_old = float(args[2])
    if dmc_old < 0:
        logger.fatal(" /n/n *****  starting DMC must be >=0")
        sys.exit(1)
    dc_old = float(args[3])
    if dc_old < 0:
        logger.fatal(" /n/n *****   starting DC must be >=0\n")
        sys.exit(1)
    logger.debug(f"TZ={timezone}    start ffmc={ffmc_old}  dmc={dmc_old}\n")
    # colnames_in = ["lat", "long", "year", "mon", "day", "hour", "temp", "rh", "wind", "rain"]
    # df = pd.read_csv(infile, header=None, names=colnames_in)
    df_wx = pd.read_csv(infile)
    logger.debug(df_wx)
    # FIX: add check for sequential hours in input
    # FIX: check for all columns being present
    df_fwi = hFWI(df_wx, timezone, ffmc_old, dmc_old, dc_old)
    save_csv(df_fwi, outfile)
