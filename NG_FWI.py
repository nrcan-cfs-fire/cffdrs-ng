import datetime
import logging
import os.path
import sys
from math import exp, log

import pandas as pd
import util

logger = logging.getLogger("cffdrs")
logger.setLevel(logging.WARNING)
# FIX: figure out what this should be
DEFAULT_LATITUDE = 55.0
DEFAULT_LONGITUDE = -120.0

# default startup values
FFMC_DEFAULT = 85
DMC_DEFAULT = 6
DC_DEFAULT = 15

# constants used in grass calculations
GRASS_FUEL_LOAD = 0.35
MAX_SOL_PROP = 0.85
PERCENT_CURED = 100.0


##
# Calculate Initial Spread Index (ISI)
#
# @param wind            Wind Speed (km/h)
# @param ffmc            Fine Fuel Moisure Code
# @return                Initial Spread Index
def isi_calc(ws, ffmc):
    fm = 147.2773 * (101.0 - ffmc) / (59.5 + ffmc)
    fw = (12 * (1 - exp(-0.0818 * (ws - 28)))) if ws >= 40 else (exp(0.05039 * ws))
    sf = 19.1152 * exp(-0.1386 * fm) * (1.0 + fm**5.31 / 4.93e07)
    isi = sf * fw
    return isi


##
# Calculate Build-up Index (BUI)
#
# @param dmc             Duff Moisture Code
# @param dc              Drought Code
# @return                Build-up Index
def bui_calc(dmc, dc):
    bui1 = 0 if (dmc == 0 and dc == 0) else (0.8 * dc * dmc / (dmc + 0.4 * dc))
    p = 0 if dmc == 0 else ((dmc - bui1) / dmc)
    cc = 0.92 * ((0.0114 * dmc) ** 1.7)
    bui0 = dmc - cc * p
    bui0 = max(0, bui0)
    bui1 = bui0 if bui1 < dmc else bui1
    return bui1


##
# Calculate Fire Weather Index (FWI)
#
# @param isi             Initial Spread Index
# @param bui             Build-up Index
# @return                Fire Weather Index
def fwi_calc(isi, bui):
    bb = (
        (0.1 * isi * (1000 / (25 + 108.64 / exp(0.023 * bui))))
        if bui > 80
        else (0.1 * isi * (0.626 * (bui**0.809) + 2))
    )
    fwi = bb if bb <= 1 else exp(2.72 * ((0.434 * log(bb)) ** 0.647))
    return fwi


##
# Calculate hourly Fine Fuel Moisture Code (FFMC)
#
# @param weatherstream   Table of hourly weather data to use for calculations [TEMP, RH, WS, PREC]
# @param ffmc_old        Fine Fuel Moisture Code for previous hour
# @return                Fine Fuel Moisture Codes for given data
def hourly_ffmc(weatherstream, ffmc_old=FFMC_DEFAULT):
    def hffmc(temp, rh, ws, prec, ffmc_old):
        Fo = ffmc_old
        t0 = 1
        mo = 147.27723 * (101 - Fo) / (59.5 + Fo)
        mr = (
            mo
            if 0 == prec
            else (mo + 42.5 * prec * exp(-100 / (251 - mo)) * (1 - exp(-6.93 / prec)))
            if mo <= 150
            else (
                mo
                + 42.5 * prec * exp(-100 / (251 - mo)) * (1 - exp(-6.93 / prec))
                + 0.0015 * ((mo - 150) ** 2) * (prec**0.5)
            )
        )
        mr = min(250, mr)
        mo = mr if prec > 0 else mo
        Ed = (
            0.942 * (rh**0.679)
            + 11 * exp((rh - 100) / 10)
            + 0.18 * (21.1 - temp) * (1 - exp(-0.115 * rh))
        )
        ko = 0.424 * (1 - (rh / 100) ** 1.7) + 0.0694 * (ws**0.5) * (
            1 - (rh / 100) ** 8
        )
        kd = ko * 0.0579 * exp(0.0365 * temp)
        md = Ed + (mo - Ed) * (10 ** (-kd * t0))
        Ew = (
            0.618 * (rh**0.753)
            + 10 * exp((rh - 100) / 10)
            + 0.18 * (21.1 - temp) * (1 - exp(-0.115 * rh))
        )
        k1 = 0.424 * (1 - ((100 - rh) / 100) ** 1.7) + 0.0694 * (ws**0.5) * (
            1 - ((100 - rh) / 100) ** 8
        )
        kw = k1 * 0.0579 * exp(0.0365 * temp)
        mw = Ew - (Ew - mo) * (10 ** (-kw * t0))
        m = md if mo > Ed else mw
        m = mo if Ed >= mo and mo >= Ew else m
        Fo = 59.5 * (250 - m) / (147.27723 + m)
        Fo = max(0, Fo)
        return Fo

    ffmc = []
    for row in weatherstream[["TEMP", "RH", "WS", "PREC"]].itertuples():
        i, temp, rh, ws, prec = row
        ffmc_old = hffmc(temp, rh, ws, prec, ffmc_old)
        ffmc.append(ffmc_old)
    return ffmc


##
# Calculate vapour pressure deficit
#
# @param temperature         Temperature (Celcius)
# @param relative_humidity   Relative Humidity (percent, 0-100)
# @return                    Vapour Pressure Deficit (kPa)
def vpd(temperature, relative_humidity):
    vapour_pressure_saturation = 0.61078 * exp(
        17.269 * temperature / (temperature + 237.3)
    )
    vapour_pressure_deficit = vapour_pressure_saturation * (
        1.0 - relative_humidity / 100.0
    )
    return vapour_pressure_deficit


##
# Calculate hourly Fine Fuel Moisture Code (FFMC)
#
# @param w               Table of hourly weather data to use for calculations [TIMESTAMP, DATE, TEMP, RH, WS, PREC]
# @param ffmc_old        Fine Fuel Moisture Code for previous hour
# @return                Fine Fuel Moisture Codes for given data
def _hffmc(w, ffmc_old):
    # get daily values but still get for days we don't have noon for because we want PREC total
    # daily <- toDaily(w, all=TRUE)
    # w[, FOR_DATE := ifelse(hour(TIMESTAMP) < 13, DATE, as.character(as.Date(DATE) + 1))]
    # daily$FOR_DATE <- daily$DATE
    #
    # FFMC uses the hourly FFMC calculation, but reduces the overall rain received by
    # 0.5mm. This is done by figuring out what fraction of the period's rain occurs in
    # each hour and then taking that fraction of 0.5mm off of that hour.
    #
    # want to take 0.5 mm off of the total but proportional to amounts per hour
    # CHECK: does this make more sense than just removing the first 0.5mm?
    # figure out what fraction of the total rain to be counted 1mm is
    # daily[, FFMC_MULTIPLIER := ifelse(0.5 >= PREC, 0, (PREC - 0.5) / PREC)]
    # for_ffmc <- merge(w, daily[, c('FOR_DATE', 'FFMC_MULTIPLIER')], by=c('FOR_DATE'))
    for_ffmc = w.loc[:]
    sum_prec = pd.DataFrame({"SUM_PREC": for_ffmc.groupby("DATE")["PREC"].sum()})
    for_ffmc = pd.merge(for_ffmc, sum_prec, on=["DATE"])
    for_ffmc["FFMC_MULTIPLIER"] = for_ffmc.apply(
        lambda row: 0
        if 0.5 >= row["SUM_PREC"]
        else (row["SUM_PREC"] - 0.5) / row["SUM_PREC"],
        axis=1,
    )
    for_ffmc["PREC"] = for_ffmc.apply(
        lambda row: row["PREC"] * row["FFMC_MULTIPLIER"], axis=1
    )
    for_ffmc["FFMC"] = hourly_ffmc(for_ffmc, ffmc_old=ffmc_old)
    # for_ffmc = for_ffmc[['TIMESTAMP', 'FFMC']]
    # NOTE: returning column doesn't assign anything if index doesn't match
    return list(for_ffmc["FFMC"])


##
# Calculate hourly Duff Moisture Code (DMC)
#
# @param w               Table of hourly weather data to use for calculations [TIMESTAMP, DATE, SUNRISE, SUNSET, MON, TEMP, RH, PREC]
# @param dmc_old         Duff Moisture Code for previous hour
# @return                Duff Moisture Codes for given data
def _hdmc(w, dmc_old):
    def hourly_dmc(
        t, rh, ws, rain, mon, lastdmc, DryFrac, rain24, DELTA_MCrain, tnoon, rhnoon
    ):
        el = [6.5, 7.5, 9, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8, 7, 6]
        # wetting FROM rain
        if rain > 0 and DELTA_MCrain > 0.0:
            # printf("rain=%f  change=%f lastdmc=%f\n",rain, DELTA_MCrain, lastdmc);
            mc = 20.0 + 280.0 / exp(0.023 * lastdmc)
            #  the MC increase by the rain in this hour...  total * rain_hour/rain24
            mc = mc + DELTA_MCrain * (rain / rain24)
            lastdmc = 43.43 * (5.6348 - log(mc - 20))
        # drying all day long too
        tnoon = max(-1.1, tnoon)
        # full day of drying in old FWI/DMC
        DELTA_dry = 1.894 * (tnoon + 1.1) * (100.0 - rhnoon) * el[mon - 1] * 0.0001
        # printf("delta dmc, %f ,lastDMC,%f , frac,%f , fractional,%f\n",DELTA_mcrain,lastdmc, DryFrac, (DELTA_dry*DryFrac));
        dmc = lastdmc + (DELTA_dry * DryFrac)
        dmc = max(0, dmc)
        return dmc

    dmc = w.loc[:]
    if "SUNSET" not in dmc.columns:
        raise RuntimeError("SUNSET column required")
    if "SUNRISE" not in dmc.columns:
        raise RuntimeError("SUNRISE column required")
    dmc["VPD"] = dmc.apply(
        lambda row: vpd(row["TEMP"], row["RH"])
        if row["SUNRISE"] <= row["HR"] and row["HR"] <= row["SUNSET"]
        else 0.0,
        axis=1,
    )
    dmc = pd.merge(
        dmc,
        pd.DataFrame(
            {
                "RAIN24": dmc.groupby("DATE")["PREC"].sum(),
                "MINRH": dmc.groupby("DATE")["RH"].min(),
                "VPD24": dmc.groupby("DATE")["VPD"].sum(),
            }
        ),
        on=["DATE"],
    )
    dmc["DRYFRAC"] = dmc.apply(
        lambda row: (
            (1 / (row["SUNSET"] - row["SUNRISE"]))
            if 0 == row["VPD24"]
            else (row["VPD"] / row["VPD24"])
        )
        if row["SUNRISE"] <= row["HR"] <= row["SUNSET"]
        else 0.0,
        axis=1,
    )
    lastdmc = dmc_old
    dates = w["DATE"].unique()
    result = []
    for d in dates:
        for_date = dmc[dmc["DATE"] == d]
        # HACK: fails if no noon for this day
        noons = for_date.loc[dmc["HR"] == 12]
        if 0 == len(noons):
            raise RuntimeError("Excpected noon value for every day provided")
        noon = noons.iloc[0]
        tnoon = float(noon["TEMP"])
        rhnoon = float(noon["RH"])
        rain24 = for_date["RAIN24"].iloc[0]
        if rain24 > 1.5:
            reff = 0.92 * rain24 - 1.27
            if lastdmc <= 33:
                b = 100.0 / (0.5 + 0.3 * lastdmc)
            elif lastdmc <= 65:
                b = 14.0 - 1.3 * log(lastdmc)
            else:
                b = 6.2 * log(lastdmc) - 17.2
            DELTA_mcddmcrain24 = 1000.0 * reff / (48.77 + b * reff)
        else:
            DELTA_mcddmcrain24 = 0.0
        hrs = for_date["HR"].unique()
        values = []
        for r in for_date.itertuples():
            lastdmc = hourly_dmc(
                r.TEMP,
                r.RH,
                r.WS,
                r.PREC,
                r.MON,
                lastdmc,
                r.DRYFRAC,
                r.RAIN24,
                DELTA_mcddmcrain24,
                tnoon,
                rhnoon,
            )
            values.append(lastdmc)
        if len(values) != len(hrs):
            raise RuntimeError("Expected one output value per input hour")
        result.extend(values)
    return result


##
# Calculate hourly Drought Code (DC)
#
# @param w               Table of hourly weather data to use for calculations [TIMESTAMP, DATE, SUNRISE, SUNSET, MON, TEMP, RH, PREC]
# @param dc_old          Drought Code for previous hour
# @return                Drought Codes for given data
def _hdc(w, dc_old):
    def hourly_dc(t, rh, ws, rain, lastdc, mon, rain24, dryfrac, DELTArain24, temp12):
        fl = [-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5, 2.4, 0.4, -1.6, -1.6]
        if rain > 0 and DELTArain24 < 0.0:
            # (weight it by Rainhour/rain24 )
            lastdc = lastdc + DELTArain24 * (rain / rain24)
        # total dry for the DAY
        DELTAdry24 = (0.36 * (temp12 + 2.8) + fl[mon - 1]) / 2.0
        # ;    /* the fix for winter negative DC change...shoulders*/
        DELTAdry24 = max(0.0, DELTAdry24)
        # /* dry frac is VPD weighted value for the hour */
        dc = lastdc + DELTAdry24 * dryfrac
        dc = max(0, dc)
        return dc

    dc = w.loc[:]
    if "SUNSET" not in dc.columns:
        raise RuntimeError("SUNSET column required")
    if "SUNRISE" not in dc.columns:
        raise RuntimeError("SUNRISE column required")
    dc["VPD"] = dc.apply(
        lambda row: vpd(row["TEMP"], row["RH"])
        if row["SUNRISE"] <= row["HR"] and row["HR"] <= row["SUNSET"]
        else 0.0,
        axis=1,
    )
    dc = pd.merge(
        dc,
        pd.DataFrame(
            {
                "RAIN24": dc.groupby("DATE")["PREC"].sum(),
                "MINRH": dc.groupby("DATE")["RH"].min(),
                "VPD24": dc.groupby("DATE")["VPD"].sum(),
            }
        ),
        on=["DATE"],
    )
    dc["DRYFRAC"] = dc.apply(
        lambda row: (
            (1 / (row["SUNSET"] - row["SUNRISE"]))
            if 0 == row["VPD24"]
            else (row["VPD"] / row["VPD24"])
        )
        if row["SUNRISE"] <= row["HR"] <= row["SUNSET"]
        else 0.0,
        axis=1,
    )
    # #>>>
    # # FAILS when RH = 100 all day because fraction is divided weird and doesn't add up to 1 all the time
    # dc[, SUMDRY := sum(DRYFRAC), by=c("DATE")]
    # stopifnot(all(dmc$SUMDRY - 1 < 0.000001))
    # #<<<
    lastdc = dc_old
    dates = w["DATE"].unique()
    result = []
    for d in dates:
        for_date = dc[dc["DATE"] == d]
        noon = for_date.loc[dc["HR"] == 12].iloc[0]
        tnoon = float(noon["TEMP"])
        rhnoon = float(noon["RH"])
        rain24 = for_date["RAIN24"].iloc[0]
        if rain24 > 2.8:
            rw = 0.83 * rain24 - 1.27
            smi = 800 * exp(-lastdc / 400)
            # TOTAL change for the TOTAL 24 hour rain from FWI1970 model
            DELTA_DCrain24 = -400.0 * log(1.0 + 3.937 * rw / smi)
        else:
            DELTA_DCrain24 = 0.0
        hrs = for_date["HR"].unique()
        values = []
        for r in for_date.itertuples():
            lastdc = hourly_dc(
                r.TEMP,
                r.RH,
                r.WS,
                r.PREC,
                lastdc,
                r.MON,
                r.RAIN24,
                r.DRYFRAC,
                DELTA_DCrain24,
                tnoon,
            )
            values.append(lastdc)
        if len(values) != len(hrs):
            raise RuntimeError("Expected one output value per input hour")
        result.extend(values)
    return result


##
# Calculate Hourly Grass Fuel Moisture. Needs to be converted to get GFMC.
#
# @param temp            Temperature (Celcius)
# @param rh              Relative Humidity (percent, 0-100)
# @param wind            Wind Speed (km/h)
# @param rain            Precipitation (mm)
# @param lastmc          Previous grass fuel moisture (percent)
# @param solrad          Solar radiation (kW/m^2)
# @param time            Time since last observation (hours)
# @return                Grass Fuel Moisture (percent)
def hourly_gfmc(temp, rh, wind, rain, lastmc, solrad, time):
    # MARK II of the model (2016) wth new solar rad model specific to grass
    #
    # Temp is temperature in C
    # RH is realtive humidty in %
    # wind is average wind speed in km/h
    # rain is rainfall in mm
    # solrad is kW/m2  (radiaiton reaching fuel)
    # mo is the old grass fuel moisture   (not as a code value...so elimates the conversion to code)
    # time - time between obs in HOURS
    #
    #
    # DRF of 1/16.1 comes from reducting the standard response time curve
    # at 26.7C, 20%RH, 2 km/h to 0.85hr.
    #
    #
    #
    # bmw
    drf = 0.389633
    mo = lastmc
    # fuel temperature/humidity
    tf = temp + 17.9 * solrad * exp(-0.034 * wind)  # fuel temp from CEVW
    rhf = (
        (
            rh
            * 6.107
            * 10.0 ** (7.5 * temp / (temp + 237.0))
            / (6.107 * 10.0 ** (7.5 * tf / (tf + 237.0)))
        )
        if tf > temp
        else rh
    )
    if rain != 0:
        # /*     mo+=rain*rf*exp(-100.0/(251.0-mo))*(1.0-exp(-6.93/rain));*/  /* old routine*/
        # /* this new routine assumes layer is 0.3 kg/m2 so 0.3mm of rain adds +100%MC*/
        mo = (
            mo + rain / 0.3 * 100.0
        )  # /* *100 to convert to %...  *1/.3 because of 0.3mm=100%  */
        mo = min(250.0, mo)
    ed = (
        1.62 * rhf**0.532
        + (13.7 * exp((rhf - 100) / 13.0))
        + 0.27 * (26.7 - tf) * (1.0 - 1.0 / exp(0.115 * rhf))
    )  # GRASS EMC
    moed = mo - ed
    ew = (
        1.42 * rhf**0.512
        + (12.0 * exp((rhf - 100) / 18.0))
        + 0.27 * (26.7 - tf) * (1.0 - 1.0 / exp(0.115 * rhf))
    )  # GRASS EMC
    moew = mo - ew
    if moed == 0 or (moew >= 0 and moed < 0):
        xm = mo
        if moed == 0:
            e = ed
        if moew >= 0:
            e = ew
    else:
        if moed > 0:
            a1 = rhf / 100
            e = ed
            moe = moed
        else:
            a1 = (100.0 - rhf) / 100.0
            e = ew
            moe = moew
        xkd = 0.424 * (1 - a1**1.7) + (0.0694 * wind**0.5 * (1 - a1**8))
        xkd = xkd * drf * exp(0.0365 * tf)
        # //   printf("tf=%8.4f rhf=%6.2f e=%4.1f mo=%5.2f xkd=%6.4f moed=%5.1f moew=%5.1f\n",tf,rhf,e,mo,xkd,moed,moew);
        xm = e + moe * exp(-1.0 * log(10.0) * xkd * time)
    return xm


##
# Calculate Grass Spread Index (GSI)
#
# @param wind            Wind Speed (km/h)
# @param mc              Grass moisture content (percent)
# @param cur             Degree of curing (percent, 0-100)
# @return                Grass Spread Index
def grass_isi(wind, mc, cur):
    fw = (
        ((0.054 + 0.209 * wind) * 16.67)
        if wind < 5
        else ((1.1 + 0.715 * (wind - 5.0) * 0.844) * 16.67)
    )
    if mc < 12:
        fm = exp(-0.108 * mc)
    elif mc < 20 and wind < 10:
        fm = 0.684 - 0.0342 * mc
    elif mc < 23.9 and wind >= 10:
        fm = 0.547 - 0.0228 * mc
    else:
        fm = 0
    cf = (1.034 / (1 + 104 * exp(-0.1 * (cur - 20)))) if cur > 20 else 0.0
    return 1.11 * fw * fm * cf


##
# Calculate Grass Fire Weather Index
#
# @param gsi               Grass Spread Index
# @param load              Fuel Load (kg/m^2)
# @return                  Grass Fire Weather Index
def grass_fwi(gsi, load):
    ros = gsi / 1.11  #  this just converts back to ROS in m/min
    Fint = 300.0 * load * ros
    gfwi = (log(Fint / 60.0) / 0.14) if Fint > 100 else (Fint / 25.0)
    return gfwi


##
# Calculate hourly Grass Fine Fuel Moisture Code (FFMC)
#
# @param w               Table of hourly weather data to use for calculations [TIMESTAMP, DATE, TEMP, RH, WS, PREC, SOLRAD]
# @param lastmcgmc       Grass Fuel Moisture for previous hour
# @return                Grass Fine Fuel Moisture Codes for given data
def _hgffmc(w, lastmcgmc):
    result = []
    for r in w[["TEMP", "RH", "WS", "PREC", "SOLRAD"]].itertuples():
        lastmcgmc = hourly_gfmc(r.TEMP, r.RH, r.WS, r.PREC, lastmcgmc, r.SOLRAD, 1.0)
        result.append(lastmcgmc)
    return result


##
# Calculate hourly FWI indices from hourly weather stream for a single station.
#
# @param     w               hourly values weather stream
# @param     timezone        integer offset from GMT to use for sun calculations
# @param     ffmc_old        previous value for Fine Fuel Moisture Code
# @param     dmc_old         previous value for Duff Moisture Code
# @param     dc_old          previous value for Drought Code
# @param     percent_cured   Grass curing (percent, 0-100)
# @return                    hourly values FWI and weather stream
def _stnHFWI(w, timezone, ffmc_old, dmc_old, dc_old, percent_cured, silent=False):
    if not util.is_sequential_hours(w):
        raise RuntimeError("Expected input to sequential hourly weather")
    if 1 != len(w["ID"].unique()):
        raise RuntimeError("Expected a single ID value for input weather")
    if 1 != len(w["LAT"].unique()):
        raise RuntimeError("Expected a single LAT value for input weather")
    if 1 != len(w["LONG"].unique()):
        raise RuntimeError("Expected a single LONG value for input weather")
    r = w.loc[:]
    # dates = w['TIMESTAMP'].apply(lambda d: d.date()).unique()
    row = r.iloc[0]
    latitude = row["LAT"]
    longitude = row["LONG"]
    # check that sun() works on a single line
    solrad, sunrise, sunset = util.sun(
        row["LAT"], row["LONG"], row["MON"], row["DAY"], row["HR"], timezone
    )
    # print(solrad, sunrise, sunset)
    r[["SOLRAD", "SUNRISE", "SUNSET"]] = r.apply(
        lambda row: util.sun(
            row["LAT"], row["LONG"], row["MON"], row["DAY"], row["HR"], timezone
        ),
        axis=1,
        result_type="expand",
    )
    # HACK: formulas don't work if noon is missing, so filter
    have_dates = r["TIMESTAMP"].dt.date.drop_duplicates()
    have_noon = r[r["HR"] == 12]["TIMESTAMP"].dt.date.drop_duplicates()
    missing_noon = list(set(have_dates).difference(set(have_noon)))
    if missing_noon:
        n0 = len(r)
        r = r.loc[~(r["TIMESTAMP"].dt.date.isin(missing_noon))]
        n1 = len(r)
        if not silent:
            logging.warning(
                f"Can't calculate indices for dates missing noon, so dropping {n0 - n1} rows"
                f"and not calculating for {[x.strftime('%Y-%m-%d') for x in missing_noon]}"
            )
    r["FFMC"] = _hffmc(r, ffmc_old)
    r["DMC"] = _hdmc(r, dmc_old)
    r["DC"] = _hdc(r, dc_old)
    r["ISI"] = r.apply(lambda row: isi_calc(row["WS"], row["FFMC"]), axis=1)
    r["BUI"] = r.apply(lambda row: bui_calc(row["DMC"], row["DC"]), axis=1)
    r["FWI"] = r.apply(lambda row: fwi_calc(row["ISI"], row["BUI"]), axis=1)
    r["DSR"] = r.apply(lambda row: 0.0272 * (row["FWI"] ** 1.77), axis=1)
    r = pd.merge(
        r, pd.DataFrame({"MIN_RH": r.groupby("DATE")["RH"].min()}), on=["DATE"]
    )
    r["MIN_RH"] = r.apply(lambda row: min(100, max(0, row["MIN_RH"])), axis=1)
    r["SOLPROP"] = r.apply(
        lambda row: (1.27 - 0.0111 * row["MIN_RH"]) if row["MIN_RH"] > 30 else 1, axis=1
    )
    r["SOLPROP"] = r.apply(lambda row: max(0, MAX_SOL_PROP * row["SOLPROP"]), axis=1)
    r["SOLRAD"] = r.apply(lambda row: row["SOLRAD"] * row["SOLPROP"], axis=1)
    lastmcgmc = 101.0 - ffmc_old  # approximation for a start up
    r["MCGMC"] = _hgffmc(r, lastmcgmc)
    r["GFMC"] = r.apply(
        lambda row: 59.5 * (250 - row["MCGMC"]) / (147.2772277 + row["MCGMC"]), axis=1
    )
    r["GSI"] = r.apply(
        lambda row: grass_isi(row["WS"], row["MCGMC"], percent_cured), axis=1
    )
    r["GFWI"] = r.apply(lambda row: grass_fwi(row["GSI"], GRASS_FUEL_LOAD), axis=1)
    # print(r)
    return r


##
# Calculate hourly FWI indices from hourly weather stream.
#
# @param     weatherstream   hourly values weather stream
# @param     timezone        integer offset from GMT to use for sun calculations
# @param     ffmc_old        previous value for Fine Fuel Moisture Code
# @param     dmc_old         previous value for Duff Moisture Code
# @param     dc_old          previous value for Drought Code
# @param     percent_cured   Grass curing (percent, 0-100)
# @return                    hourly values FWI and weather stream
def hFWI(
    weatherstream,
    timezone,
    ffmc_old=FFMC_DEFAULT,
    dmc_old=DMC_DEFAULT,
    dc_old=DC_DEFAULT,
    percent_cured=PERCENT_CURED,
    silent=False,
):
    wx = weatherstream.loc[:]
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
    if not (0 <= percent_cured <= 100):
        raise RuntimeError("percent_cured must be 0-100")
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
    COLUMN_SYNONYMS = {"WIND": "WS", "RAIN": "PREC", "YEAR": "YR", "HOUR": "HR"}
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
        r = _stnHFWI(
            by_year.reset_index(),
            timezone,
            ffmc_old,
            dmc_old,
            dc_old,
            percent_cured,
            silent,
        )
        results = pd.concat([results, r])
    #########################################
    # REVERT TO ORIGINAL COLUMN NAMES
    #########################################
    if not had_stn:
        del results["ID"]
    if not had_minute:
        del results["MINUTE"]
    if not had_date:
        del results["DATE"]
    if not had_latitude:
        del results["LAT"]
    if not had_longitude:
        del results["LONG"]
    if not had_timestamp:
        del results["TIMESTAMP"]
    results = results.rename(columns={v: k for k, v in COLUMN_SYNONYMS.items()})
    return results


if "__main__" == __name__:
    # sys.argv = ['NG_FWI.py', '-6', '85', '6', '15', './bak_diurnal.csv', 'test3_py.csv']
    # print(sys.argv)
    if len(sys.argv) != 7:
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
    outfile = sys.argv[6]
    infile = sys.argv[5]
    if not os.path.exists(infile):
        logger.fatal(f"/n/n ***** FILE  {infile}  does not exist\n")
        sys.exit(1)
    TZadjust = int(sys.argv[1])
    if TZadjust < -9 or TZadjust > -2:
        logger.fatal(
            "/n *****   Local time zone adjustment must be vaguely in CAnada so between -9 and -2"
        )
        sys.exit(1)
    lastffmc = float(sys.argv[2])
    if lastffmc > 101 or lastffmc < 0:
        logger.fatal(" /n/n *****   FFMC must be between 0 and 101")
        sys.exit(1)
    lastdmc = float(sys.argv[3])
    if lastdmc < 0:
        logger.fatal(" /n/n *****  starting DMC must be >=0")
        sys.exit(1)
    lastdc = float(sys.argv[4])
    if lastdc < 0:
        logger.fatal(" /n/n *****   starting DC must be >=0\n")
        sys.exit(1)
    logger.debug(f"TZ={TZadjust}    start ffmc={lastffmc}  dmc={lastdmc}\n")
    colnames_out = [
        "year",
        "mon",
        "day",
        "hour",
        "temp",
        "rh",
        "wind",
        "rain",
        "ffmc",
        "dmc",
        "dc",
        "isi",
        "bui",
        "fwi",
        "gfmc",
        "gsi",
        "gfwi",
    ]
    # colnames_in = ["lat", "long", "year", "mon", "day", "hour", "temp", "rh", "wind", "rain"]
    # df = pd.read_csv(infile, header=None, names=colnames_in)
    df = pd.read_csv(infile)
    logger.debug(df)
    # FIX: add check for sequential hours in input
    # FIX: check for all columns being present
    # weatherstream = df
    # timezone = TZadjust
    # ffmc_old = lastffmc
    # dmc_old = lastdmc
    # dc_old = lastdc
    # percent_cured = PERCENT_CURED
    df_fwi = hFWI(df, TZadjust, lastffmc, lastdmc, lastdc, PERCENT_CURED)
    df_fwi.to_csv(outfile, index=False)
