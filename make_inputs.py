import datetime
import logging
import os.path
import sys
from math import exp, log, pow, sqrt

import pandas as pd

import util
from util import save_csv

logger = logging.getLogger("cffdrs")
logger.setLevel(logging.WARNING)

# HOUR_TO_START_FROM = 12

# Fuel Load (kg/m^2)
DEFAULT_GRASS_FUEL_LOAD = 0.35

# FIX: figure out what this should be
DEFAULT_LATITUDE = 55.0
DEFAULT_LONGITUDE = -120.0


if "__main__" == __name__:
    # CSV headers
    header = "lat,long,yr,mon,day,hr,temp,rh,ws,prec"
    header_out = (
        "lat,long,yr,mon,day,hr,temp,rh,ws,prec,solrad,percent_cured,grass_fuel_load"
    )
    if 4 != len(sys.argv):
        logger.fatal(
            "\n".join(
                [
                    f"Command line:   {sys.argv[0]}  <local GMToffset> <input file> <output file>\n",
                    "<local GMToffset> is the off of Greenich mean time (for Eastern = -5  Central=-6   MT=-7  PT=-8 )",
                    "All times should be local standard time",
                    "INPUT FILE format must be HOURLY weather data, comma separated and take the form",
                    f"{header}\n",
                ]
            )
        )
        sys.exit(1)
    outfile = sys.argv[3]
    infile = sys.argv[2]
    if not os.path.exists(infile):
        logger.fatal(f"/n/n ***** FILE  {infile}  does not exist\n")
        sys.exit(1)
    timezone = int(sys.argv[1])
    if timezone < -9 or timezone > -2:
        logger.fatal(
            "/n *****   Local time zone adjustment must be vaguely in CAnada so between -9 and -2"
        )
        sys.exit(1)
    # colnames_in = ["lat", "long", "year", "mon", "day", "hour", "temp", "rh", "wind", "rain"]
    # df = pd.read_csv(infile, header=None, names=colnames_in)
    wx = pd.read_csv(infile)
    logger.debug(wx)
    # solar radiation function relies on all hours of the day, so need to pass
    # whole frame
    # print(solrad, sunrise, sunset)
    if any(wx.columns != header.split(",")):
        raise RuntimeError(f"Expected columns {header}")
    wx.columns = [x.upper() for x in wx.columns]
    # require exact columns for now
    if "MINUTE" not in wx.columns:
        wx.loc[:, "MINUTE"] = 0
    # print(wx.dtypes)
    if "TIMESTAMP" not in wx.columns:
        wx.loc[:, "TIMESTAMP"] = wx.apply(
            lambda row: datetime.datetime(
                int(row["YR"]),
                int(row["MON"]),
                int(row["DAY"]),
                int(row["HR"]),
                int(row["MINUTE"]),
            ),
            axis=1,
        )
    wx["TIMEZONE"] = timezone
    wx = util.get_sunlight(wx, with_solrad=True)
    if "PERCENT_CURED" not in wx.columns:
        wx["JULIAN"] = wx.apply(lambda row: util.julian(row["MON"], row["DAY"]), axis=1)
        wx["PERCENT_CURED"] = wx["JULIAN"].apply(util.seasonal_curing)
    if "GRASS_FUEL_LOAD" not in wx.columns:
        wx["GRASS_FUEL_LOAD"] = DEFAULT_GRASS_FUEL_LOAD
    wx.columns = [x.lower() for x in wx.columns]
    colnames_out = header_out.split(",")
    wx = wx.loc[:, colnames_out]
    save_csv(wx, outfile)
