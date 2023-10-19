import pandas as pd

import make_daily
import make_hourly
import make_minmax
import NG_FWI
from util import save_csv


def round_format(fmt, precision):
    return lambda x: fmt.format(round(x, precision))


def run_tests():
    df_wx = pd.read_csv("./data/wx_hourly.csv")
    result = NG_FWI.hFWI(df_wx, timezone=-6)
    save_csv(result, "./result_py.csv")

    df_wx = pd.read_csv("./data/wx_hourly.csv")
    test_hffmc = pd.read_csv("test_hffmc.csv")
    df = test_hffmc.copy()
    df["lat"] = df_wx["lat"].iloc[0]
    df["long"] = df_wx["long"].iloc[0]
    COLUMN_SYNONYMS = {"yr": "year", "hr": "hour", "ws": "wind", "prec": "rain"}
    df = df.rename(columns=COLUMN_SYNONYMS)
    df = df[["lat", "long", "year", "mon", "day", "hour", "temp", "rh", "wind", "rain"]]
    df.to_csv("./data/test_hffmc_py.csv", index=False)
    df = pd.read_csv("./data/test_hffmc_py.csv")

    result_hffmc = NG_FWI.hFWI(df, timezone=-6)
    save_csv(result_hffmc, "./result_hffmc_py.csv")

    df_wx = pd.read_csv("./data/wx_hourly.csv")
    df = make_daily.hourly_to_daily(df_wx)
    df["mon"] = df["mon"].apply("{:02d}".format)
    df["day"] = df["day"].apply("{:02d}".format)
    df["hour"] = df["hour"].apply("{:02d}".format)
    df["temp"] = df["temp"].apply(round_format("{:.1f}", 1))
    df["rh"] = df["rh"].apply(round_format("{:.0f}", 0))
    df["wind"] = df["wind"].apply(round_format("{:.1f}", 1))
    df["rain"] = df["rain"].apply(round_format("{:.1f}", 1))
    df = df[["lat", "long", "year", "mon", "day", "hour", "temp", "rh", "wind", "rain"]]
    df.to_csv("./data/wx_daily_py.csv", index=False)

    df_wx = pd.read_csv("./data/wx_daily_py.csv")
    df = make_minmax.daily_to_minmax(df_wx)
    df["mon"] = df["mon"].apply("{:02d}".format)
    df["day"] = df["day"].apply("{:02d}".format)
    df["hour"] = df["hour"].apply("{:02d}".format)
    df["temp_max"] = df["temp_max"].apply("{:.1f}".format)
    df["temp_min"] = df["temp_min"].apply("{:.1f}".format)
    df["rh_max"] = df["rh_max"].apply("{:.0f}".format)
    df["rh_min"] = df["rh_min"].apply("{:.0f}".format)
    df["ws_max"] = df["ws_max"].apply("{:.1f}".format)
    df["ws_min"] = df["ws_min"].apply("{:.1f}".format)
    df["rain"] = df["rain"].apply("{:.1f}".format)
    df = df[
        [
            "lat",
            "long",
            "year",
            "mon",
            "day",
            "hour",
            "temp_min",
            "temp_max",
            "rh_min",
            "rh_max",
            "ws_min",
            "ws_max",
            "rain",
        ]
    ]
    df.to_csv("./data/wx_minmax_py.csv", index=False)

    wx_minmax = pd.read_csv("./data/wx_minmax_py.csv")
    df = make_hourly.minmax_to_hourly(wx_minmax, timezone=-6)
    # FIX: this is just to match R code for now
    df["year"] = df["year"].apply("{:02d}".format)
    df["mon"] = df["mon"].apply("{:02d}".format)
    df["day"] = df["day"].apply("{:02d}".format)
    df["hour"] = df["hour"].apply("{:02d}".format)
    df["temp"] = df["temp"].apply(round_format("{:.1f}", 1))
    df["rh"] = df["rh"].apply(round_format("{:.0f}", 0))
    df["wind"] = df["wind"].apply(round_format("{:.1f}", 1))
    df["rain"] = df["rain"].apply(round_format("{:.1f}", 1))
    df = df[["lat", "long", "year", "mon", "day", "hour", "temp", "rh", "wind", "rain"]]
    df.to_csv("./data/wx_diurnal_py.csv", index=False)

    wx_diurnal = pd.read_csv("./data/wx_diurnal_py.csv")
    result3 = NG_FWI.hFWI(wx_diurnal, timezone=-6)
    save_csv(result3, "./result3_py.csv")

    wx_windy = pd.read_csv("./data/wx_windy.csv")
    result4 = NG_FWI.hFWI(wx_windy, timezone=-6)
    save_csv(result4, "result4_py.csv")

    save_csv(
        NG_FWI.hFWI(pd.read_csv("./data/wx_rh100.csv"), timezone=-6), "./result5_py.csv"
    )
    save_csv(
        NG_FWI.hFWI(pd.read_csv("./data/wx_rh0.csv"), timezone=-6), "./result6_py.csv"
    )


if __name__ == "__main__":
    run_tests()
