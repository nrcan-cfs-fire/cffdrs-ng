import pandas as pd

import make_daily
import make_hourly
import make_minmax
import NG_FWI


def round_format(fmt, precision):
    return lambda x: fmt.format(round(x, precision))


def save_csv(df, file):
    result = df.copy()
    result.columns = map(str.lower, result.columns)
    result["mon"] = result["mon"].apply("{:2d}".format)
    result["day"] = result["day"].apply("{:2d}".format)
    result["hour"] = result["hour"].apply("{:2d}".format)
    result["temp"] = result["temp"].apply(round_format("{:5.1f}", 1))
    result["rh"] = result["rh"].apply(round_format("{:3.0f}", 0))
    result["wind"] = result["wind"].apply(round_format("{:5.1f}", 1))
    result["rain"] = result["rain"].apply(round_format("{:5.1f}", 2))
    result["ffmc"] = result["ffmc"].apply(round_format("{:6.1f}", 1))
    result["dmc"] = result["dmc"].apply(round_format("{:6.1f}", 1))
    result["dc"] = result["dc"].apply(round_format("{:6.1f}", 1))
    result["isi"] = result["isi"].apply(round_format("{:6.1f}", 1))
    result["bui"] = result["bui"].apply(round_format("{:6.1f}", 1))
    result["fwi"] = result["fwi"].apply(round_format("{:6.1f}", 1))
    result["gfmc"] = result["gfmc"].apply(round_format("{:6.1f}", 1))
    result["gsi"] = result["gsi"].apply(round_format("{:6.1f}", 1))
    result["gfwi"] = result["gfwi"].apply(round_format("{:6.1f}", 1))
    result = result[
        [
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
    ]
    result.to_csv(file, index=False)


def run_tests():
    df_wx = pd.read_csv("./wx_hourly.csv")
    result = NG_FWI.hFWI(df_wx, timezone=-6)
    save_csv(result, "./result_py.csv")

    df_wx = pd.read_csv("./wx_hourly.csv")
    test_hffmc = pd.read_csv("test_hffmc.csv")
    df = test_hffmc.copy()
    df["lat"] = df_wx["lat"].iloc[0]
    df["long"] = df_wx["long"].iloc[0]
    COLUMN_SYNONYMS = {"yr": "year", "hr": "hour", "ws": "wind", "prec": "rain"}
    df = df.rename(columns=COLUMN_SYNONYMS)
    df = df[["lat", "long", "year", "mon", "day", "hour", "temp", "rh", "wind", "rain"]]
    df.to_csv("./input_hffmc_py.csv", index=False)
    df = pd.read_csv("./input_hffmc_py.csv")

    result_hffmc = NG_FWI.hFWI(df, timezone=-6)
    save_csv(result_hffmc, "./result_hffmc_py.csv")

    df_wx = pd.read_csv("./wx_hourly.csv")
    df = make_daily.hourly_to_daily(df_wx)
    df["mon"] = df["mon"].apply("{:02d}".format)
    df["day"] = df["day"].apply("{:02d}".format)
    df["hour"] = df["hour"].apply("{:02d}".format)
    df["temp"] = df["temp"].apply(round_format("{:.1f}", 1))
    df["rh"] = df["rh"].apply(round_format("{:.0f}", 0))
    df["wind"] = df["wind"].apply(round_format("{:.1f}", 1))
    df["rain"] = df["rain"].apply(round_format("{:.1f}", 1))
    df = df[["lat", "long", "year", "mon", "day", "hour", "temp", "rh", "wind", "rain"]]
    df.to_csv("wx_daily_py.csv", index=False)

    df_wx = pd.read_csv("./wx_daily_py.csv")
    df = make_minmax.daily_to_minmax(df_wx)
    df["mon"] = df["mon"].apply("{:02d}".format)
    df["day"] = df["day"].apply("{:02d}".format)
    df["hour"] = df["hour"].apply("{:02d}".format)
    df["temp_max"] = df["temp_max"].apply("{:.1f}".format)
    df["temp_min"] = df["temp_min"].apply("{:.1f}".format)
    df["rh_max"] = df["rh_max"].apply("{:.0f}".format)
    df["rh_min"] = df["rh_min"].apply("{:.0f}".format)
    df["wind_max"] = df["wind_max"].apply("{:.1f}".format)
    df["wind_min"] = df["wind_min"].apply("{:.1f}".format)
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
            "wind_min",
            "wind_max",
            "rain",
        ]
    ]
    df.to_csv("wx_minmax_py.csv", index=False)

    wx_minmax = pd.read_csv("./wx_minmax_py.csv")
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
    df.to_csv("wx_diurnal_py.csv", index=False)

    wx_diurnal = pd.read_csv("./wx_diurnal_py.csv")
    result3 = NG_FWI.hFWI(wx_diurnal, timezone=-6)
    save_csv(result3, "./result3_py.csv")

    wx_windy = pd.read_csv("./wx_windy.csv")
    result4 = NG_FWI.hFWI(wx_windy, timezone=-6)
    save_csv(result4, "result4_py.csv")

    save_csv(
        NG_FWI.hFWI(pd.read_csv("./wx_rh100.csv"), timezone=-6), "./result5_py.csv"
    )
    save_csv(NG_FWI.hFWI(pd.read_csv("./wx_rh0.csv"), timezone=-6), "./result6_py.csv")


if __name__ == "__main__":
    run_tests()
