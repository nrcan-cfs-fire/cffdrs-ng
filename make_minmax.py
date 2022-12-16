import pandas as pd
import util

##
# Convert daily noon values stream to daily min/max values stream.
# Uses values from statistics to do the conversion.
#
# @param df        daily noon values weather stream [lat, long, year, mon, day, hour, temp, rh, wind, rain]
# @return          daily min/max values weather stream [lat, long, year, mon, day, hour, temp_min, temp_max, rh_min, rh_max, wind_min, wind_max, rain]
def daily_to_minmax(df):
    df = df.copy()
    had_id = 'id' in map(str.lower, df.columns)
    df['temp_min'] = df['temp'] - 15
    df['temp_max'] = df['temp'] + 2
    df['q'] = df.apply(lambda row: util.find_q(row['temp'], row['rh']), axis=1)
    df['rh_min'] = df.apply(lambda row: util.find_rh(row['q'], row['temp_max']), axis=1)
    df['rh_min'] = df['rh_min'].apply(lambda rh: max(0, rh))
    df['rh_max'] = df.apply(lambda row: util.find_rh(row['q'], row['temp_min']), axis=1)
    df['rh_max'] = df['rh_max'].apply(lambda rh: min(100, rh))
    df['wind_min'] = 0.15 * df['wind']
    df['wind_max'] = 1.25 * df['wind']
    columns = ['lat', 'long', 'year', 'mon', 'day', 'hour', 'temp_min', 'temp_max', 'rh_min', 'rh_max', 'wind_min', 'wind_max', 'rain']
    if had_id:
        df = df[['id'] + columns]
    else:
        df = df[columns]
    return df