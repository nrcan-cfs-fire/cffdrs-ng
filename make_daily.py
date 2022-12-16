import pandas as pd
import datetime

##
# Convert hourly values stream to daily noon values stream.
#
# @param   df    hourly values weather stream [lat, long, year, mon, day, hour, temp, rh, wind, rain]
# @return        daily noon values weather stream [lat, long, year, mon, day, hour, temp, rh, wind, rain]
def hourly_to_daily(df):
    df = df.copy()
    df['DATE'] = df.apply(lambda row: f'{int(row["year"]):4d}-{int(row["mon"]):02d}-{int(row["day"]):02d}', axis=1)
    df['FOR_DATE'] = df.apply(lambda row: (pd.to_datetime(row['DATE']) if row['hour'] <= 12 else (pd.to_datetime(row['DATE']) + datetime.timedelta(days=1))).date(), axis=1)
    rain = df.groupby('FOR_DATE')['rain'].sum()
    del df['rain']
    df = df.loc[df['hour'] == 12]
    df = pd.merge(df, pd.DataFrame({'rain': rain}), on=['FOR_DATE'])
    return df
