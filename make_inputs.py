import pandas as pd

# make some extreme weather to test boundary conditions

df = pd.read_csv("./data/wx_hourly.csv")
df["rh"] = df["rh"].apply(lambda x: max(0, x - 20))
df.to_csv("./data/wx_rh0.csv", index=False)

df = pd.read_csv("./data/wx_hourly.csv")
df["rh"] = df["rh"].apply(lambda x: min(100, x * 3))
df.to_csv("./data/wx_rh100.csv", index=False)

df = pd.read_csv("./data/wx_hourly.csv")
df["wind"] = df["wind"].apply(lambda x: x * 4)
df.to_csv("./data/wx_windy.csv", index=False)
