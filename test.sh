#!/bin/bash
set -e

export CHECK_DIFF=$*
python make_inputs.py

cmake -S. -B./build
cmake --build ./build

mkdir -p ./out

./run.sh NG_FWI -6 85 6 15 ./data/wx_hourly.csv ./out/wx_hourly_fwi.csv
./run.sh NG_FWI -6 85 6 15 ./data/test_hffmc.csv ./out/test_hffmc_fwi.csv

./run.sh make_daily ./data/wx_hourly.csv ./out/wx_daily.csv

# use same starting csv so any error in previous step doesn't carry forward
./run.sh make_minmax ./out/wx_daily.csv ./out/wx_minmax.csv

./run.sh make_hourly -6 ./out/wx_minmax.csv ./out/wx_diurnal.csv

./run.sh NG_FWI -6 85 6 15 ./out/wx_diurnal.csv ./out/wx_diurnal_fwi.csv

./run.sh NG_FWI -6 85 6 15 ./data/wx_windy.csv ./out/wx_windy_fwi.csv
./run.sh NG_FWI -6 85 6 15 ./data/wx_rh100.csv ./out/wx_rh100_fwi.csv
./run.sh NG_FWI -6 85 6 15 ./data/wx_rh0.csv ./out/wx_rh0_fwi.csv
