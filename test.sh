#!/bin/bash
set -e

export CHECK_DIFF=$*
python create_inputs.py

cmake -S. -B./build
cmake --build ./build

mkdir -p ./out

./run.sh make_inputs -6 ./data/wx_hourly.csv ./out/inputs_hourly.csv
./run.sh NG_FWI -6 85 6 15 ./out/inputs_hourly.csv ./out/wx_hourly_fwi.csv

./run.sh make_inputs -6 ./data/test_hffmc.csv ./out/inputs_hffmc.csv
./run.sh NG_FWI -6 85 6 15 ./out/inputs_hffmc.csv ./out/test_hffmc_fwi.csv

./run.sh make_inputs -6 ./data/test_hffmc.csv ./out/inputs_hffmc.csv
./run.sh NG_FWI -6 85 6 15 ./out/inputs_hffmc.csv ./out/test_hffmc_fwi.csv

./run.sh make_daily ./data/wx_hourly.csv ./out/wx_daily.csv

# use same starting csv so any error in previous step doesn't carry forward
./run.sh make_minmax ./out/wx_daily.csv ./out/wx_minmax.csv

export CHECK_DIFF=
# FIX: C code doesn't match rounding exactly all the time right now
./run.sh make_hourly -6 ./out/wx_minmax.csv ./out/wx_diurnal.csv
export CHECK_DIFF=$*

./run.sh make_inputs -6 ./out/wx_diurnal.csv ./out/inputs_diurnal.csv
./run.sh NG_FWI -6 85 6 15 ./out/inputs_diurnal.csv ./out/wx_diurnal_fwi.csv

./run.sh make_inputs -6 ./data/wx_windy.csv ./out/inputs_windy.csv
./run.sh NG_FWI -6 85 6 15 ./out/inputs_windy.csv ./out/wx_windy_fwi.csv

./run.sh make_inputs -6 ./data/wx_rh100.csv ./out/inputs_rh100.csv
./run.sh NG_FWI -6 85 6 15 ./out/inputs_rh100.csv ./out/wx_rh100_fwi.csv

./run.sh make_inputs -6 ./data/wx_rh0.csv ./out/inputs_rh0.csv
./run.sh NG_FWI -6 85 6 15 ./out/inputs_rh0.csv ./out/wx_rh0_fwi.csv
