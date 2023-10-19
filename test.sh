python make_inputs.py

cmake -S. -B./build || exit
cmake --build ./build || exit

mkdir -p ./out
./bin/ngfwi -6 85 6 15 ./data/wx_hourly.csv ./out/wx_hourly_fwi_c.csv
./bin/ngfwi -6 85 6 15 ./data/test_hffmc.csv ./out/test_hffmc_fwi_c.csv

./bin/make_daily ./data/wx_hourly.csv ./out/wx_daily.csv
python make_daily.py ./data/wx_hourly.csv ./out/wx_daily_py.csv
# diff ./out/wx_daily.csv ./out/wx_daily_py.csv || exit
Rscript make_daily.r ./data/wx_hourly.csv ./out/wx_daily_r.csv
# diff ./out/wx_daily_py.csv ./out/wx_daily_r.csv || exit

# use same starting csv so any error in previous step doesn't carry forward
./bin/make_minmax ./out/wx_daily.csv ./out/wx_minmax.csv
python make_minmax.py ./out/wx_daily.csv ./out/wx_minmax_py.csv
# diff ./out/wx_minmax.csv ./out/wx_minmax_py.csv || exit
Rscript make_minmax.r ./out/wx_daily.csv ./out/wx_minmax_r.csv
# diff ./out/wx_minmax_py.csv ./out/wx_minmax_r.csv || exit

./bin/make_hourly -6 ./out/wx_minmax.csv ./out/wx_diurnal.csv
python make_hourly.py -6 ./out/wx_minmax.csv ./out/wx_diurnal_py.csv
# diff ./out/wx_diurnal.csv ./out/wx_diurnal_py.csv || exit
Rscript make_hourly.r -6 ./out/wx_minmax.csv ./out/wx_diurnal_r.csv
# diff ./out/wx_diurnal_py.csv ./out/wx_diurnal_r.csv || exit

./bin/ngfwi -6 85 6 15 ./out/wx_diurnal.csv ./out/wx_diurnal_fwi.csv
python NG_FWI.py -6 85 6 15 ./out/wx_diurnal.csv ./out/wx_diurnal_fwi_py.csv
# diff ./out/wx_diurnal_fwi.csv ./out/wx_diurnal_fwi_py.csv || exit
Rscript NG_FWI.r -6 85 6 15 ./out/wx_diurnal.csv ./out/wx_diurnal_fwi_r.csv
# diff ./out/wx_diurnal_fwi.csv ./out/wx_diurnal_fwi_r.csv || exit


./bin/ngfwi -6 85 6 15 ./data/wx_windy.csv ./out/wx_windy_fwi_c.csv
./bin/ngfwi -6 85 6 15 ./data/wx_rh100.csv ./out/wx_rh100_fwi_c.csv
./bin/ngfwi -6 85 6 15 ./data/wx_rh0.csv ./out/wx_rh0_fwi_c.csv
