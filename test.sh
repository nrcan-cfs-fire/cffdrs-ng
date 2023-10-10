python3 make_inputs.py

cmake -S. -B./build || exit
cmake --build ./build || exit

mkdir -p ./out
./bin/ngfwi -6 85 6 15 ./data/wx_hourly.csv ./out/test.csv
./bin/ngfwi -6 85 6 15 ./data/test_hffmc.csv ./out/test2.csv

./bin/make_daily ./data/wx_hourly.csv ./out/wx_daily_c.csv

./bin/make_minmax ./data/wx_daily.csv ./out/wx_minmax_c.csv

./bin/make_hourly -6 ./data/wx_minmax_c.csv ./out/wx_diurnal_c.csv

./bin/ngfwi -6 85 6 15 ./data/wx_diurnal.csv ./out/test3.csv

./bin/ngfwi -6 85 6 15 ./data/wx_windy.csv ./out/test4.csv
./bin/ngfwi -6 85 6 15 ./data/wx_rh100.csv ./out/test5.csv
./bin/ngfwi -6 85 6 15 ./data/wx_rh0.csv ./out/test6.csv
