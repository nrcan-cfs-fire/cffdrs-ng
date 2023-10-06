cmake --configure . || exit
cmake --build . || exit

./ngfwi -6 85 6 15 ./wx_hourly.csv test.csv
./ngfwi -6 85 6 15 ./input_hffmc.csv test2.csv

./make_daily ./wx_hourly.csv wx_daily_c.csv

./make_minmax ./wx_daily.csv wx_minmax_c.csv

./make_hourly -6 ./wx_minmax_c.csv wx_diurnal_c.csv

./ngfwi -6 85 6 15 ./wx_diurnal.csv test3.csv

./ngfwi -6 85 6 15 ./wx_windy.csv test4.csv
./ngfwi -6 85 6 15 ./wx_rh100.csv test5.csv
./ngfwi -6 85 6 15 ./wx_rh0.csv test6.csv
