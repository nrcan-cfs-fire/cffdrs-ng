cmake --configure . || exit
cmake --build . || exit

./ngfwi -6 85 6 15 ./bak_hourly.csv test.csv
./ngfwi -6 85 6 15 ./input_hffmc.csv test2.csv

./make_daily ./bak_hourly.csv bak_daily_c.csv

./make_minmax ./bak_daily.csv bak_minmax_c.csv

./make_hourly -6 ./bak_minmax_c.csv bak_diurnal_c.csv

./ngfwi -6 85 6 15 ./bak_diurnal.csv test3.csv

./ngfwi -6 85 6 15 ./bak_windy.csv test4.csv
./ngfwi -6 85 6 15 ./bak_rh100.csv test5.csv
./ngfwi -6 85 6 15 ./bak_rh0.csv test6.csv
