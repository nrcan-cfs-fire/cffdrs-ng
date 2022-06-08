gcc -o ngfwi NG_FWI.c util.c -lm -std=c90 || exit
./ngfwi -6 85 6 15 ./bak_hourly.csv test.csv
./ngfwi -6 85 6 15 ./input_hffmc.csv test2.csv

gcc -o make_daily make_daily.c util.c -lm -std=c90 || exit
./make_daily ./bak_hourly.csv bak_daily_c.csv

gcc -o make_minmax make_minmax.c util.c -lm -std=c90 || exit
./make_minmax ./bak_daily.csv bak_minmax_c.csv

gcc -o make_hourly make_hourly.c util.c -lm -std=c90 || exit
./make_hourly -6 ./bak_minmax_c.csv bak_diurnal_c.csv

./ngfwi -6 85 6 15 ./bak_diurnal.csv test3.csv

./ngfwi -6 85 6 15 ./bak_windy.csv test4.csv
./ngfwi -6 85 6 15 ./bak_rh100.csv test5.csv
