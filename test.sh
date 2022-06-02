gcc -o ngfwi NG_FWI.c -lm -std=c90 || exit
./ngfwi -6 85 6 15 ./bak_hourly.csv test.csv
./ngfwi -6 85 6 15 ./input_hffmc.csv test2.csv
./ngfwi -6 85 6 15 ./bak_diurnal.csv test3.csv

gcc -o make_daily make_daily.c -lm -std=c90
./make_daily ./bak_hourly.csv bak_daily_c.csv
