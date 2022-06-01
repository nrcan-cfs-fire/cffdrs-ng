gcc -o ngfwi NG_FWI.c -lm -std=c90 || exit
./ngfwi -6 85 6 15 ./bak_hourly.csv test.csv
./ngfwi -6 85 6 15 ./input_hffmc.csv test2.csv
./ngfwi -6 85 6 15 ./bak_diurnal.csv test3.csv
