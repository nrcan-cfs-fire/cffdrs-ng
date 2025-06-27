# NG-CFFDRS GitHub Repository
The [Canadian Forest Fire Danger Rating System (CFFDRS)](https://natural-resources.canada.ca/our-natural-resources/forests/wildland-fires-insects-disturbances/canadian-forest-fire-danger-rating-system/14470) is the principal source of fire information for all [wildland fire management agencies](https://ciffc.ca/mobilization-stats/member-agencies) across Canada. It has widespread use as a regional and fireline safety and awareness tool. The CFFDRS is undergoing extensive revisions under the name [Next Generation CFFDRS (NG-CFFDRS)](https://ostrnrcan-dostrncan.canada.ca/handle/1845/245411), with rollout to practitioners occurring during 2024, 2025, and beyond. This GitHub repository is home to the NG-CFFDRS code as it is developed by us, the [Canadian Forest Service](https://natural-resources.canada.ca/corporate/corporate-overview/canadian-forest-service) Fire Danger Group. For more details about implementation and development, check out: 

1. Our website at [cffdrs.github.io/website_en](https://cffdrs.github.io/website_en)
2. The 2021 document on [*An overview of the next generation of the Canadian Forest Fire Danger Rating System*](https://ostrnrcan-dostrncan.canada.ca/handle/1845/245411)
3. [Sign up to our newsletter](https://forms.office.com/r/jmT8HVrsK8) where you can specifically select to receive code updates 

This repository contains three programming languages: R, Python, and C (**all C code is still under development**). These three versions are written to produce the same results so users can choose the version that fits their own software systems.  

While we are not actively seeking code contributions at this time, see our [contributing page](https://github.com/nrcan-cfs-fire/cffdrs-ng/blob/main/CONTRIBUTING.md) for details on how to provide us feedback and report bugs.  

The next generation revisions to CFFDRS will be released in stages and will be collectively referred to as CFFDRS2025. The Fire Weather Index system (FWI2025) is the first module released.

## Repository Structure
The only branch of this repository with code intended for use is in the default *main* branch. All the code (in all three languages) is currently unorganized in the repository root directory. This will change and be **reorganized in Fall 2025**, with ample notice and warning. 

cffdrs-ng/ 
- data/ 
    - `test_hffmc.csv` 
    - `wx_hourly.csv` 
    - `wx_prf.csv` (test data of hourly weather recorded from the Petawawa Research Forest in 2007) 
- `.gitignore` 
- `CMakeLists.txt` 
- `CONTRIBUTING.md` (guidelines on providing feedback and reporting bugs) 
- `LICENSE.txt` (GPL 2.0 license) 
- `NG_FWI` [.c, .py, .r] (functions and equations for generating FWI2025 outputs) 
- `README.md` 
- `daily_summaries` [.py, .r] (function to generate the daily summary output) 
- `make_daily` [.c, .py, .r] (converts hourly weather data to daily data) 
- `make_hourly` [.c, .py, .r] (converts daily min/max weather data to hourly data) 
- `make_inputs` [.c, .py, .r] 
- `make_minmax` [.c, .py, .r] (converts daily weather data to daily min/max data) 
- `old_cffdrs` [.py, .r] 
- `test.sh` 
- `util` [.c, .h, .py, .r] (basic, intermediate functions that are not a part of FWI2025 equations)

## Revisions to FWI
FWI2025 remains a daily, landscape-level planning tool. Changes include improving daily weather tracking, reconciling different versions of codes and indices, and introducing new codes and indices.  

Detailed instructions on how to run FWI2025 scripts can be found on the Tutorials page of [the website](https://cffdrs.github.io/website_en). The first tutorial focuses on calculating hourly FWI codes and indices.

### Hourly Fire Weather Data Estimation
FWI2025 requires hourly weather data. Some weather stations, forecasts, and gridded datasets provide this directly. However, for sub-hourly sources, hourly values must be estimated before calculating hourly FWI by running the `NG_FWI` code. This repository provides methods for this conversion, including specific methods for daily 13:00 Local Daylight Time (LDT) observations as well as daily min/max forecasts:
- to convert daily min/max values to hourly data run `make_hourly` [.c, .py, .r]
- to convert daily 13:00 LDT (or noon standard time) values to daily min/max data run `make_minmax` [.c, .py, .r]. Follow this with the `make_hourly` method above to finally get hourly weather data
