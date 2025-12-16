# NG-CFFDRS GitHub Repository
The [Canadian Forest Fire Danger Rating System (CFFDRS)](https://natural-resources.canada.ca/our-natural-resources/forests/wildland-fires-insects-disturbances/canadian-forest-fire-danger-rating-system/14470) is the principal source of fire information for all [wildland fire management agencies](https://ciffc.ca/mobilization-stats/member-agencies) across Canada. It has widespread use as a regional and fireline safety and awareness tool. The CFFDRS is undergoing extensive revisions under the name [Next Generation CFFDRS (NG-CFFDRS)](https://ostrnrcan-dostrncan.canada.ca/handle/1845/245411), with rollout to practitioners occurring during 2024, 2025, and beyond. This GitHub repository is home to the NG-CFFDRS code as it is developed by us, the [Canadian Forest Service](https://natural-resources.canada.ca/corporate/corporate-overview/canadian-forest-service) Fire Danger Group. For more details about implementation and development, check out: 

1. Our website at [cffdrs.github.io/website_en](https://cffdrs.github.io/website_en)
2. The 2021 document on [*An overview of the next generation of the Canadian Forest Fire Danger Rating System*](https://ostrnrcan-dostrncan.canada.ca/handle/1845/245411)
3. [Sign up to our newsletter](https://forms.office.com/r/jmT8HVrsK8) where you can specifically select to receive code updates 

This repository contains three programming languages: C, Python, and R. These three versions are written to produce the same results so users can choose the version that fits their own software systems.  

While we are not actively seeking code contributions at this time, see our [contributing page](https://github.com/nrcan-cfs-fire/cffdrs-ng/blob/main/CONTRIBUTING.md) for details on how to provide us feedback and report bugs.  

The next generation revisions to CFFDRS will be released in stages and will be collectively referred to as CFFDRS2025. The Fire Weather Index system (FWI2025) is the first module released.

## Repository Structure
The only branch of this repository with code intended for use is in the default *main* branch. The code is organized into CFFDRS Subsystems followed by programming language in the following structure:  

cffdrs-ng/
- data/
    - `PRF2007_hourly_wx_C-format.csv` (hourly weather test data recorded at Petawawa Research Forest in 2007 for C)
    - `PRF2007_hourly_wx.csv` (hourly weather test data recorded at Petawawa Research Forest in 2007)
    - `PRF2007_standard-daily_summaries.csv` (sample output file to compare daily summaries)
    - `PRF2007_standard_hourly_FWI` (sample output file to compare FWI2025) 
- FWI/
    - C/
        - tutorial/
            - `tutorial_hourly_FWI.ps1`
        - `daily_summaries.c` (function to generate the daily summary output)
        - `make_hourly.c` (converts daily min/max weather data to hourly data)
        - `make_minmax.c` (converts daily weather data to daily min/max data)
        - `NG_FWI_main.c` (`main()` function for generating FWI2025 outputs)
        - `NG_FWI.c` (functions and equations for generating FWI2025 outputs)
        - `NG_FWI.h`
        - `util.c` (basic, intermediate functions that are not a part of FWI2025 equations)
        - `util.h`
    - Python/
        - tutorial/
            - `tutorial_hourly_FWI.py`
        - `daily_summaries.py` (function to generate the daily summary output)
        - `make_hourly.py` (converts daily min/max weather data to hourly data)
        - `make_minmax.py` (converts daily weather data to daily min/max data)
        - `NG_FWI.py` (functions and equations for generating FWI2025 outputs)
        - `util.py` (basic, intermediate functions that are not a part of FWI2025 equations)
    - R/
        - tutorial/
            - `tutorial_hourly_FWI.r`
        - `daily_summaries.r` (function to generate the daily summary output)
        - `make_hourly.r` (converts daily min/max weather data to hourly data)
        - `make_minmax.r` (converts daily weather data to daily min/max data)
        - `NG_FWI.r` (functions and equations for generating FWI2025 outputs)
        - `util.r` (basic, intermediate functions that are not a part of FWI2025 equations)
- `.gitignore`
- `CHANGELOG.md`
- `CONTRIBUTING.md` (guideline on providing feedback and reporting bugs)
- `LICENSE.txt` (GPL 2.0 license)
- `README.md`
- `SECURITY.md`

## Revisions to FWI
FWI2025 remains a daily, landscape-level planning tool. Changes include improving daily weather tracking, reconciling different versions of codes and indices, and introducing new codes and indices.  

Detailed instructions on how to run FWI2025 scripts can be found on the [Tutorials page](https://cffdrs.github.io/website_en/tutorials) of the website. The first tutorial focuses on calculating hourly FWI codes and indices.

### Hourly Fire Weather Data Estimation
FWI2025 requires hourly weather data. Some weather stations, forecasts, and gridded datasets provide this directly. However, for sub-hourly sources, hourly values must be estimated before calculating hourly FWI by running the `NG_FWI` code. This repository provides methods for this conversion, including specific methods for daily 13:00 Local Daylight Time (LDT) observations as well as daily min/max forecasts:
- to convert daily min/max values to hourly data run `make_hourly` [.c, .py, .r]
- to convert daily 13:00 LDT (or noon standard time) values to daily min/max data run `make_minmax` [.c, .py, .r]. Follow this with the `make_hourly` method above to finally get hourly weather data
