Provide ability to calculate hourly indices from:
- hourly weather
- daily min/max values
- daily noon values





PERCEIVED GOALS

take hourly weather and produce hourly fwi indices
- determine wetting and drying portions for DC and DMC
- subdivide wetting based on vapour pressure
- subdivide drying based on solar radiation(?) during sunlight hours(?)



take daily min/max and produce hourly weather
- use beck & trevitt equations plus default (or provided) a/b/g values


take daily noon values and produce daily min/max
- determine rules of thumb based on statistics
- by area, or just for the whole country?



take hourly weather and daily weather and produce a/b/g for beck & trevitt equations
- geographically
- by month
- by index
- is a/b/g or sinusoidal better for rh or just do dew point and temperature?



determine best method of doing beck & trevitt type equations for each weather index
- fit beck & trevitt equations
- look at which equations have the least variance in a/b/g values geographically and by month
- compare predictions from each equation and fitted a/b/g to actual values to determine which performs best



demo of hourly indices for ontario weather
- get hourly weather from ontario service
- get predictions from ontario service
- convert predictions to hourly values
- run hourly fwi indices for past and future hourly weather
- show difference in fwi values based on predictions vs actual vs daily

