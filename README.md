# Interim Revisions to the CFFDRS

The CFFDRS is undergoing extensive revisions, with rollout to practitioners occurring during 2024, 2025, and beyond.  The primary goal is to meet the evolving needs of advanced users, while maintaining the simplicity adn effectiveness of the CFFDRS for its widespread use as a regional and fireline safety and awareness tool.   For a full list of revision topics and more discussion, see the 2021 *An overview of the next generation of the Canadian Forest Fire Danger Rating System* at http://cfs.nrcan.gc.ca/publications?id=40474.

## Revisions to the Fire Weather Index System
### Hourly Fire Weather backbone

One of the larger changes to the FWI is a shift towards hourly FWI outputs.  Recognizing that hourly input data is not always available, the NG-FWI allows the user to choose from a variety of input options:

- hourly weather
- daily min/max values
- daily noon values

Thie code repository details the algorithm and provides the source code to any interesed users.  Look for more documentation on the NG-FWI in early 2024 and onwards.

GOALS

take hourly weather and produce hourly fwi indices
- determine wetting and drying portions for DC and DMC
- subdivide wetting based on rain proportion
- subdivide drying based on vapour pressure during sunlight hours(?)

take daily min/max and produce hourly weather
- use beck & trevitt equations plus default (or provided) a/b/g values

take daily noon values and produce daily min/max
- use rules of thumb based on statistics
- by area, or just for the whole country?
