# Interim Revisions  to the CFFDRS
The Canadian Forest Fire Danger Rating System (CFFDRS) is the principal source of fire information for all wildland fire management agencies across Canada.  It has widespread use as a regional and fireline safety and awareness tool. The CFFDRS is undergoing extensive revisions under the name Next Generation CFFDRS (NG-CFFDRS), with rollout to practitioners occurring during 2024, 2025, and beyond. The primary goal is to meet the evolving needs of advanced users while maintaining it’s simplicity and effectiveness.  For a full list of revision topics and more discussion, see the 2021 *An overview of the next generation of the Canadian Forest Fire Danger Rating System* at https://ostrnrcan-dostrncan.canada.ca/handle/1845/245411 and French at https://ostrnrcan-dostrncan.canada.ca/entities/publication/4666ae3e-bcd4-4012-b6ef-d8a86ea3bb22?fromSearchPage=true .
The revisions to CFFDRS will be released in stages and are referred to as CFFDRS205.  The Fire Weather Index System (FWI2025) is the first module.
Users will see this repository contains three programming languages: R, Python, and C. These three versions are all written to produce the same result and were created so the user can choose the version that fits their own software systems.
News, resources and information about CFFDRS2025 can be found on the Next Generation CFFDRS github.io https://cffdrs.github.io/website_en .

## Revisions to the Fire Weather Index System
The next generation Fire Weather Index System (FWI2025) remains a daily, landscape level planning tool.  Changes aim to better track daily weather, align versions of System’s codes and indices, and introduce new codes and indices.  
Details on changes to the FWI2025 on the Resources  pages.
### Run FWI2025 (Next Generation FWI System)
Detailed instructions on how to run FWI2025 System scripts can be found on our Tutorials page.
To generate FWI2025 System estimates of fire danger, two scripts are required.
1.	NG_FWI (.r, or .py, or .c) : Holds the functions and equations for generating FWI2025 outputs.
2.	util (.r, or .py, or .c and .h) : Includes basic functions that are not part of FWI2025 equations, but generate intermediate information for the calculation of the FWI2025 components.

### Hourly Fire Weather 
FWI2025 requires hourly weather data. Some weather stations, forecasts, and gridded datasets provide this directly. However, for non-hourly sources (e.g., daily min/max forecasts, single daily observations at 1300h), hourly values must be estimated before running the NG_FWI code. This repository provides methods for this conversion, including specific methods for 1300h observations. Users with other non-hourly datasets must similarly convert them to hourly data before using them with FWI2025.
To estimate hourly weather from daily min/max or single 1300h observations:
- daily min/max values:  make_hourly.r, make_hourly.py OR make_hourly.c. Instructions on how to use the scripts and their inputs can be found in our documentation pages.  
- daily noon values : make_minmax.r, make_minmax.py OR make_minmax.c.  Instructions on how to use the scripts and their inputs can be found in our documentation  pages.  

## Feedback and Bug Reports
We welcome your feedback and bug reports!  While we're not actively seeking code contributions at this time, your input is invaluable for improving the project.  If you encounter any issues, have suggestions for improvements, or simply want to share your experience with the scripts, please let us know.
How to Provide Feedback:
The best way to provide feedback is by [creating an issue]( https://github.com/nrcan-cfs-fire/cffdrs-ng/issues) on this GitHub repository.  When submitting an issue, please include the following information:
Clear and concise description of the issue:  Explain the problem you encountered or the suggestion you have.
Steps to reproduce the issue (if applicable):   Provide specific instructions on how to recreate the problem.
 Relevant code snippets (if applicable):  Include any code that might be helpful in understanding the issue.
 Operating system and environment details:  Let us know the operating system you're using (e.g., Windows, macOS, Linux) and any relevant software versions (e.g., Python version).
 Expected behavior and actual behavior:  Describe what you expected to happen and what actually happened.
We appreciate you taking the time to provide feedback.  We'll do our best to respond to your issues and incorporate your suggestions. 
**Note:**  While we appreciate feedback and bug reports, we cannot guarantee that all suggestions will be implemented.  We will, however, carefully consider all input.



