# Developer's guide: Youth Rex / Stepping Up

Youth Compass Stepping Up Web Application

## Data sources

### Geographic data

- Census division shapefile was retrieved from http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm on 2017-11-03. Its filename is `gcd_000b11a_e`.  
- Census sub-division shapefile was retrivied from http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm on 2017-11-03. Its filename is `gcsd_000b11a_e`. 
- Province and sub-province boundaries were retrieved via the raster package, from the GADM database, on 2017-10-25.  

### Census data  

Customized Canadian census data for 2001, 2006, 2011, and 2016 has been purchased by YouthREX and is stored (in a modified form) privately in the `databrew/Conulting Projects/york university/census_data` google drive folder. The files were originally saved in .ivt format, and were opened by downloading the Statistics Canada's Beyond 20/20 Professional Browser Software on a Windows machine. The Beyond 20/20 Browser can be downloaded here: https://www.statcan.gc.ca/eng/public/beyond20-20

## The application

### Location
Not hosted yet

### Use

## Developer's guide 

### Task 1: Get data
get link from drive folder with just census data
### Task 2: Run global.R, save data (optional)

Run global.R and it will save the data to your data folder (outside of census_data). Alternatively, the first time you run the app, `global.R` will automatically be called.

### Task 3: Start app

Now that you have the final data set with all info start a new branch to work on your app off of `global.R`. 


Contact
=======

[Databrew](http://www.databrew.cc), empowering researchers in academia and industry to explore, understand, and communicate their data through consulting and teaching. <a href="mailto:info@databrew.cc?Subject=Hello" target="_top">info@databrew.cc</a>.

<img align="center" src="logo_clear.png" alt="http://databrew.cc">
