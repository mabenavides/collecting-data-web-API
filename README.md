# Collecting data from the web using an API

## Author: Maria Benavides 

In this repository, I, first, analysed the correlation between life expectancy and population density using gapminder and geonames. Also, I 
wrote an API query function to access data of the City of Chicago, to understand how crime changed after the shelter-in-place measure was established. 

### Libraries needed

* tidyverse
* geonames, for accesing the data
* countrycode, to create key for merging data sets (in gapminder exercise)
* broom
* kableExtra
* RSocrata, for API to access Chicago Open Data  

### Files

* [gapminder.md](gapminder.md): analysis of correlation between life expectancy and population density
* [gapminder.Rmd](gapminder.Rmd): rendered version of previous file
* [gapminder_files](gapminder_files): plots generated for the gapminder exercise
* [API_query.md](API_query.md): code to query the City of Chicago server and some analysis
* [API_query.Rmd](API_query.Rmd): rendered version of previous file
* [API_query_files](API_query): plots generated for the API query exercise
* [chicago_crime_2020.csv](chicago_crime_2020.csv): dataframe stored in the repository
