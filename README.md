# covid19_socialdist_us

## COVID-19: An Exploration of Early Public Sentiment about Social Distancing through Twitter and Google Reports
The purpose of the project is to understand public sentiment about social distancing via Twitter data, across the United States, and correlate it to the extent to which US states are effectively social distancing.

### The Workflow
This repository contains:
Shiny_app, a folder containing the actual Shiny app (`app.r` which powers my app), requisite raw data, and Shiny-ready processed data
Data Processing, a folder containing all .rmd files in which I processed dataframes and user tables for Shiny_app

### Notes on the Project

#### The App
Link: https://arushisax.shinyapps.io/COVID19_socialdistancing/

#### 1: Tweet Analysis
Subtabs include an interactive Word Cloud tab and a Sentiment Analysis tab. The data processing for each tab is conducted in socialdist_word_cloud.rmd and tweet_sentanalysis.rmd respectively.
I used `textmineR` and `tm` packages for text mining in preparation for a word cloud, `snowballC` to process the words for sentiment analysis, and the `syuzhet` package for actual sentiment analysis. 
#### 2: Geographic Analysis
This map is built with `leaflet` to be interactive. The data processing for this tab was conducted in sent_analysis_mapdata.rmd and uses `rtweet` to pull latitude and longitude for each Tweet based on Twitter user’s stated location. 
#### 3: Google Search Trends
This tab utilizes `ggplot2` and `dplyr` libraries to create traditional scatterplots and `ggpubr` to publish significance values and correlation coefficients on the charts. The x-axis data is from Google Social Community Mobility Reports (https://www.google.com/covid19/mobility/) meanwhile the y-axis data is from Google Trends.   

#### Technical Details
This project is written in R and it's UI is built with RShiny to be interactive and reactive. I use rtweet to scrape twitter data (more on rtweet here: https://rtweet.info/) in the gather_covid_data.rmd file. 



