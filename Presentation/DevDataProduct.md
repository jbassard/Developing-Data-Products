<style>
.footer {
    color: black;
    background: #E8E8E8;
    position: fixed;
  }
</style>

<style>
.small-code pre code {
  font-size: 1em;
}
</style>

Developping Data Product - Coursera Course Project
========================================================
author: jbassard
date: 09/03/2017
autosize: true
transition: rotate

Shiny app to navigate in NOAA's Database
========================================================
class: footer

This presentation is part of the assignment for the Coursera course on Developping Data Product in the Data Science Specialization.

This Course Project was about to:
- Produce a Shiny app
- Prepare a companion presentation with R-presentation for example

About this shiny app
========================================================
class: footer

This application is based on the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database.
Weather events can cause both public health and economic problems for communities. Many severe events can result in fatalities, injuries, and property damage, this NOAA's dataset isa record of events from 1950 ot 2011 in all US states.

Dataset has been obtained from the Coursera Reproducible Research Course site [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2). Data has been cleaned up. For clarity, events have been combined based on term similarity leading to 24 types of event from the initial 985 in the original database. Code is available on [GitHub](https://github.com/jbassard/Developing-Data-Products) "Preparing_Data.Rmd".

Source code for this project is available on [GitHub](https://github.com/jbassard/Developing-Data-Products).

The presentation for this project can be found on RPubs [here](http://rpubs.com/jbassard/303924).


Importance of the NOAA's Database
========================================================
class: small-code
class: footer

Injuries, fatalities and economical impacts arise each year from extreme weather events that are recorded in the  NOAA's database.

```r
if (!require("data.table")) {install.packages("data.table")}
library(data.table)
data <- read.csv('./DataProductProject/data/events.csv', header=TRUE, stringsAsFactors=FALSE)
print(paste("This NOAA's database is made of " , dim(data)[1], "number of records assigned to ", length(unique(data$EVTYPE)), "major event categories, which have been limited in numbers for readability of the data one the Shiny app"))
```

```
[1] "This NOAA's database is made of  902042 number of records assigned to  24 major event categories, which have been limited in numbers for readability of the data one the Shiny app"
```


What the app is doing
========================================================
class: footer

This app is designed to explore the NOAA's database:

+ You can display NOAA's data on a map using the "Plot Data by US State" tab or plot the data as a time serie using "Plot Data by Year" tab.


+ You can adjust dates range and weather event types using control panels located on the left side of the App.
Then the results are shown in the main pannel at the center of the page displaying data only using the selected filters.

+ Selected dataset can be downloaded using "Download Selected Data" tab.


