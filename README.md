
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MsdrCapstoneMPS

## Overview

This project is the capstone project for the Coursera Specialization
*Mastering Software Development in R*, offered by The Johns Hopkins
University. For more information on the specialization, you can view the
overview page on Coursera at:
<https://www.coursera.org/specializations/r>

The goal of the capstone project is to build an R package with tools to
assist in analyzing historical earthquake data provided by the U.S.
National Oceanic and Atmospheric Administration (NOAA). The [NOAA
Significant Earthquake
Database](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1)\[1\]
contains information on destructive earthquakes from 2150 B.C. to the
present that meet at least one of the following criteria: Moderate
damage (approximately $1 million or more), 10 or more deaths, Magnitude
7.5 or greater, Modified Mercalli Intensity X or greater, or the
earthquake generated a tsunami.

The full database [can be downloaded as a tab-delimited text
file](https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt),
and [variable definitions are available
online](https://www.ngdc.noaa.gov/nndc/struts/results?&t=101650&s=225&d=225)
also.

\[1\] National Geophysical Data Center / World Data Service (NGDC/WDS):
Significant Earthquake Database. National Geophysical Data Center, NOAA.
[doi:10.7289/V5TD9V7K](http://dx.doi.org/10.7289/V5TD9V7K)

## Function Summary

There are 2 data cleaning functions:  
\* `eq_clean_data()` cleans latitude, longitude, and date  
\* `eq_location_clean()` cleans location

There are 3 functions to plot earthquakes over time:  
\* `geom_timeline()` plots a timeline with circles showing the dates
when the earthquakes occurred  
\* `theme_earthquake()` creates the appropriate theme to plot the
earthquake timelines  
\* `geom_timeline_labels()` labels the timeline by drawing vertical
lines up from the points on the timeline along with labels

There are 2 functions to build interactive maps that use leaflet:  
\* `eq_map()` creates an interactive map of earthquakes, with
earthquakes plotted as circles and optional labels  
\* `eq_create_label()` builds html labels for earthquakes that appear on
the interactive map

## Installation

You can install the development version of MsdrCapstoneMPS from github
with:

``` r
devtools::install_github("marksendak/MsdrCapstoneMPS")
```

If you have any questions, comments, or feedback, please email
<mark.sendak@gmail.com>.
