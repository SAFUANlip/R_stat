library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(nlme)

# Particulate Matter (PM) refers to tiny particles or droplets in the air that can be inhaled into the lungs and may
# cause serious health problems. PM10 specifically refers to inhalable particles with diameters that are generally
# 10 micrometers and smaller. These particles can come from various sources, including vehicle exhaust, industrial
# emissions, construction activities, and natural sources such as dust storms and wildfires.
# The Lombardia region is interested in modeling the distribution of PM10 concentration over its entire territory
# to assess air quality and potential health risks.
# The file pm10.txt contains measurements of daily maximum concentration z of PM10 [µg/m3] recorded on July
# 14th, 2023, within 160 locations in Lombardia. The locations sk (k = 1, ..., 160) are expressed in UTM coordinates
# (variables x and y). Additionally, an ”urbanization measure” of the area surrounding the location through the
# numerical variable urban ranging from 0 to 3 is provided. The variable urban is built as follow: urban = 3 inside
# an agglomeration, urban = 2 in an urbanized plain, urban = 1 in an low-urbanized plain and urban = 0 in an a
# mountain or forest area.
# For all questions, 

