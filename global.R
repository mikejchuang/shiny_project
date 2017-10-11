## global.R ##

# convert matrix to dataframe
#state_stat <- data.frame(state.name = rownames(state.x77), state.x77)
# remove row names
#rownames(state_stat) <- NULL
# create variable with colnames as choice
#choice <-col_groups

##save data file as .rda here
library(data.table)
library(plotly)
library(leaflet)
library(shiny)
library(DT)
library(shinydashboard)
library(dplyr)
library(ggplot2)

raw_slice = fread('./shiny_data.csv')
raw_slice[raw_slice==""] <- NA
raw_slice <- na.omit(raw_slice)
raw_slice$weekday <- factor(raw_slice$weekday,levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

weekday_ <- unique(raw_slice$weekday)
hour_ <- unique(raw_slice$hour)
month_ <- unique(raw_slice$month)
complaint_ <- unique(raw_slice$complaint_type)

