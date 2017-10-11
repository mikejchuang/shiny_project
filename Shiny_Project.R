library(dplyr)
library(ggplot2)
library(data.table)
library(R.utils)
raw_data_full <- fread("./Service_Requests_311.csv", stringsAsFactors = F, nrows=5000)
#raw_data_full <- as.data.frame(raw_data_full)

nL <- countLines('./311_Service_Requests_from_2015.csv')

raw_slice = fread('./311_Service_Requests_from_2015.csv',select = c("Unique Key","Incident Zip","Incident Address", "Created Date", "Complaint Type",'Descriptor','Borough', 'Latitude','Longitude'))
names(raw_slice) <-  tolower(names(raw_slice))
names(raw_slice) <- gsub(" ", '_', names(raw_slice))

col_groups <- c('heating', 'noise','construction','plumbing', 'paint', 'unsanitary', 'car_related','street_lights_signals','tree','electric','vermin','street_sidewalk')

###consolidate complaint types
raw_slice <- raw_slice %>%
  filter(., !is.na(longitude), longitude > -77, !incident_address=="") %>%
  #filter(., !incident_address=="") %>%
  mutate(.,complaint_type=gsub('(.*heat.*)', replacement=col_groups[1], x=complaint_type, ignore.case=TRUE)) %>% 
  mutate(.,complaint_type=gsub('(.*noise.*)', replacement=col_groups[2], x=complaint_type, ignore.case=TRUE))  %>%
  mutate(.,complaint_type=gsub('(.*construction.*)', replacement=col_groups[3], x=complaint_type, ignore.case=TRUE)) %>% 
  mutate(.,complaint_type=gsub('(.*plumbing*|.*water sys.*|.*leak.*)', replacement=col_groups[4], x=complaint_type, ignore.case=TRUE))%>%
  mutate(.,complaint_type=gsub('(.*paint.*)', replacement=col_groups[5], x=complaint_type, ignore.case=TRUE)) %>%
  mutate(.,complaint_type=gsub('(.*sani.*|.*dirty.*)', replacement=col_groups[6], x=complaint_type, ignore.case=TRUE)) %>%
  mutate(.,complaint_type=gsub('blocked driveway|Illegal Parking|Traffic$', replacement=col_groups[7], x=complaint_type, ignore.case=TRUE)) %>%
  #mutate(.,complaint_type=gsub('.*(Street Sign).*|.*(Street Light).*|.*signal.*', replacement=col_groups[8], x=complaint_type, ignore.case=TRUE)) %>%
  mutate(.,complaint_type=gsub('((.*\\btree.*)|(branch))', replacement=col_groups[9], x=complaint_type, ignore.case=TRUE))%>%
  #mutate(.,complaint_type=gsub('(.*electric.*)', replacement=col_groups[10], x=complaint_type, ignore.case=TRUE))%>%
  mutate(.,complaint_type=gsub('(.*rodent.*)|(.*nonconst.*)', replacement=col_groups[11], x=complaint_type, ignore.case=TRUE)) %>%
  mutate(.,complaint_type=gsub('(.*sidewalk.*)|(.*street\\b).*', replacement=col_groups[12], x=complaint_type, ignore.case=TRUE)) %>% 
  filter(., complaint_type %in% col_groups) %>%
  mutate(.,weekday = weekdays(as.Date(created_date,format = "%m/%d/%Y")),
         year = year(as.Date(created_date,format = "%m/%d/%Y")), 
         hour = hour(parse_date_time(created_date, 'mdY H!MS p' )),
         month = month(as.Date(created_date,format = "%m/%d/%Y")), 
         weeknum = week(as.Date(created_date,format = "%m/%d/%Y")),
         yday = yday(as.Date(created_date,format = "%m/%d/%Y")))

raw_slice[raw_slice==""] <- NA
raw_slice <- na.omit(raw_slice)
raw_slice$weekday <- factor(raw_slice$weekday,levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

write.csv(raw_slice, file='shiny_data.csv')


View(Fruits)
topfew <- raw_slice %>% group_by(.,complaint_type) %>% summarize(., count=n()) %>% arrange(.,count)

test <- raw_slice %>% filter(., longitude <= -77)
raw_slice

#Motion Chart Test
raw_slice_motion <- raw_slice %>% group_by(., gvisdate, complaint_type) %>%
  mutate(.,count=n())
rsm <- raw_slice_motion[1:300,c('unique_key','created_date','complaint_type', 'count')]
rsm[rsm==""]
row.names(rsm)
rsm$created_date <- as_date(rsm$created_date)
gm <- gvisMotionChart(rsm, idvar="unique_key", timevar='created_date')
plot(gm)

####Most common hour for calls to be made
library(lubridate)
lubridate_test <- parse_date_time(raw_slice$created_date, 'mdY H!MS p' )
hour(lubridate_test)

PerMonth<- raw_slice  %>% mutate(month = month(as.Date(created_date,format = "%m/%d/%Y"))) %>% 
  group_by(., month) %>% summarize(.,count=n())
plot_ly(PerMonth,x=~month, y=~count ,type='scatter', mode='lines')


PerHour<- raw_slice %>% group_by(.,created_date)  %>% summarize(.,count=n()) %>%
  mutate(day = weekdays(as.Date(created_date,format = "%m/%d/%Y")),year = year(as.Date(created_date,format = "%m/%d/%Y")), hour = hour(parse_date_time(created_date, 'mdY H!MS p' )))  %>% 
  filter(., !substr(created_date,12,22)=='12:00:00 AM') %>% group_by(.,hour) %>% summarize(., sum=sum(count))                                                                          
library(plotly)
Hourplot <- gvisLineChart(PerHour)
plot_ly(PerHour,x=~hour, y=~sum ,type='scatter', mode='lines')
# Plot each day by hour on the same graph

# PerDay <- raw_slice %>% group_by(.,created_date)  %>% summarize(.,count=n()) %>%
#   mutate(day = weekdays(as.Date(created_date,format = "%m/%d/%Y")),year = year(as.Date(created_date,format = "%m/%d/%Y")), hour = hour(parse_date_time(created_date, 'mdY H!MS p' ))) %>% group_by(.,day) %>% summarize(.,daysum=sum(count)) 
# 
# Over_Day$day <- factor(Over_Day$day,levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))
# 
# View(Over_Day$day)
# 
# Dayplot <-  gvisLineChart(Over_Day[order(Over_Day$day),])
# plot(Dayplot)

Over_HourDay <- raw_slice %>% group_by(.,created_date)  %>% summarize(.,count=n()) %>% filter(., !substr(created_date,12,22)=='12:00:00 AM') %>%
  mutate(day = weekdays(as.Date(created_date,format = "%m/%d/%Y")), hour = hour(parse_date_time(created_date, 'mdY H!MS p' )))  %>% mutate(., new_col=paste(hour,day,sep="")) %>% group_by(., new_col, day)  %>% summarize(.,weeksum=sum(count))# %>% arrange(., day, hour)

library(plotly)
x <- outer(c(0:23), c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'), FUN = "paste0")

Over_HourDay$day <- factor(Over_HourDay$day,levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))
Over_HourDay$new_col <- factor(Over_HourDay$new_col,levels=c(x),ordered = TRUE)
length (x2)
Hour_day_plot <- gvisLineChart(Over_HourDay[order(Over_HourDay$day),], xvar="day")
plot(Hour_day_plot)
p <- plot_ly(Over_HourDay, x = ~new_col, y = ~weeksum, type = 'scatter', mode = 'smooth',line = list(color = 'rgb(100, 100, 200)', width = 2))
p

###### Most Common Addresss:
top_addresses <- raw_slice %>% group_by(., latitude, longitude,complaint_type, incident_address) %>% summarize(., count=n()) %>%
  group_by(., incident_address) %>% mutate(., total=sum(count)) %>% arrange(.,desc(total))
View(top_addresses)
library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)
library(leaflet)
library(maps)
top_ad <- leaflet(head(top_addresses,20)) %>%
  addTiles() %>%
  addCircleMarkers(radius = top_addresses$count*.1)
top_ad

View(head(top_addresses,20))

###extra testing
sum(desc_fast[12:nrow(desc_fast),2])/sum(desc_fast[1:10,2])
x123 <- desc_fast[13:nrow(desc_fast),]

table(is.na(raw_slice$complaint_type))
table(is.na(raw_slice$longitude))
table(raw_slice$incident_address[raw_slice$incident_address==""])
library(leaflet.extras)

leaflet()%>%
  addProviderTiles(providers$Stamen.Toner) %>%  
  setView(lat = 40.73, lng = -73.92,zoom = 11) %>%
  addWebGLHeatmap(data=raw_slice, lng=~longitude, lat=~latitude, size=75, alphaRange=0.01)


leaflet(raw_slice) %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions(), popup = ~paste('<b><font color="Black">','Test','</font></b><br/>',
                                                                     'Longitude:', longitude,'<br/>',
                                                                     'Latitude:', latitude,'<br/>',
                                                              'Zip', incident_zip))
