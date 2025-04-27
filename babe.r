library(tidyverse)
library(tidytext)
library(readtext)
library(widyr)
library(SnowballC)
library(lubridate)
library(ggmap)
library(tidygeocoder)
library(ggplot2) 
library(leaflet)


metadata <- read.csv("babemetadata.csv", sep = ",", header = TRUE)
events <- read.csv("babeevents.csv", sep = ",", header = TRUE)
resources <- read.csv("baberesources.csv", sep = ",", header = TRUE)

#test plot - protests by months of 1970
protestsbyymonth70 <- events %>%
    filter(event.type == "Protest Report", event.year == 1970) %>%
    group_by(event.month) %>%
    summarize(count = n())

ggplot(protestsbyymonth70, aes(x = event.month, y = count)) + geom_col()

#put approx date from issue publication for entries with NA (month and year only)
#joined metadata with events, pared down to just the columns I want
eventswithmetadata <- full_join(events, metadata, join_by("Volumes" == "vol.ID"))

eventswithmetadata$approx.date <- NA

for (i in 1:nrow(eventswithmetadata)) {
    if (is.na(eventswithmetadata$event.month[i])) {
        eventswithmetadata$event.month[i] <- eventswithmetadata$issue.mo[i]
        eventswithmetadata$approx.date[i] <- TRUE
    } else {
        print("Event has month")
    }
}

for (i in 1:nrow(eventswithmetadata)) {
    if (is.na(eventswithmetadata$event.year[i])) {
        eventswithmetadata$event.year[i] <- eventswithmetadata$issue.year[i]
        eventswithmetadata$approx.date[i] <- TRUE
    } else {
        print("Event has year")
    }
}

#visualizations - dataset bio presentation
#types of events over time
eventtypes <- eventswithmetadata %>%
    separate_rows(event.type, sep = ",") %>% #split entries with multiple types into separate rows
    group_by(event.type, event.month, event.year) %>%
    summarize(count = n()) %>%
    mutate(event.date = make_date(year = event.year, month = event.month)) #create date with both for use in plots

ggplot(eventtypes, aes(x=factor(event.date), y=count, fill = event.type)) + 
    facet_wrap(~event.type, nrow = 1) + geom_col() + 
    labs(title = "Number of events in the Babe over time, faceted by type of event", x="Date", y="count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") 

ggsave("eventtypes_plot.png", width = 15, height = 5)

#types of(?) events over space
eventsgeo <- eventswithmetadata %>%
    select(ID, event.title, Volumes, event.type, event.month, event.date, event.year, event.location, event.address, event.city, event.state, event.country, source.publication, notes)

#have some dates with date and others without - to combine have to do two sep things
eventsgeo <- eventsgeo %>%
    mutate(date = if_else(is.na(event.date),
                         make_date(year = event.year, month = event.month),
                         make_date(year = event.year, month = event.month, day = event.date)))

#geocode - some have addresses - others do not (have to omit or do separately)
eventsaddcoord <- eventsgeo %>% 
    filter(event.address != "NA") %>%
    geocode(address = event.address, method = 'osm', lat = latitude, long = longitude)

eventscoord <- eventsgeo %>% 
    filter(is.na(event.address)) %>%
    geocode(city = event.city, state = event.state, country = event.country, method = 'osm', lat = latitude, long=longitude)

alleventscoords <- full_join(eventsaddcoord, eventscoord) %>%
    separate_rows(event.type, sep = ",")
#some with NA/Invalid - going to omit for this visualization but would eventually need to fix

usa <- map_data("state")

ggplot() + 
  geom_map(data = usa, map = usa, aes(x=long, y=lat, map_id=region), fill = "lightgray", color = "black") +
  geom_jitter(data = alleventscoords, mapping = aes(x = longitude, y = latitude, color = event.type, size = 7)) +
  labs(title = "Event Locations by Type")
#jitter not super effective but fine for this map

#few outliers messing with skew of map
#copilot - exclude based on USA specific bounds:
alleventscoords <- alleventscoords %>%
    filter(latitude >= 24 & latitude <= 50,  # Approximate bounds for the USA
           longitude >= -125 & longitude <= -66)
#same issue with duplicate types - for ggplot would want separated out - for leaflet could leave together

#leaflet
alleventscoords <- alleventscoords %>%
    select(ID, event.title, Volumes, event.type, 
           event.location, event.address, event.city, event.state, event.country, 
           latitude, longitude, notes, date)

eventmap <- leaflet(alleventscoords) %>%
    addTiles() %>%
    addMarkers(~longitude, ~latitude, popup = paste(alleventscoords$event.title, "-", alleventscoords$event.type, "- in", alleventscoords$Volumes, "at", alleventscoords$event.location, "(", alleventscoords$event.address, ",", alleventscoords$event.city, ",", alleventscoords$event.state, ")", alleventscoords$date, ":", alleventscoords$notes, sep = " "))
eventmap

#issue with recognizing column names: "Error: object 'event.title' not found" - copilot not being helpful
#not the end of the world - will circle back at some point - NEEDED THE DATA NAME AND A $ 

#trying to put on website - html file and then put onto post in hugo site repo
#copilot
install.packages("htmlwidgets")
library(htmlwidgets)
saveWidget(eventmap, "eventmap.html", selfcontained = TRUE) #saves leaflet as HTML file - selfcontained makes it portable
#not working - not worth more hours of tinkering

#repeat with resources
resourcesmetadata <- full_join(resources, metadata, join_by("Volumes" == "vol.ID"))
#resources over time
resourcetypes <- resourcesmetadata %>%
    separate_rows(type.resource, sep = ",") %>% #split entries with multiple types into separate rows
    group_by(type.resource, issue.mo, issue.year) %>%
    summarize(count = n()) %>%
    mutate(date = make_date(year = issue.year, month = issue.mo)) #create date with both for use in plots

ggplot(resourcetypes, aes(x=factor(date), y=count, fill = type.resource)) + 
    facet_wrap(~type.resource, nrow = 1) + geom_col() + 
    labs(title = "Number of resources in the Babe over time, faceted by type of event", x="Date", y="count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") 

ggsave("resourcetypes_plot.png", width = 15, height = 5)
#resources over space
resourcesgeo <- resourcesmetadata %>%
    select(ID, resource.title, Volumes, type.resource, resource.location, resource.address, resource.city, resource.state, source.publication, notes)

#geocode - some have addresses - others do not (have to omit or do separately)
resaddcoords <- resourcesgeo %>% 
    filter(resource.address != "NA") %>%
    geocode(address = resource.address, method = 'osm', lat = latitude, long = longitude)

rescoord <- resourcesgeo %>% 
    filter(is.na(resource.address)) %>%
    geocode(city = resource.city, state = resource.state, method = 'osm', lat = latitude, long=longitude)

allresourcecoords <- full_join(resaddcoords, rescoord) %>%
    separate_rows(type.resource, sep = ",")
#a lot more invalid here because of the addresses being weird

# ----------NOTES---------------------
#Row 21 - issue.year: NA
#blank row? probably just an error - will remove row and then run loop again and see if that fixes it
# Remove row 21 from the dataframe
eventswithmetadata <- eventswithmetadata[-21, ]
#still getting invalid number for all event years with TRUE approx dates even though the issue year is there
#separating loops for month and year - resetting dataframe to try again - FIXED IT - am genius

#copilot
# Initialize the approx.date column with NA
# Loop to update event.month and approx.date
#   getting Invalid Numbers for parts or all of the date when approx date is true - seems kinda random?
#ensure both are numeric before running loop
#add print() into loop to inspect values being assigned

#should not use mutate() within loop - direct assignment of columns instead (saving and pushing what i did before changing for visibility)
#   - didn't work - replaced those with dates with just the first of the month
#trying without loop - vectorized approach with mutate() - BASICALLY WHAT I HAD TO BEGIN WITH

#my notes
#4/27
#  - one issue I foresee running into is event types that fit multiple categories throwing off the data analysis (fized with separate_rows)
#  - date as factor to make it easier to see rather than a skinney line indicating 1st of month (as put into data frame)
#  - filtered to USA coordinates because of so few international entries - ease of viewing for presentation
#  - applied jitter to ggplot map for visibility at concentrated areas - not super effective but not worrying about minute details for this presentation


