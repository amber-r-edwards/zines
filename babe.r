library(tidyverse)
library(tidytext)
library(readtext)
library(widyr)
library(SnowballC)


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
eventswithmetadata <- full_join(events, metadata, join_by("Volumes" == "vol.ID")) %>%
    select(ID, event.title, publication, Volumes, issue.mo, issue.year, vol.title, event.type, event.month, event.year, notes)

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

#To Do:
#- visualizations for presentation of dataset 
#- fix invalid numbers issues