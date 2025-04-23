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
    select(ID, event.title, publication, Volumes, issue.mo, issue.year, vol.title, event.type, event.month, event.year)

for(i in 1:nrow(eventswithmetadata)) {
    if (is.na(eventswithmetadata$event.month[i])) {
        eventswithmetadata$event.month[i] <- eventswithmetadata$issue.mo[i]
        eventswithmetadata$approx.date[i] <- TRUE
    }
    else if (eventswithmetadata$event.month[i] is.na = FALSE) {
    print("Event has date")
    }
}

#copilot
# Initialize the approx.date column with NA
eventswithmetadata$approx.date <- NA

# Loop to update event.month and approx.date
for (i in 1:nrow(eventswithmetadata)) {
    if (is.na(eventswithmetadata$event.month[i])) {
        eventswithmetadata$event.month[i] <- eventswithmetadata$issue.mo[i]
        eventswithmetadata$approx.date[i] <- TRUE
    } else {
        print("Event has date")
    }
}

for (i in 1:nrow(eventswithmetadata)) {
    if (is.na(eventswithmetadata$event.month[i])) {
        eventswithmetadata$event.month[i] <- eventswithmetadata$issue.mo[i]
        eventswithmetadata$approx.date[i] <- TRUE
    } else {
        print("Event has date")
    }
}
#getting Invalid Numbers for parts or all of the date when approx date is true - seems kinda random?

#To Do:
#- visualizations for presentation of dataset 
#- fix invalid numbers issues