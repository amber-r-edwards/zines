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
