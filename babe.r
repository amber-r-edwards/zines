library(tidyverse)
library(tidytext)
library(readtext)
library(widyr)
library(SnowballC)

metadata <- read.csv("babemetadata.csv", sep = ",", header = TRUE)
events <- read.csv("babeevents.csv", sep = ",", header = TRUE)
resources <- read.csv("baberesources.csv", sep = ",", header = TRUE)
