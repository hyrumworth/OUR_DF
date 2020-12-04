# packages 
library(tidyr)
library(dplyr)

# Here I am combining the HT csv files into 1
y2013 <- read.csv("Year_2013.csv")
y2014 <- read.csv("Year_2014.csv")
y2015 <- read.csv("Year_2015.csv")
y2016 <- read.csv("Year_2016.csv")
y2017 <- read.csv("Year_2017.csv")
y2018 <- read.csv("Year_2018.csv")
y2019 <- read.csv("Year_2019.csv")

HT_COMBINED <- rbind(y2013,y2014,y2015,y2016,y2017,y2018,y2019)

FBI_HT_COMBINED <- write.csv(HT_COMBINED, "HT_COMBINED.csv")


# me just messing around with the format that Neilson wants 

