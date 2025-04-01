# TODO: encode variables as binary variables

# libraries
library(tidyverse)

# read in data
data = read.csv(file.choose())

# check data
head(data)
nrow(data)
ncol(data)

# select columns we need
data = data %>% select(Speed.Limit,Weather,Surface.Condition,
                             Collision.Type,Injury.Severity)
head(data)
ncol(data)

# check for NA rows
data = na.omit(data)
nrow(data) # no NA rows

# remove unknown data
data = data[data$Weather != "N/A" 
            & data$Surface.Condition != "N/A" 
            & data$Collision.Type != "OTHER",]
nrow(data)

# make ordinal index for Weather attribute
data$Weather.Index = ifelse(data$Weather=="CLEAR",1,
                  ifelse(data$Weather=="CLOUDY",2,
                  ifelse(data$Weather=="RAINING",3,
                  ifelse(data$Weather=="SNOW",4,5))))

