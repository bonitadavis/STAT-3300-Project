# libraries
library(tidyverse)

# read in data
data = read.csv(file.choose())

# check data
head(data)
nrow(data)
ncol(data)

# select columns we need
data_sub = data %>% select(Speed.Limit,Weather,Surface.Condition,
                             Collision.Type,Injury.Severity)
head(data_sub)