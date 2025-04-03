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

#convert InjurySeverity to binary indicator
data$InjuryBinary = ifelse(data$Injury.Severity == "NO APPARENT INJURY", 0, 1)

data$SurfaceStandard <- case_when(
  str_detect(toupper(data$Surface.Condition), "DRY") ~ "DRY",
  str_detect(toupper(data$Surface.Condition), "WET|WATER") ~ "WET",
  str_detect(toupper(data$Surface.Condition), "ICE|FROST") ~ "ICE",
  str_detect(toupper(data$Surface.Condition), "SNOW|SLUSH") ~ "SNOW",
  TRUE ~ "OTHER"
)

# use SurfaceStandard to assign ordinal SurfaceIndex
data$SurfaceIndex = ifelse(data$SurfaceStandard == "DRY", 1,
                           ifelse(data$SurfaceStandard == "WET", 2,
                                  ifelse(data$SurfaceStandard == "ICE", 3,
                                         ifelse(data$SurfaceStandard == "SNOW", 4, 5))))

# make a mini data frame with injury rate per collision type
collision_scores <- data %>%
  group_by(Collision.Type) %>%
  summarise(InjuryRate = mean(InjuryBinary), .groups = "drop") %>%
  arrange(InjuryRate) %>%
  mutate(CollisionScore = round(seq(1.0, by = 0.1, length.out = n()), 1))

# add the injury rate scores back to the main dataset
data <- data %>%
  left_join(collision_scores, by = c("Collision.Type" = "Collision.Type"))

# convert column names to CamelCase
colnames(data) <- c("SpeedLimit", "Weather", "SurfaceCondition",
                    "CollisionType", "InjurySeverity", "WeatherIndex", 
                    "InjuryBinary", "InjuryRate", "SurfaceStandard", "SurfaceIndex", "CollisionScore")

write.csv(data, "Cleaned_Crash_Data.csv", row.names = FALSE)

# Show the first 50 rows of cleaned data
head(data, 50)
