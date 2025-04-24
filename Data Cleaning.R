# libraries
library(tidyverse)

# read in data
raw = read.csv(file.choose())

# check data
head(raw)
nrow(raw)
ncol(raw)

# select columns we need
data = raw %>% select(Speed.Limit,Weather,Surface.Condition,
                             Collision.Type,Injury.Severity)

head(data)
ncol(data)

# check for NA rows
data = na.omit(data)
nrow(data) # no NA rows

# make all characters upper case
data = data %>%
  mutate(across(where(is.character), toupper))


# check what values are in Weather
data %>% count(Weather)

# ensure similar values have the same value
data = data %>%
  mutate(Weather = str_replace(Weather, "RAINING", "RAIN"), 
         Weather = str_replace(Weather, "BLOWING SNOW", "SNOW")) %>%
  filter(Weather != "N/A")

# make ordinal index for Weather attribute
data$Weather.Index = ifelse(data$Weather=="CLEAR",1,
                  ifelse(data$Weather=="CLOUDY",2,
                  ifelse(data$Weather=="RAIN",3,
                  ifelse(data$Weather=="SNOW",4,5))))


# check what values are in injury severity
data %>% count(Injury.Severity)

# remove empty rows
data = data %>%
  filter(Injury.Severity != "")
nrow(data)

#convert InjurySeverity to binary indicator
data$InjuryBinary = ifelse(data$Injury.Severity == "NO APPARENT INJURY", 0, 1)


# check what values are in surface condition
data %>% count(Surface.Condition)

data = data %>% filter(!(Surface.Condition %in% c("","N/A")))

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
                                  ifelse(data$SurfaceStandard == "SNOW", 3,
                                         ifelse(data$SurfaceStandard == "ICE", 4, 5))))


# check what values are in collision type
data %>% count(Collision.Type)

# remove insignificant values
data = data %>%
  filter(!(Collision.Type %in% c("N/A", "UNKNOWN")))

# list of dangerous collision types
dangerous <- c(
  "HEAD ON", "FRONT TO FRONT", "HEAD ON LEFT TURN",
  "ANGLE", "STRAIGHT MOVEMENT ANGLE", "ANGLE MEETS LEFT TURN", 
  "ANGLE MEETS RIGHT TURN", "ANGLE MEETS LEFT HEAD ON",
  "REAR TO SIDE", "SAME DIR REND LEFT TURN", "SAME DIR REND RIGHT TURN"
)

data$CollisionSeverity <- ifelse(data$Collision.Type %in% dangerous, 1, 0)

# checking column names
names(data)

# convert column names to CamelCase
data <- data %>%
  rename(
    SpeedLimit = Speed.Limit,
    WeatherIndex = Weather.Index,
    InjuryBinary = InjuryBinary,
    SurfaceStandard = SurfaceStandard,
    SurfaceIndex = SurfaceIndex,
    InjurySeverity = Injury.Severity,
    CollisionType = Collision.Type,
    Weather = Weather,
    SurfaceCondition = Surface.Condition,
    CollisionSeverity = CollisionSeverity
  )

#checking col names 
names(data)

write.csv(data, "Cleaned_Crash_Data.csv", row.names = FALSE)

# Show the first 50 rows of cleaned data
head(data, 50)