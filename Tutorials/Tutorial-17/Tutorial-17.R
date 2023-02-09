# Data Manipulation with data.table, dplyr, and tidyr
# data.table, dplyr, tidyr

install.packages(c("dplyr", "tidyr", "data.table"))
# Syntax basic
# [i = row, j = column, by = groups]
library(data.table)
library(dplyr)

aq <- as.data.table(airquality)

# change wind speed to m/s instead of mph

aq[, Wind:=Wind*0.44704]

aq%>% mutate(Wind = Wind*0.44704) -> aq

# compute new variable based on exiting variable. 

aq %>%
  mutate(Hot = Temp>90)-> aq
aq

# Renaming a variable 
aq %>%
  rename(HotDay  = Hot) ->aq
aq

# Removing a variable. 