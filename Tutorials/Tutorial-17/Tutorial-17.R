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
aq[, HotDay:=NULL]
# Multiple columns at once

aq %>%
  select(-Month, -Day) ->aq
aq

#Recording factor level

smoke <- c("Never", "Never","Heavy","Never" ,"Occassionally", 
           "Never", "Never", "Regularly", "Regularly", "No")
smoke2 <- factor(smoke, levels = c("Never", "Occasionally", "Regularly", "Heavy"), ordered = T)


# change name 
new_names = c("Nvr", "Occ", "Reg", "Hvy")

smoke3 <-data.table(smoke2)
smoke3 %>%
  mutate(smoke2 = recode(smoke2, 
                         "Never" = "Nvr", 
                         "Occassionally" = "Occ", 
                         "Regularly" = "Reg",
                         "Heavy" = "Hvy"))
smoke3


# Grouped Summaries 

aq <-  data.table(airquality)

# Calcuate mean

aq[, mean(Ozone), Month]

aq %>%
  group_by(Month)%>%
  summarise(meanTemp = mean(Ozone))

# Sub-setting: Select Columns 
