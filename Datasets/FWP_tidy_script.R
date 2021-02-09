#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Script to tidy FWP Harvest csv's
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
elk <- read.csv("https://raw.githubusercontent.com/peterdonati/MT_ungulate_harvest_app/main/Datasets/fromFWP/elk_2004_harvests.csv", quote = "")
deer <- read.csv("https://raw.githubusercontent.com/peterdonati/MT_ungulate_harvest_app/main/Datasets/fromFWP/deer_2004_harvests.csv", quote = "")

# Elk ==========================================================================
t_elk <- filter(elk, Residency == "SUM")

# Remove non-district entries (i.e. all 200 districts are 2xx):
t_elk <- t_elk[!grepl("[[:alpha:]]", t_elk$Hunting.District), ]
names(t_elk)[15] <- "six_plus"
t_elk <- pivot_longer(t_elk, c(Bulls, Cows, Bow, Rifle, six_plus),
                      names_to = "group",
                      values_to = "group_harvest")
t_elk <- mutate(t_elk, ung = "elk")

t_elk <- select(t_elk, c(License.Year, Hunting.District, Hunters, Days, 
                Total.Harvest, group, group_harvest, ung))
names(t_elk) <- c("Year", "District", "Hunters", "Days", "T_harvest", "group", 
                  "group_harvest", "ung")

# Fixing FWP putting zeroes in years when they don't do effort estimates:
fix_na_elk <- t_elk %>% 
  group_by(Year) %>% 
  summarise(
    hunters = mean(Hunters),
    days = mean(Days)
  )

na_these_elk <- fix_na_elk$Year[which(fix_na_elk$hunters == 0 
                                      & fix_na_elk$days == 0)]

t_elk$Hunters[which(t_elk$Year %in% na_these_elk)] <- NA_integer_

# Deer =========================================================================
t_deer <- filter(deer, Residency == "SUM")

t_deer <- t_deer[!grepl("[[:alpha:]]", t_deer$Hunting.District), ]
names(t_deer)[15] <- "four_plus"
t_deer <- pivot_longer(t_deer, c(Bucks, Does, Bow, Rifle, four_plus),
                      names_to = "group",
                      values_to = "group_harvest")
t_deer <- mutate(t_deer, ung = "deer")

t_deer <- select(t_deer, c(License.Year, Hunting.District, Deer.Species,
                           Hunters, Days, Total.Harvest, group, group_harvest, 
                           ung))
names(t_deer) <- c("Year", "District", "Species", "Hunters", "Days", 
                   "T_harvest", "group", "group_harvest", "ung")

fix_na_deer <- t_deer %>% 
  group_by(Year) %>% 
  summarise(
    hunters = mean(Hunters),
    days = mean(Days)
  )

na_these_deer <- fix_na_deer$Year[which(fix_na_deer$hunters == 0 
                                      & fix_na_deer$days == 0)]

t_deer$Hunters[which(t_deer$Year %in% na_these_deer)] <- NA_integer_

# Joining together =============================================================
t_all <- bind_rows(t_deer, t_elk)
t_all <- t_all %>% 
  mutate(p_success = (T_harvest / Hunters))

write.csv(t_all, "harvest_dat.csv", row.names = FALSE)

