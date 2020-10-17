################################################################################
#
# Script to tidy FWP Harvest csv's
#
################################################################################

library(tidyverse)
elk <- read.csv("C:/Users/Peter/Desktop/misc/Hunt_fish/elk_fwpHarvestEstimatesReport.csv")
deer <- read.csv("C:/Users/Peter/Desktop/misc/Hunt_fish/deer_fwpHarvestEstimatesReport.csv")

# Not sure what's happening here but:
ant <- read.csv("C:/Users/Peter/Desktop/misc/Hunt_fish/ant_fwpHarvestEstimatesReport.csv", row.names = NULL)
names(ant) <- lead(names(ant))
ant <- ant[ ,-11]
ant$License.Year <- as.integer(ant$License.Year)

# Elk ==========================================================================
t_elk <- filter(elk, Residency == "SUM")

t_elk <- t_elk[!grepl("[[:alpha:]]", t_elk$Hunting.District), ]

elk1 <- t_elk %>% 
  pivot_longer(c(Bulls, Cows),
               names_to = "Sex",
               values_to = "n_sex")
key_length <- nrow(elk1)
elk1 <- mutate(elk1, key = 1:key_length) %>% 
  select(c(License.Year, Hunting.District, Hunters:Days, 
           Total.Harvest, X6.or.More.Points, Sex:key))

elk2 <- t_elk %>% 
  pivot_longer(c(Bow, Rifle), names_to = "Weapon", values_to = "n_weapon") %>% 
  mutate(key = 1:key_length, ung = "elk") %>% 
  select(c(License.Year, Hunting.District, Total.Harvest, Weapon:ung))

t_elk <- left_join(elk1, elk2) %>% 
  select(-key)

t_elk <- rename(
  t_elk, 
  Year = License.Year, 
  District = Hunting.District,
  N = Total.Harvest,
  six_plus = X6.or.More.Points 
)

# Deer =========================================================================
t_deer <- filter(deer, Residency == "SUM")

t_deer <- t_deer[!grepl("[[:alpha:]]", t_deer$Hunting.District), ]

deer1 <- t_deer %>% 
  pivot_longer(c(Bucks, Does),
               names_to = "Sex",
               values_to = "n_sex")
key_length <- nrow(deer1)
deer1 <- mutate(deer1, key = 1:key_length) %>% 
  select(c(License.Year:Deer.Species, Hunters:Days, 
           Total.Harvest, X4.or.More.Points, Sex:key))

deer2 <- t_deer %>% 
  pivot_longer(c(Bow, Rifle), names_to = "Weapon", values_to = "n_weapon") %>% 
  mutate(key = 1:key_length, ung = "deer") %>% 
  select(c(License.Year, Hunting.District, Total.Harvest, Weapon:ung))

t_deer <- left_join(deer1, deer2) %>% 
  select(-key)

t_deer <- rename(
  t_deer, 
  Year = License.Year, 
  District = Hunting.District,
  Species = Deer.Species,
  N = Total.Harvest,
  four_plus = X4.or.More.Points 
)

# Antelope =====================================================================
t_ant <- filter(ant, Residency == "SUM")

t_ant <- t_ant[!grepl("[[:alpha:]]", t_ant$Hunting.District), ]

t_ant <- t_ant %>% 
  pivot_longer(c(Bucks, Does),
               names_to = "Sex",
               values_to = "n_sex") %>% 
  mutate(ung = "ant") %>% 
  select(c(License.Year, Hunting.District, Hunters:Days, 
           Total.Harvest, Sex:ung))

t_ant <- rename(
  t_ant, 
  Year = License.Year, 
  District = Hunting.District,
  N = Total.Harvest
)
# Joining together =============================================================

t_all <- bind_rows(t_deer, t_elk, t_ant)
write.csv(t_all, "harvest_dat.csv", row.names = FALSE)
