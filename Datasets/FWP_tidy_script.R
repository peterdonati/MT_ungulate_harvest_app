#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Script to tidy FWP Harvest csv's
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
elk <- read.csv("https://raw.githubusercontent.com/peterdonati/MT_ungulate_harvest_app/main/Datasets/fromFWP/elk_04-20.csv", quote = "")
deer <- read.csv("https://raw.githubusercontent.com/peterdonati/MT_ungulate_harvest_app/main/Datasets/fromFWP/deer_04-20.csv", quote = "")

# Elk ==========================================================================
t_elk <- filter(elk, Residency == "SUM")

# Remove non-district entries (i.e. all 200 districts are 2xx):
t_elk <- t_elk[!grepl("[[:alpha:]]", t_elk$Hunting.District), ]

names(t_elk)[15] <- "six_plus"
t_elk$ung <- "elk"

t_elk <- t_elk[ , c(1, 2, 4, 7, 8, 9, 11, 12, 15, 16)]
names(t_elk)[c(1, 2, 4)] <- c("Year", "District", "T_harvest")

# Fixing FWP putting zeroes in years when they don't do effort estimates:
fix_na_elk <- t_elk %>% 
  group_by(Year) %>% 
  summarise(
    hunters = mean(Hunters)
  )

na_these_elk <- fix_na_elk$Year[which(fix_na_elk$hunters == 0)]

t_elk$Hunters[which(t_elk$Year %in% na_these_elk)] <- NA_integer_

# Deer =========================================================================
t_deer <- filter(deer, Residency == "SUM")

t_deer <- t_deer[!grepl("[[:alpha:]]", t_deer$Hunting.District), ]
names(t_deer)[15] <- "four_plus"

t_deer$ung <- "deer"

t_deer <- t_deer[ , c(1, 2, 3, 5, 8, 9, 10, 12, 13, 15, 16)]

names(t_deer)[c(1, 2, 3, 5)] <- c("Year", "District", "Species", "T_harvest")

fix_na_deer <- t_deer %>% 
  group_by(Year) %>% 
  summarise(
    hunters = mean(Hunters)
  )

na_these_deer <- fix_na_deer$Year[which(fix_na_deer$hunters == 0)]
t_deer$Hunters[which(t_deer$Year %in% na_these_deer)] <- NA_integer_

# Joining together =============================================================
t_all <- bind_rows(t_deer, t_elk)

t_all$p_success <- ifelse(
  t_all$Species %in% c("md", "wt"),
  NA,
  t_all$T_harvest / t_all$Hunters
)

write.csv(t_all, "harvest_dat.csv", row.names = FALSE)

