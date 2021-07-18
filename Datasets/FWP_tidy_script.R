#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Script to tidy FWP Harvest csv's
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(janitor)
elk <- read.csv("https://raw.githubusercontent.com/peterdonati/MT_ungulate_harvest_app/main/Datasets/fromFWP/elk_04-20.csv", quote = "")
deer <- read.csv("https://raw.githubusercontent.com/peterdonati/MT_ungulate_harvest_app/main/Datasets/fromFWP/deer_04-20.csv", quote = "")

# Elk ==========================================================================
t_elk <- clean_names(elk)
t_elk <- filter(t_elk, residency == "SUM")
t_elk <- t_elk[!grepl("[[:alpha:]]", t_elk$hunting_district), ]

# This is dangerous when updating with new data!!!!!:
t_elk <- t_elk[ , c(1, 2, 4, 7, 8, 9, 11, 12, 15)]
# names(t_elk)

# Fixing FWP putting zeroes in years when they don't do effort estimates:
fix_na_elk <- t_elk %>% 
  group_by(license_year) %>% 
  summarise(
    hunters = sum(hunters)
  )
na_these_elk <- fix_na_elk$license_year[which(fix_na_elk$hunters == 0)]
t_elk$hunters[which(t_elk$license_year %in% na_these_elk)] <- NA_integer_

t_elk$success_rate_per_hunter <- t_elk$total_harvest / t_elk$hunters

write.csv(t_elk, "tidy_elk.csv", row.names = FALSE)

# Deer =========================================================================
t_deer <- clean_names(deer)
t_deer <- filter(t_deer, residency == "SUM")
t_deer <- t_deer[!grepl("[[:alpha:]]", t_deer$hunting_district), ]

t_deer <- t_deer[ , c(1, 2, 3, 5, 8, 9, 10, 12, 13, 15)]

fix_na_deer <- t_deer %>% 
  group_by(license_year) %>% 
  summarise(
    hunters = sum(hunters)
  )
na_these_deer <- fix_na_deer$license_year[which(fix_na_deer$hunters == 0)]
t_deer$hunters[which(t_deer$license_year %in% na_these_deer)] <- NA_integer_

t_deer$success_rate_per_hunter <- ifelse(
  t_deer$deer_species %in% c("md", "wt"),
  NA,
  t_deer$total_harvest / t_deer$hunters
)

write.csv(t_deer, "tidy_deer.csv", row.names = FALSE)

