# Uncomment and run 1x
pacman::p_load(googlesheets4, googledrive, tidyverse)

# Run this once in R
gs4_auth()
# Follow instructions it outputs

# Then run again from here

# Load data you've scraped
longDF = read_rds("datasets/LONG-MostInCommon-voting-intention.rds")
wideDF = read_rds("datasets/WIDE-MostInCommon-voting-intention.rds")

# Use gs4_create function

ss <- gs4_create(
  "more-in-common-voting-intentions",
  sheets = list(
    long = longDF,
    wide = wideDF
  ))

ss