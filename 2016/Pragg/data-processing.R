## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================

## =========================== Livian Distribution ====================================
## ==============================================================================

livian_dist <- read.csv(file = "livian-distribution.csv")
colnames(livian_dist) <- c("Book","Rome.and.the.West","East.of.the.Adriatic")
livian_dist$Book <- as.factor(livian_dist$Book)

## Convert wide to long with tidr
livian_dist_long <- gather(livian_dist, key = "Book", value = "Value")

## =========================== Legions Data ====================================
## ==============================================================================

legions_data <- read.csv(file = "legions.csv")
## Make long use tidyr's gather
legions_data_long <- gather(legions_data, key = "Country", value = "Value")
colnames(legions_data_long) <- c("Country","Year","Value")
## Remove preceeding "X" from all values in Year:
legions_data_long$Year <- as.factor(gsub(pattern = "X", replacement = "", legions_data_long$Year))

## =========================== Triumphs ====================================
## ==============================================================================

triumphs_data <- read.csv(file = "tirumph-frequency.csv")