#### Code adapted from: 
# Open data and code in sports science analysis
# Author: David N Borg
# Contributor: Joshua J Bon
# Data collection and processing: David N Borg, Brenton J Baguley
# https://github.com/SciBorgo/Open-data-in-sports-science/tree/master
#install.packages(c("binom","dplyr"))

library(dplyr)
library(binom)

## Function to read and combine all the data from Scopus
read_and_filter_data <- function() {
  # Location of the data
  data_folder <- "/my/path/to/data/folder/"
  
  # List all CSV files in the directory with full paths
  files <- list.files(path = data_folder, pattern = "\\.csv$", full.names = TRUE)
  
  # Read each file into a list of data frames
  data_list <- lapply(files, read.csv)
  
  # Combine all data frames into a single data frame
  combined_data <- do.call(rbind, data_list)
  
  # Select only articles with data
  articles_with_data <- subset(combined_data, Include == 1)
  
  # Add the id for each article
  articles_with_data$id <- seq(1:nrow(articles_with_data))
  
  # Return the filtered data frame
  return(articles_with_data)
}



## Function to extract a stratified random sample based on journal of 5%
pick_stratified_sample <- function(data, proportion) {
  # Set seed for reproducibility
  set.seed(42)
  
  # Perform stratified random sampling
  sampled_data <- data %>%
    group_by(Source.title) %>%
    sample_frac(size = proportion, replace = FALSE)
  
  # Ungroup the data to avoid grouping side effects in downstream analysis
  sampled_data <- ungroup(sampled_data)
  
  # Return the stratified sample
  return(sampled_data)
}

## Get the data
data <- read_and_filter_data()
## Get the random sample
# selected_articles <- pick_random_sample(data, 0.05)
stratified <- pick_stratified_sample(data, 0.05)
# Stratified sampling allows to select a random sample from all the 
# journal, thus having a more representative sample to infer on the population.

## Write data to preferred location (to do only once at the start)
# write.csv(stratified, 
#           file = "./random_sample_articles.csv",
#           row.names = FALSE)



#===============================================================================



## Calculate proportions and CIs from articles:


#### Calculate proportions for all studies from random sample
final_data <- read.csv("./random_sample_articles.csv")

successes <- sum(final_data$Outliers == 1, na.rm = TRUE)
trials <- sum(!is.na(final_data$Outliers))
result <- binom.confint(x = successes, n = trials, conf.level = 0.99, methods = "exact")
result
#   method  x   n       mean      lower    upper
# 1  exact 21 232 0.09051724 0.04866528 0.149947

# Therefore, it results that the percentage of studies including an
# identification of outliers was 9.05% (population mean) with a 99% confidence interval
# of 4.87 - 14.99%.





