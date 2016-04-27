library(dplyr)

# Converts an age in string form (ex: "3 years") into a numeric count in months
age_in_months <- function(age) {
  # Counts days as 1/30th and weeks as 1/4th of a month.
  unit_conversions <- list("month" = 1.0, "months" = 1.0,
                           "week" = 0.25, "weeks" = 0.25,
                           "year" = 12.0, "years" = 12.0,
                           "day" = 0.333, "days" = 0.333)
  
  # Apply as.character to allow for both string and stringAsFactor input
  split_age_string <- strsplit(x = as.character(age),
                               split = " ")
  
  # Extract numeric age from the first value of each split age string
  numeric_age <- as.numeric(sapply(split_age_string),
                            function (age_str) { age_str[[1]] })
  
  # Extract its unit of measure for each from the second, then pull the
  # appropriate conversion coefficients
  unit_string <- sapply(split_age_string,
                        function (age_str) { age_str[[2]] })
  conversion_multiple <- unit_conversions[[unit_string]]
  
  
  numeric_age * conversion_multiple
}

outcome_by_breed <- function(data_source = read.csv("train.csv")) {
  data_source %>% group_by(Breed)
}