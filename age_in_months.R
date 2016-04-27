# Converts an age in string form (ex: "3 years") into a numeric count in months
age_in_months <- function(age) {
  # Counts days as 1/30th and weeks as 1/4th of a month.
  unit_conversions <- list("month" = 1.0, "months" = 1.0,
                           "week" = 0.25, "weeks" = 0.25,
                           "year" = 12.0, "years" = 12.0,
                           "day" = 0.333, "days" = 0.333)
  
  # Split each string (or as.character-ized factor) into a length-2 list of
  # (numeric age, unit of measure)
  split_age_string <- strsplit(x = as.character(age),
                               split = " ")
  
  # Extract numeric age from the first value of each split age string
  numeric_age <- sapply(X = split_age_string,
                        FUN = function(age_str_list) {
                          as.numeric(as.character(age_str_list)[1])
                        })
  
  # Extract its unit of measure for each from the second, then pull the
  # appropriate conversion coefficients
  conversion_multiple <- sapply(X = split_age_string,
                                FUN = function(age_str_list) {
                                  unit_of_measure <- as.character(age_str_list)[2]
                                  as.numeric(unit_conversions[[unit_of_measure]])
                                })
  
  numeric_age * as.numeric(conversion_multiple)
}