library(dplyr)
source("age_in_months.R")

load_data_source <- function(path="train.csv",
                             method=read.csv) {
  as_color_keywords <- function(strings) {
    strsplit(as.character(strings),
             split = "[^[:alnum:]]+")
  }
  
  
  method(path) -> raw
  
  transmute(raw,
            id = AnimalID,
            # Factor variable: cat or dog.
            species = AnimalType,
                
            # Factor variables, target: Adopted, passed away, euthanized,
            # returned to owner, or transferred.
            outcome_type = OutcomeType,
            
            # Factor variable, many levels: Creature's specific breed
            breed = Breed,
                
            # Factor variable, many levels: Coat color
            color = as_color_keywords(Color),
            
            # Factor: Levels depend on outcome_type.
            outcome_subtype = OutcomeSubtype,
            
            # Factor: Five levels for intact/fixed male/female, plus "Unknown".
            sex_at_outcome = SexuponOutcome,
            
            # Numeric: Age in months, estimating by 1/30th-month days and 1/4th-month weeks
            age_at_outcome = age_in_months(AgeuponOutcome))
}

# Returns a chatacter vector of the unique color adjectives used in the dataset
color_keywords <- function(data_source = load_data_source()) {
  # First pull all atomic values from the color column, then identify unique elements.
  unique(unlist(shelter$color))
}

# Accepts a column of variable-length character vectors
# Returns a df of binary values representing each unique keyword
keywords_to_numeric_binaries <- function(keyword_column) {
  # Flatten the list and extract unique keywords
  all_keywords <- unique(unlist(keyword_column))
  
  # Converts a vector of keywords into a partial row of numeric booleans
  value_to_binary_numeric_row <- function(keywords_in_value) {
    is.known_keyword_in_value <- function(known_keyword) { 
      as.numeric(known_keyword %in% keywords_in_value)
    }
    
    lapply(all_keywords, is.known_keyword_in_value)
  }
  
  
  converted_rows <- t(sapply(keyword_column, value_to_binary_numeric_row))
  keyword_df <- data.frame(converted_rows)
  names(keyword_df) <- all_keywords
  
  keyword_df
}