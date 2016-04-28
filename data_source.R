library(dplyr)
source("age_in_months.R")

load_data_source <- function(path="train.csv",
                             method=read.csv) {
  raw <- method(path)
  
  as_keywords <- function(strings) {
    strsplit(x, split = "\s+")
  }
  
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
            color = as_keywords(Color),
            
            # Factor: Levels depend on outcome_type.
            # outcome_subtype = OutcomeSubtype,
            
            # Factor: Five levels for intact/fixed male/female, plus "Unknown".
            sex_at_outcome = SexuponOutcome,
            
            # Numeric: Age in months, estimating by 1/30th-month days and 1/4th-month weeks
            age_at_outcome = age_in_months(AgeuponOutcome))
}
