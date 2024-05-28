#Rowan Mascarenhas - ID: 1797865 - 3/6/24

library(dplyr)

best <- function(country, pricerange) {
  # Read zomato and country code data
  zomato_data <- read.csv("zomato.csv")
  country_code_data <- read.csv("Country-code.csv")
  
  # Sort zomato data to show country and price range
  sorted_data <- zomato_data
    filter(`Country.Code` == country_code_data$`Country Code`[country_code_data$`3lettercode` == country],`Price range` == pricerange)
    group_by(`Country.Code`, `Price.range`)
    arrange(desc(`Aggregate.rating`), `Restaurant.Name`)
    slice_head(n = 3)
    select(`Restaurant.Name`, City)
  
  return(sorted_data)
}

# Example usage:
best("IND", 2)  # Replace "IND" and 2 with desired country code and price range
