
library(dplyr)

rankrestaurant <- function(country, pricerange, num) {
  # Read zomato data
  zomato_data <- read.csv("zomato.csv")
  country_code_data <- read.csv("Country-code.csv")

  sorted_data <- zomato_data %>%
    filter(`Country Code` == country_code_data$`Country Code`[country_code_data$`3lettercode` == country],
           `Price range` == pricerange) 
  
  # Determine the number of restaurants for the specified country and price range
  num_restaurants <- nrow(sorted_data)
  
  # Handle the "best" and "worst" cases
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- num_restaurants
  }
  
  # Check if num is within valid range
  if (num < 1 || num > num_restaurants) {
    return(NA)
  }
  
  # Sort the filtered data by ranking
  sorted_data <- sorted_data %>%
    arrange(desc(`Aggregate.Rating`), `Restaurant.Name`) 
  
  # Select the restaurant at the specified ranking
  ranked_restaurant <- sorted_data %>%
    slice(num) %>%
    select(`Restaurant.Name`, City)
  
  return(ranked_restaurant)
}

# Example usage:
rankrestaurant("IND", 1, 3)
