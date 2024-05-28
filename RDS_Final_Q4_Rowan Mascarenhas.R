# Rowan Mascarenhas - ID: 1797865 - 3/6/24

library(dplyr)

rankall <- function(pricerange, num) {
  # Read zomato data
  zomato_data <- read.csv("zomato.csv")
  
  # Check validity of price range
  if (!(pricerange %in% 1:4)) {
    stop("invalid price range")
  }
  
  # Filter zomato data for the specified price range
  sorted_data <- zomato_data %>%
    filter(`Price.range` == pricerange) 
  
  # Determine the number of restaurants for the specified price range
  num_restaurants <- nrow(sorted_data)
  
  # If num is 'best' or 'worst':
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- num_restaurants
  }
  
  # Initialize an empty tibble to store results
  result <- tibble(restaurant = character(), country = character())
  
  # Loop through each unique country
  country_codes <- unique(sorted_data$`Country.Code`)
  for (country_code in country_codes) {
    # Filter data for the current country
    country_data <- sorted_data %>%
      filter(`Country.Code` == country_code)
    
    # Check if num is within valid range
    if (num < 1 || num > nrow(country_data)) {
      result <- bind_rows(result, tibble(restaurant = NA, country = country_code))
    } else {
      # Sort the filtered data by ranking
      sorted_data <- country_data
        arrange(desc(`Aggregate.Rating`), `Restaurant.Name`)
      
      # Select the restaurant at the specified ranking
      final_restaurant <- sorted_data
        slice(num)
        select(`Restaurant.Name`)
      
      result <- bind_rows(result, tibble(restaurant = final_restaurant$`Restaurant.Name`, country = country_code))
    }
  }
  
  return(result)
}

# Example usage:
rankall(1, "best")  # Replace 1 and "best" with desired price range and ranking


