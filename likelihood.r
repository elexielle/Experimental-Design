# Define the data set and sigma
data <- c(38.7, 40.4, 37.2, 36.6, 35.9, 34.7, 37.6, 35.1, 37.5, 35.6)
sigma <- 3

# Define a function to calculate the likelihood for a given mu
likelihood <- function(mu) {
  # Calculate the sum of the squared differences between each data point and mu
  sum_squared_differences <- sum((data - mu)^2)
  
  # Calculate the likelihood
  likelihood <- (1/(sqrt(2*pi)*sigma))^length(data) * exp(-1/2 * (1/sigma^2) * sum_squared_differences)
  
  return(likelihood)
}

# Test the function for a specific mu
mu <- 36
likelihood(mu)
