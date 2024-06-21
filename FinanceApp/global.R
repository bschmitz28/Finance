# global.R

# Author: Rosie Schmitz

# Define global variables (Update Yearly) 2024
lim401k <- 23000
limrothira <- 7000
limhsa_one <- 4150
limhsa_two <- 8300 

# Catch up Contributions 
calculate_limits <- function(age) {
  lim401k50 <- ifelse(age >= 50, lim401k + 7500, lim401k)
  limrothira50 <- ifelse(age >= 50, limrothira + 1000, limrothira)
  limhsaone55 <- ifelse(age >= 55, limhsa_one + 1000, limhsa_one)
  limhsatwo55 <- ifelse(age >= 55, limhsa_two + 1000, limhsa_two)
  
  list(
    lim401k50 = lim401k50,
    limrothira50 = limrothira50,
    limhsaone55 = limhsaone55,
    limhsatwo55 = limhsatwo55
  )
}
