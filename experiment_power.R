library(tidyverse)
library(haven)

get_power <- function(sample_size) {
  ## Y = Performance (WPA)
  ## D = Tenure -- Binary -- 1 if traded, 0 if not
  ## X1 = Salary
  ## X2 = Age - Mean_Age
  ## X3 = Previous Year WAR
  
  X1_coeff = 0.4
  X2_coeff = -0.1
  X3_coeff = 0.6
  
  reps = 500
  effect_size = -1
  
  alpha = 0.05
  
  pvalues = c()
  
  
  for(iter in 1:reps){
    D = rbinom(sample_size, 1, 0.25)
    X1 = rlnorm(sample_size, meanlog = log(4000000), sdlog = 0.5)
    X2 = rnorm(sample_size, 0, 4)
    X3 = rnorm(sample_size, 1.5, 1)
    U = rnorm(sample_size, 10, 15)
    Y = effect_size * D + X1_coeff * X1 + X2_coeff * X2 + X3_coeff * X3 + U
    
    model = lm(
      Y ~ as.factor(D) + X1 + X2 + X3
    )
    pvalue = summary(model)$coefficients["as.factor(D)1","Pr(>|t|)"]
    pvalues = c(pvalues, pvalue)
  }
  
  rejected = pvalues[pvalues<=alpha]
  
  power = length(rejected) / reps
  
  return(power)
}

get_min_sample_size <- function(grid) {
  powers = c()
  for (size in grid) {
    power = get_power(size)
    powers = c(powers, power)
  }
  min_index = min(which(powers >= 0.8))
  return(c(grid[min_index], powers[min_index]))
}


multipliers = c(0.8, 0.85, 0.9, 0.95, 1.0, 1.05, 1.1, 1.15, 1.2)

grid = c(100, 500, 1000, 2000, 3000, 5000, 10000)

power <- 0

while (abs(power - 0.8) > 0.01) {
  size_power = get_min_sample_size(grid)
  power = size_power[2]
  size = size_power[1]
  grid = size * multipliers
}

size

power
