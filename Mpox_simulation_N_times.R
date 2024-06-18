
simulate_mpox_impact <- function(m = 50, sigma = 20, threshold_mild = 10, threshold_moderate = 100, M = 1000,
                                 shape_mild = 2, scale_mild = 5, shape_moderate = 3, scale_moderate = 7,
                                 shape_severe = 4, scale_severe = 9, a_mild = 2, b_mild = 5, 
                                 a_moderate = 3, b_moderate = 4, a_severe = 5, b_severe = 2,prob_death = 0.05) {
  #Shape and scale parameter calculation
  k <- (m/sigma)^2
  theta <- sigma^2/m
  
  #Probabilities for each severity state
  p_mild <- pgamma(threshold_mild, shape = k, scale = theta)
  p_moderate <- pgamma(threshold_moderate, shape = k, scale = theta) - p_mild
  p_severe <- 1 - pgamma(threshold_moderate, shape = k, scale = theta)
  
  #Creating probability vector
  probabilities <- c(p_mild, p_moderate, p_severe)
  
  #Drawing sample from multinomial distribution
  sample <- rmultinom(1, size = M, prob = probabilities)
  
  #Extract counts for each severity state
  M_mild <- sample[1]
  M_moderate <- sample[2]
  M_severe <- sample[3]
  
  #Simulate the duration of disease for each individual in each state
  duration_mild <- rgamma(M_mild, shape = shape_mild, scale = scale_mild)
  duration_moderate <- rgamma(M_moderate, shape = shape_moderate, scale = scale_moderate)
  duration_severe <- rgamma(M_severe, shape = shape_severe, scale = scale_severe)
  
  #Simulate QoL impact caused by symptoms for each disease state
  qol_reduction_mild <- rbeta(M_mild, shape1 = a_mild, shape2 = b_mild)
  qol_reduction_moderate <- rbeta(M_moderate, shape1 = a_moderate, shape2 = b_moderate)
  qol_reduction_severe <- rbeta(M_severe, shape1 = a_severe, shape2 = b_severe)
  
  #Simulate deaths
  death_mild <- rbinom(M_mild, 1, prob_death)
  death_moderate <- rbinom(M_moderate, 1, prob_death)
  death_severe <- rbinom(M_severe, 1, prob_death)
  
  
  #Adjust QoL reduction for deaths (assuming 0 QoL after death)
  total_reduction_mild <- duration_mild * qol_reduction_mild * (1 - death_mild)
  total_reduction_moderate <- duration_moderate * qol_reduction_moderate * (1 - death_moderate)
  total_reduction_severe <- duration_severe * qol_reduction_severe * (1 - death_severe)
  
  #Sum all the QoL reductions to get the overall reduction in QoL
  overall_qol_reduction <- sum(total_reduction_mild) + sum(total_reduction_moderate) + sum(total_reduction_severe)
  
  #Calculate the number of deaths
  total_deaths <- sum(death_mild) + sum(death_moderate) + sum(death_severe)
  
  return(list(overall_qol_reduction = overall_qol_reduction, total_deaths = total_deaths))
}

#Define a wrapper function to simulate N times
simulate_n_times <- function(N, ...) {
  results <- replicate(N, simulate_mpox_impact(...), simplify = FALSE)
  overall_qol_reductions <- sapply(results, function(x) x$overall_qol_reduction)
  total_deaths <- sapply(results, function(x) x$total_deaths)
  return(list(overall_qol_reductions = overall_qol_reductions, total_deaths = total_deaths))
}
