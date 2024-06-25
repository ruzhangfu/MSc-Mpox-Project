#mpox_simulation.R

#packages
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

#Define the simulation function with vaccination effects
simulate_mpox_impact <- function(m = 50, sigma = 20, threshold_mild = 10, threshold_moderate = 100, M = 1000,
                                 shape_mild = 2, scale_mild = 5, shape_moderate = 3, scale_moderate = 7,
                                 shape_severe = 4, scale_severe = 9, a_mild = 2, b_mild = 5, 
                                 a_moderate = 3, b_moderate = 4, a_severe = 5, b_severe = 2,
                                 cfr = 0.05, vacc_coverage = 0.7, ve_infection = 0.9,
                                 ve_death = 0.8, ve_symptoms = 0.7) {
  
 #dead individuals
  dead <- M * cfr
  alive <- M - dead
  
  never_infected <- 1 - alive * vacc_coverage
  vaccinated_infected 
  unvaccinated_infected
  
  
   #Determine the number of vaccinated and unvaccinated individuals
  num_vaccinated <- round(M * vacc_coverage)
  num_unvaccinated <- M - num_vaccinated
  
  #Shape and scale parameter calculation
  k <- (m/sigma)^2
  theta <- sigma^2/m
  
  #Probabilities for each severity state
  p_mild <- pgamma(threshold_mild, shape = k, scale = theta)
  p_moderate <- pgamma(threshold_moderate, shape = k, scale = theta) - p_mild
  p_severe <- 1 - pgamma(threshold_moderate, shape = k, scale = theta)
  
  #Creating probability vector
  probabilities <- c(p_mild, p_moderate, p_severe)
  
  #Adjust probabilities for vaccinated individuals based on vaccine efficacy
  probabilities_vaccinated <- probabilities * (1 - ve_infection)
  probabilities_vaccinated <- probabilities_vaccinated / sum(probabilities_vaccinated) # Normalize
  
  #Drawing sample from multinomial distribution
  sample_unvaccinated <- rmultinom(1, size = num_unvaccinated, prob = probabilities)
  sample_vaccinated <- rmultinom(1, size = num_vaccinated, prob = probabilities_vaccinated)
  
  #Extract counts for each severity state
  M_mild <- sample_unvaccinated[1] + sample_vaccinated[1]
  M_moderate <- sample_unvaccinated[2] + sample_vaccinated[2]
  M_severe <- sample_unvaccinated[3] + sample_vaccinated[3]
  
  #Simulate the duration of disease for each individual in each state
  duration_mild <- rgamma(M_mild, shape = shape_mild, scale = scale_mild)
  duration_moderate <- rgamma(M_moderate, shape = shape_moderate, scale = scale_moderate)
  duration_severe <- rgamma(M_severe, shape = shape_severe, scale = scale_severe)
  
  #Simulate QoL impact caused by symptoms for each disease state
  qol_reduction_mild <- rbeta(M_mild, shape1 = a_mild, shape2 = b_mild)
  qol_reduction_moderate <- rbeta(M_moderate, shape1 = a_moderate, shape2 = b_moderate)
  qol_reduction_severe <- rbeta(M_severe, shape1 = a_severe, shape2 = b_severe)
  
  #Adjust QoL reduction for vaccinated individuals based on vaccine efficacy
  qol_reduction_mild[1:num_vaccinated] <- qol_reduction_mild[1:num_vaccinated] * (1 - ve_symptoms)
  qol_reduction_moderate[1:num_vaccinated] <- qol_reduction_moderate[1:num_vaccinated] * (1 - ve_symptoms)
  qol_reduction_severe[1:num_vaccinated] <- qol_reduction_severe[1:num_vaccinated] * (1 - ve_symptoms)
  
  #Simulate deaths
  death_unvaccinated_mild <- rbinom(sample_unvaccinated[1], 1, prob_death)
  death_unvaccinated_moderate <- rbinom(sample_unvaccinated[2], 1, prob_death)
  death_unvaccinated_severe <- rbinom(sample_unvaccinated[3], 1, prob_death)
  
  death_vaccinated_mild <- rbinom(sample_vaccinated[1], 1, prob_death * (1 - ve_death))
  death_vaccinated_moderate <- rbinom(sample_vaccinated[2], 1, prob_death * (1 - ve_death))
  death_vaccinated_severe <- rbinom(sample_vaccinated[3], 1, prob_death * (1 - ve_death))
  
  #Combine death counts
  death_mild <- c(death_unvaccinated_mild, death_vaccinated_mild)
  death_moderate <- c(death_unvaccinated_moderate, death_vaccinated_moderate)
  death_severe <- c(death_unvaccinated_severe, death_vaccinated_severe)
  
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
  return(data.frame(overall_qol_reduction = overall_qol_reductions, total_deaths = total_deaths))
}
