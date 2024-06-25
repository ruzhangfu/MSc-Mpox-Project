# Configuration function to set parameters
get_simulation_params <- function() {
  list(
    counts = 1000,
    cfr = 0.01,
    mild_threshold = 20,
    moderate_threshold = 50,
    lambda = 10,
    prob_vaccinated_infected = 0.1,
    prob_unvaccinated_infected = 0.2,
    prob_not_infected = 0.7,
    shape = 2,
    scale = 5,
    vaccine_efficacy = 0.5, # 50% reduction in lesions for vaccinated individuals
    qol_loss_params = list(
      "vaccinated and infected" = c(alpha = 2, beta = 8),
      "unvaccinated and infected" = c(alpha = 2, beta = 5),
      "not infected" = c(alpha = 1, beta = 10),
      "mild" = c(alpha = 2, beta = 8),
      "moderate" = c(alpha = 2, beta = 5),
      "severe" = c(alpha = 3, beta = 3)
    )
  )
}

# Simulate deaths using binomial distribution
simulate_deaths <- function(counts, cfr) {
  rbinom(counts, size=1, prob=cfr)
}

# Simulate disease duration in days using gamma distribution
simulate_duration <- function(counts, shape, scale) {
  rgamma(counts, shape = shape, scale = scale)
}

# Define helper function to simulate lesions and categorize severity
categorize_severity <- function(survivors_count, lambda, mild_threshold, moderate_threshold, infection_status, vaccine_efficacy) {
  lesions <- rpois(survivors_count, lambda = lambda)
  
  # Apply vaccine efficacy to reduce the number of lesions for vaccinated individuals
  vaccinated_indices <- which(infection_status == "vaccinated and infected")
  lesions[vaccinated_indices] <- lesions[vaccinated_indices] * (1 - vaccine_efficacy)
  
  severity <- cut(lesions, breaks = c(-Inf, mild_threshold, moderate_threshold, Inf),
                  labels = c("mild", "moderate", "severe"))
  list(lesions = lesions, severity = severity)
}

# Define helper function to simulate infection status
simulate_infection_status <- function(counts, prob_vaccinated_infected, prob_unvaccinated_infected, prob_not_infected) {
  if (abs(sum(prob_vaccinated_infected, prob_unvaccinated_infected, prob_not_infected) - 1) > .Machine$double.eps^0.5) {
    stop("Probabilities must sum to 1")
  }
  
  status_counts <- rmultinom(1, size = counts, prob = c(prob_vaccinated_infected, prob_unvaccinated_infected, prob_not_infected))
  rep(c("vaccinated and infected", "unvaccinated and infected", "not infected"), status_counts)
}

# Define function to calculate QoL loss using beta distribution
calculate_qol_loss <- function(infection_status, severity, duration, qol_loss_params) {
  qol_loss <- numeric(length(infection_status))
  
  for (status in unique(infection_status)) {
    params <- qol_loss_params[[status]]
    indices <- which(infection_status == status)
    qol_loss[indices] <- rbeta(length(indices), params['alpha'], params['beta'])
  }
  
  for (sev in unique(severity)) {
    params <- qol_loss_params[[sev]]
    indices <- which(severity == sev)
    qol_loss[indices] <- qol_loss[indices] + rbeta(length(indices), params['alpha'], params['beta'])
  }
  
  non_na_indices <- which(!is.na(duration))
  qol_loss[non_na_indices] <- qol_loss[non_na_indices] * duration[non_na_indices]
  
  qol_loss
}

# Define main simulation function
run_simulation <- function(params) {
  deaths_vector <- simulate_deaths(params$counts, params$cfr)
  deaths_count <- sum(deaths_vector)
  survivors_count <- params$counts - deaths_count
  
  infection_status <- simulate_infection_status(params$counts, params$prob_vaccinated_infected, params$prob_unvaccinated_infected, params$prob_not_infected)
  infected_indices <- which(infection_status %in% c("vaccinated and infected", "unvaccinated and infected"))
  infected_count <- length(infected_indices)
  
  infection_duration <- rep(NA, params$counts)
  if (infected_count > 0) {
    infection_duration[infected_indices] <- simulate_duration(infected_count, params$shape, params$scale)
  }
  
  severity_data <- categorize_severity(survivors_count, params$lambda, params$mild_threshold, params$moderate_threshold, infection_status, params$vaccine_efficacy)
  lesions <- severity_data$lesions
  severity <- severity_data$severity
  
  qol_loss <- calculate_qol_loss(infection_status, severity, infection_duration, params$qol_loss_params)
  
  data.frame(
    status = c(rep("dead", deaths_count), rep("alive", survivors_count)),
    lesions = c(rep(NA, deaths_count), lesions),
    severity = c(rep(NA, deaths_count), severity),
    infection_status = infection_status,
    infection_duration = infection_duration,
    qol_loss = qol_loss
  )
}

# Define wrapper function to run the simulation multiple times
run_multiple_simulations <- function(n, params) {
  results_list <- lapply(1:n, function(x) run_simulation(params))
  do.call(rbind, results_list)
}

# Define function to generate summary statistics
generate_summary_statistics <- function(results) {
  summary_stats <- data.frame(
    total_individuals = nrow(results),
    total_deaths = sum(results$status == "dead"),
    total_survivors = sum(results$status == "alive"),
    avg_lesions = mean(results$lesions, na.rm = TRUE),
    avg_qol_loss = mean(results$qol_loss, na.rm = TRUE),
    mild_cases = sum(results$severity == "mild", na.rm = TRUE),
    moderate_cases = sum(results$severity == "moderate", na.rm = TRUE),
    severe_cases = sum(results$severity == "severe", na.rm = TRUE)
  )
  summary_stats
}

# Run the simulation 1000 times
params <- get_simulation_params()
all_results <- run_multiple_simulations(1000, params)

# Generate and print summary statistics
summary_statistics <- generate_summary_statistics(all_results)
print(summary_statistics)

# Inspect the combined results
print(head(all_results))
