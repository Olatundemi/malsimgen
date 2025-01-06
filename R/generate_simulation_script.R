library(hipercow)
windows_authenticate()

# Initialize hipercow

hipercow_init(driver = 'windows')
resources <- hipercow_resources(cores=32)
hipercow_environment_create(sources = "generate_sim_compendium.R")

set.seed(123)
n <- 10  # Number of samples
log_min <- log10(0.1)
log_max <- log10(500)
random_samples <- 10^runif(n, min = log_min, max = log_max)


#random volatility values between 0.1 and 1.0
volatility_samples <- seq(0.1, 1.0, by = 0.1)

# Number of simulations
n_sims <- 10000

#task for the simulations
sims_compendium_train <- task_create_expr({
  sims <- lapply(1:n_sims, function(i) {

    # Sample init_EIR and volatility for each simulation run
    init_EIR <- sample(random_samples, 1)
    vol <- sample(volatility_samples, 1)

    #simulation with the selected init_EIR and volatility
    generate_sim_compendium(n_sims=1,
                            volatility=vol,     # Random volatility
                            init_EIR=init_EIR,  # Random init_EIR
                            duration=20*365,
                            out_step=30,
                            plot_instance=FALSE)

  })

  #all simulation results Combined into a single data frame
  combined_sims <- do.call(rbind, sims)

  #distinct run values assigned to each row
  combined_sims$run <- rep(1:n_sims, each = nrow(combined_sims) / n_sims)

  return(combined_sims)
}, resources = resources)
task_status(sims_compendium_train)

#task_log_show(sims_compendium_train)
file = task_result(sims_compendium_train)
write.csv(file, "./ANC_Simulation_10000_runs.csv")

use_github(private = FALSE)
