#------------------------------------------------
#' Function that returns values from optional seasonal deterministic model
#'
#' \code{data_gen} Generate simulated data based on a random walk of EIR values
#'
#' @param volatility Volatility of the random walk
#' @param init_EIR Initial EIR value to kick of the random walk
#' @param min_EIR Minimum value over which the random walk will not exceed.
#' @param max_EIR Maximum value over which the random walk will not exceed.
#' @param EIR_step This controls the frequency with which EIR changes in the model.
#'                  This is the length of time (in days) for each EIR period.
#' @param sample_size Average sample size for number of tested individuals in the simulated data set.
#' @param sample_sd Standard deviation of the normal distribution of
#'                   sample size for the number of tested individuals in the simulated data set.
#' @param out_step The number of days between each simulated data point.
#' @param time The length of time in days the model will run.
#' @param prop_treated Proportion of the cases that are treated
#' @param init_age Age groups within the model
#' @param plot_instance If TRUE, plots the simulated prevalence.
#'
#' @export

source("R/genrandwalk.R")
source("R/equilibrium-init-create-stripped.R")
source("R/model_parameters.R")
data_gen <- function(volatility,
                     init_EIR=100,
                     min_EIR = 0.01,
                     max_EIR=500,
                     EIR_step = 30,
                     sample_size=220,
                     sample_sd=40,
                     out_step=30,
                     time= 5*365,
                     prop_treated = 0.4,
                     init_age = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 3.5, 5, 7.5, 10, 15, 20, 30, 40, 50, 60, 70, 80),
                     plot_instance = TRUE){
  #model_file <- system.file("odin", "odin_model_stripped_matched.R", package = "mamasante")

  model_file <- "inst/odin/odin_model_stripped_matched.R"
  cat('EIR_vol = ',volatility,' init_EIR = ',init_EIR,'\n')

  rA_preg <- 0.00512821
  rU_preg <- 0.00906627
  het_brackets <- 5
  comparison <- 'u5'

  ################## generate the data ######################
  # generate random walk of EIR (recursive fn)
  EIR_times<-seq(0,time,by=EIR_step)

  ### just a random walk on logscale
  EIR_vals=genrandwalk(length(EIR_times)-1,volatility,init_EIR,min_EIR,max_EIR)

  ##set up the simulation for the simualted data
  mpl <- model_param_list_create(init_EIR = init_EIR,
                                 init_ft = prop_treated,
                                 EIR_times=EIR_times,
                                 EIR_vals=EIR_vals,
                                 comparison='u5',
                                 lag_rates=10
  )
  pars <- equilibrium_init_create_stripped(age_vector = init_age,
                                           init_EIR = init_EIR,
                                           ft = prop_treated,
                                           model_param_list = mpl,
                                           het_brackets = het_brackets)

  ##The malaria model but only on human side (i.e. no mosquitoes to worry about)
  generator <- odin::odin(model_file)
  state_use <- pars[names(pars) %in% coef(generator)$name]

  # create model with initial values
  mod <- generator(user = state_use, use_dde = TRUE)
  tt <- seq(0, time, out_step)

  # run the simulation to base the data
  start.time <- Sys.time()
  mod_run <- mod$run(tt, step_max_n = 1e7,
                     atol = 1e-5,
                     rtol = 1e-5)
  print(Sys.time()-start.time)

  # shape output
  out <- mod$transform_variables(mod_run)

  # plot data and generate data
  if(plot_instance){
    plot(out$t,out$prev,col="white")
    lines(out$t,out$prev,col="blue",lwd=4)
  }
  tested<-round(rnorm(length(out$prev_all),sample_size,sample_sd))
  positive<-rbinom(length(out$prev_all),tested,out$prev_all)
  month <- seq.Date(from = as.Date('2015-01-01'),by = 'month',length.out = length(tt))

  data_raw<-data.frame(t=out$t+30,
                       tested=tested,
                       positive=positive,
                       prev_true=out$prev_all,
                       EIR_true=EIR_vals,
                       vol_true=volatility,
                       #inc_true=out$incunder5,
                       prev_2to10=out$prev_out_2to10,
                       inc_2to10= out$inc2_10,
                       incall=out$inc)
  return(data_raw)
}

