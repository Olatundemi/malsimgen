#------------------------------------------------
#' Function that generates multiple instances of simulated data
#'
#' \code{generate_sim_compendium} Generate multiple simulation iterations
#'
#' @param n_sims Number of simulated datasets to be created
#' @param volatility Volatility of the random walk
#' @param init_EIR Initial EIR value to kick of the random walk
#' @param min_EIR Minimum value over which the random walk will not exceed.
#' @param max_EIR Maximum value over which the random walk will not exceed.
#' @param out_step The number of days between each simulated data point.
#' @param duration The length of time in days the model will run.
#' @param plot_instance If TRUE, plots the simulated prevalence.
#' @export
#'

source("Z:/Olatunde/MalariaMod/malsimgen/R/data_gen.R")

generate_sim_compendium<-function(n_sims,volatility,init_EIR,duration,out_step,plot_instance=FALSE){
  sims_compendium<-data.frame(
    run=numeric(),
    t=numeric(),
    prev_true=numeric(),
    EIR_true=numeric(),
    vol_true=numeric(),
    #inc_true=numeric(),
    tested=numeric(),
    positive=numeric(),
    prev_2to10=numeric(),
    inc_2to10=numeric(),
    incall=numeric()

  )
  for(i in 1:n_sims){
    sims_compendium<-dplyr::add_row(sims_compendium,
      cbind(
        data.frame(run=i),
        data_gen(volatility = volatility, init_EIR = init_EIR,time = duration,out_step = out_step,plot_instance = plot_instance)
      )
    )
  }
  return(sims_compendium)
}
