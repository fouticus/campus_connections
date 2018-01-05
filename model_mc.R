########################################################################################
## model_mc.R
## Monte Carlo simulation of network models to evaluate quality of model fit
## Computes various network characteristics and plots their distributions along with the empirical values
########################################################################################

# run config file
source("config.R")

# set random seed
set.seed(983182)

##########################
#### Helper Functions ####
##########################
source("mc_functions.R")  # in separate file for brevity

N <- 500  # Number of iterations
bins <- 50  # number of bins to use when creating histograms

################################################
#### SBM with 2 blocks: mentors and mentees ####
################################################
# Simulate each night/semester separately (we think of all semesters/nights consisting of a superpopulation)
k <- 1
results <- list(length=length(semesters)*length(nights))
empirical <- list(length=length(semesters)*length(nights))
for(sem in semesters){
  for(night in nights){
    t_start <- Sys.time()
    cat(sprintf("%s: Semester: %s, Night: %s", t_start, sem, night))
    # run the simulation
    modeling_results <- SBM_modeling(night, sem, N, bins, par=T, cores=3)
    results[[k]] <- modeling_results[[1]]
    empirical[[k]] <- modeling_results[[2]]
    cat(sprintf(", Duration: %s minutes\n", round(difftime(Sys.time(), t_start, units="min"), 2)))
    k <- k + 1
  }
}
# make a list of all statistics that are averages and standard deviations. we don't want these in the global results
stat_names <- names(results[[1]])
ignore = c(stat_names[grep("stddev", stat_names)], stat_names[grep("ave", stat_names)])
# combine results from each simulation
global_results <- row_summary(combine_envs(results, along=2, ignore=ignore))
global_empirical <- row_summary(combine_envs(empirical, along=2, ignore=ignore))
plot_results(global_results, global_empirical, bins, c("Mentees", "Mentors"), save=T, fprefix=sprintf("%s_%d_%s", "SBM", N, "global"))


##################################################################
#### SBM with 2 blocks: mentors and mentees, and MF Condition ####
##################################################################
k <- 1
results <- list(length=length(semesters)*length(nights))
empirical <- list(length=length(semesters)*length(nights))
for(sem in semesters){
  for(night in nights){
    t_start <- Sys.time()
    cat(sprintf("%s: Semester: %s, Night: %s", t_start, sem, night))
    # run the simulation
    modeling_results <- SBM2_modeling(night, sem, N, bins, par=T, cores=3)
    results[[k]] <- modeling_results[[1]]
    empirical[[k]] <- modeling_results[[2]]
    cat(sprintf(", Duration: %s minutes\n", round(difftime(Sys.time(), t_start, units="min"), 2)))
    k <- k + 1
  }
}
# make a list of all statistics that are averages and standard deviations. we don't want these in the global results
stat_names <- names(results[[1]])
ignore = c(stat_names[grep("stddev", stat_names)], stat_names[grep("ave", stat_names)])
# combine results from each simulation
global_results <- row_summary(combine_envs(results, along=2, ignore=ignore))
global_empirical <- row_summary(combine_envs(empirical, along=2, ignore=ignore))
plot_results(global_results, global_empirical, bins, c("Mentees", "Mentors"), save=T, fprefix=sprintf("%s_%d_%s", "SBM2", N, "global"))


##############################################################################################################
#### Dyad independent model with 2 blocka and MF condition, w/ factors for same gender/mentor-family/dyad ####
##############################################################################################################
N <- 500
k <- 1
results <- list(length=length(semesters)*length(nights))
empirical <- list(length=length(semesters)*length(nights))
for(sem in semesters){
  for(night in nights){
    t_start <- Sys.time()
    cat(sprintf("%s: Semester: %s, Night: %s", t_start, sem, night))
    # run the sim
    modeling_results <- DI_modeling(night, sem, N, bins, par=T, cores=3)
    results[[k]] <- modeling_results[[1]]
    empirical[[k]] <- modeling_results[[2]]
    cat(sprintf(", Duration: %s minutes\n", round(difftime(Sys.time(), t_start, units="min"), 2)))
    k <- k + 1
  }
}
# make a list of all statistics that are averages and standard deviations. we don't want these in the global results
stat_names <- names(results[[1]])
ignore = c(stat_names[grep("stddev", stat_names)], stat_names[grep("ave", stat_names)])
# combine results from each simulation
global_results <- row_summary(combine_envs(results, along=2, ignore=ignore))
global_empirical <- row_summary(combine_envs(empirical, along=2, ignore=ignore))
plot_results(global_results, global_empirical, bins, c("Mentees", "Mentors"), save=T, fprefix=sprintf("%s_%d_%s", "DI", N, "global"))
