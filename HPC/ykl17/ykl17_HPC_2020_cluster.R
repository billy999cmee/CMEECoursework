# CMEE 2020 HPC excercises R code HPC run code proforma

rm(list=ls()) # good practice 
graphics.off()
source("/rds/general/user/ykl17/home/ykl17_HPC_2020_main.R")

##################################### Main.R ###################################

############################## Main.R #####################################

# Question 18
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

# Testing codes locally
#iter <- list(seq(1, 100, by = 1))
#iter <- 32

# Makes seed different for each simulation
set.seed(iter)

# 4 different community sizes in the 100 simulations, 25 for each
if (iter > 0 & iter <= 25){
  size <- 500
}
if (iter > 25 & iter <= 50){
  size <- 1000
}
if (iter > 50 & iter <= 75){
  size <- 2500
}
if (iter > 75 & iter <= 100){
  size <- 5000
}

# Create file name to store results
output_file_name <- paste("ykl17_cluster_output", iter, ".rda", sep = "")

cluster_run(speciation_rate = 0.0068445, size = size, wall_time = (11.5 * 60), interval_rich = 1, interval_oct = size/10, burn_in_generations = 8 * size, output_file_name = output_file_name)
