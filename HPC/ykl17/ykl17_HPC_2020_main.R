# CMEE 2020 HPC excercises R code main proforma
# you don't HAVE to use this but it will be very helpful.  If you opt to write everything yourself from scratch please ensure you use EXACTLY the same function and parameter names and beware that you may loose marks if it doesn't work properly because of not using the proforma.

name <- "Billy Lam"
preferred_name <- "Billy"
email <- "yu.lam17@imperial.ac.uk"
username <- "ykl17"
personal_speciation_rate <- 0.0068445

# please remember *not* to clear the workspace here, or anywhere in this file. If you do, it'll wipe out your username information that you entered just above, and when you use this file as a 'toolbox' as intended it'll also wipe away everything you're doing outside of the toolbox.  For example, it would wipe away any automarking code that may be running and that would be annoying!

# Question 1
# Calculate the species richness, aka the number of unique elements in a vector
species_richness <- function(community){
  return(length(unique(community)))
}

# Question 2
# Gives maximum number of spp for the community
init_community_max <- function(size){
  return(seq(size))
}

# Question 3
# Gives the minimum number of spp for the community
init_community_min <- function(size){
  return(rep(1, size))
}

# Question 4
# Chooses two individuals at random
choose_two <- function(max_value){
  return(sample(1:max_value, 2, replace = FALSE))
}

# Question 5
# Single step of a neutral model, randomly chooses one to die and another to reproduce
neutral_step <- function(community){
  
  # Chooses two individuals at random using function from Q4
  randomly_die <- choose_two(length(community)) 
  
  # One individual dies & one individual reproduces
  community[randomly_die[1]] <- community[randomly_die[2]] 
  
  return(community)
}

# Question 6
# Generate several neutral steps, meaning that one generation has passed!
neutral_generation <- function(community){
  # If community size is even/odd
  n <- round(length(community)/2) # round could be either high or low
  
  for( i in 1:n){
    community <- neutral_step(community) 
  }
  return(community)
}

# Question 7
# Neutral theory simulation, returns spp richness at each generation
neutral_time_series <- function(community, duration)  {
  
  spp_rich <- species_richness(community) 
  
  # Runs several generations and return species richness at each gen
  for (i in 1:duration){ 
    community <- neutral_generation(community) 
    spp_rich <- c(spp_rich, species_richness(community)) 
  }
  return(spp_rich)
}

# Question 8
# Plot time series of neutral model simulation with max community of 200 generations
question_8 <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  # Initialize parameters for simulation
  community <- init_community_max(100)
  duration <- 200
  
  # Neutral model simulation
  model_sim <- neutral_time_series(community, duration)
  # Duration of generations
  x <- 1:length(model_sim) 
  
  plot(x = x, y = model_sim, cex=0.5, main="A neutral model with no speciation",
       xlab = "Generations", ylab = "Species richness")
  
  return("Given enough time and without the permission of emmigrations and immigrations, the population will always converge to 1 species as permanent species loss will occur due to random fluctuations.")
}

# Question 9 
# One step of neutral model with speciation
neutral_step_speciation <- function(community,speciation_rate)  {
  # Generate a number from 0 to 1 and compare it with the speciation rate
  r <- runif(1, min = 0, max = 1) 
  
  # If the random number is greater than the speciation rate
  if (r < speciation_rate){ 
    
    spp <- max(community) + 1 # a new spp id
    randead <- sample(1:length(community), 1) # randomly selects an individual to die
    community[randead] <- spp # replace with new spp
    
  } else { # no speciation occurs
    community <- neutral_step(community)
    
  }
  return(community)
}

# Question 10
# Same as Q6, neutral model simulation with a chance for speciation to occur
neutral_generation_speciation <- function(community,speciation_rate)  {
  
  n <- round(length(community)/2)
  
  for (i in 1:n){
    community <- neutral_step_speciation(community, speciation_rate)
  }
  return(community)
}

# Question 11
# Same as Q7, simulation with multiple generations, returning spp richness at each generation
neutral_time_series_speciation <- function(community, speciation_rate, duration)  {
  
  spp_rich <- c(species_richness(community))
  
  for (i in 1:duration){
    community <- neutral_generation_speciation(community, speciation_rate)
    richness <- species_richness(community) 
    spp_rich <- c(spp_rich, richness)
  }
  return(spp_rich)
}

# Question 12
# Neutral theory simulation with speciation
# Plot spp richness against time series
question_12 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  mincomm <- neutral_time_series_speciation(init_community_min(100), 0.1, 200)
  maxcomm <- neutral_time_series_speciation(init_community_max(100), 0.1, 200)
  
  x <- 1:201
  
  plot(x = x, y = mincomm, main="A neutral model with no speciation", 
       xlab = "Generations", ylab = "Species richness", col="red", ylim = c(1, max(maxcomm)), type = "l") # type = l make both lines
  
  lines(x = x, y = maxcomm, col = "green")
  
  legend(x = "topright", legend=c("Maximum species richness", "Minimum species richness"),
         col = c("green", "red"), cex = 0.8, lty = 1)
  
  return("Despite massive differences in species richness for the two starting communities, both communities converges around the same species richness (between 20 and 40). This is due to the neutral model consisting of identical parameters for speciation rates and population sizes, with communities reaching a dynamic equilibrium when speciation equals to species extinction.")
}

# Question 13
# Spp abundance in a vector, 
species_abundance <- function(community)  {
  return(as.vector(sort(table(community), decreasing = T)))
}

# Question 14
# Bin spp abundances into octave classes
octaves <- function(abundance_vector) {
  # Log base 2 of the input
  log_input <- log(abundance_vector, 2) 
  # Convert to bin by rounding and + 1
  transformed_input <- floor(log_input) + 1  
  # Put numbers into corresponding bins
  bins <- tabulate(transformed_input) 
  
  return(bins)
}

# Question 15
# Sums up two inout vectors
sum_vect <- function(x, y) {
  if (length(x) > length(y)){ # If x is longer than y
    # Fills y with 0
    y <- c(y, rep(0, length(x) - length(y))) 
    # Adds the two vectors
    z <- x + y 
    
  } else if(length(x) < length(y)) { # If y is longer than x
    # Fills x with 0
    x <- c(x, rep(0, length(y) - length(x))) 
    # Adds the two vectors
    z <- x + y 
    
  } else { # If x and y are equally long
    z <- x + y
  }
  return(z)
}

# Question 16 
# Neutral model simulation for burnin period (200 gen) and further 2000 gen with spp abundance every 20 gen
question_16 <- function(){
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  # Initialize parameters for the model simulation
  mincomm <- init_community_min(100) 
  maxcomm <- init_community_max(100)
  speciation_rate <- 0.1
  duration <- 2200
  burnin <- 200
  
  # Run neutral model for the 'burn in' period
  for (i in 1:burnin) {
    comm1 <- neutral_generation_speciation(maxcomm, speciation_rate)
    comm2 <- neutral_generation_speciation(mincomm, speciation_rate)
  }
  
  # Record spp abundance octave vectors
  maxsum <- octaves(species_abundance(comm1))
  minsum <- octaves(species_abundance(comm2))
  
  avgmean <- 0
  
  # Run neutral model for another 2000 generations
  for (i in 1:duration) {
    comm1 <- neutral_generation_speciation(comm1, speciation_rate)
    comm2 <- neutral_generation_speciation(comm2, speciation_rate)
    # For every 20th iteration
    if (i %% 20 == 0) {
      avgmean <- avgmean + 1 
      maxsum <- sum_vect(maxsum, octaves(species_abundance(comm1)))
      minsum <- sum_vect(minsum, octaves(species_abundance(comm2)))
    }
  }
  # Taking the average octaves
  mean1 <- maxsum/avgmean 
  mean2 <- minsum/avgmean
  
  # Plot bar charts of average octaves for communities initialised with
  # maximum and minimal diverstiy
  par(mfrow=c(1,2))
  barplot(mean1, duration, col = "blue", xlab = "Species abundance", ylab = "Number of species", names.arg =  c("1", "2-3", "4-7", "8-15", "16-31", "32-63"), main = "Species abundance distribution \nwith maximum community size", cex.names = 0.6, ylim = c(0, 11))
  barplot(mean2, duration, col = "red", xlab = "Species abundance", ylab = "Number of species", names.arg =  c("1", "2-3", "4-7", "8-15", "16-31", "32-63", "64-127"), main = "Species abundance distribution \nwith minimum community size", cex.names = 0.5, ylim = c(0, 11))
  #mtext("Why isnt my title showing", side = 3, line = 5, outer = TRUE)
  
  return("The initial conditions (min or max) do not make a difference to the final species abundance distributions as the values are only measured after 200 generations, when a dynamic equilibrium has been reached.")
}


# Question 17 
cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name)  {
  # Initial parameters
  community <- init_community_min(size)
  start <- proc.time()[[3]] 
  gen <- 0 
  
  # Initializing spp abundance as a list the later generated abundance octave vectors
  spp_abundance <- list(octaves(species_abundance(community))) 
  # Initializing the time_series as a vector for all the later calculated species richness
  spprich <- species_richness(community) 
  
  end <- start 
  # Looping only occurs before: when the difference in end time and start is equal to wall_time in mins
  while((end - start) < (wall_time * 60)){ # *60 as proc.time gives seconds and wall_time is in minutes
    community <- neutral_generation_speciation(community, speciation_rate) # The community evolves after each generation
    gen <- gen + 1 
    
    if (gen < burn_in_generations & (gen %% interval_rich == 0) ){
      spprich <- c(spprich, species_richness(community))
    }
    
    if( gen %% interval_oct == 0){ 
      # Takes the abundance octave everytime the generations are multiples of the interval_oct
      spp_abundance[length(spp_abundance) + 1] <- list(octaves(species_abundance(community))) 
    }
    # Calculates end time after every loop
    end <- proc.time()[[3]] 
  }
  total <- end - start # Total time
  save(spprich, spp_abundance, community, total, speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, file = output_file_name) 
}



# Questions 18 and 19 involve writing code elsewhere to run your simulations on the cluster

# Question 20 
process_cluster_results <- function()  {
  # Initialize vectors to store data
  r500 <- c()
  r1000 <- c()
  r2500 <- c()
  r5000 <- c()
  
  # Initialize count for calculating means
  m500 <- 0
  m1000 <- 0
  m2500 <- 0
  m5000 <- 0
  
  nofile <- list.files(path = ".", pattern = "ykl17_cluster_output*")
  
  # Load each output file
  for (i in nofile){
    load(i)
    afterburn <- burn_in_generations/interval_oct
    # Start after the burnin period
    for (j in afterburn:length(spp_abundance)){
      if (size == 500){
        r500 <- sum_vect(r500, spp_abundance[[j]])
        m500 <- m500 + 1
      }
      if (size == 1000){
        r1000 <- sum_vect(r1000, spp_abundance[[j]])
        m1000 <- m1000 + 1
      }
      if (size == 2500){
        r2500 <- sum_vect(r2500, spp_abundance[[j]])
        m2500 <- m2500 + 1
      }
      if (size == 5000){
        r5000 <- sum_vect(r5000, spp_abundance[[j]])
        m5000 <- m5000 + 1
      }
    }
  }
  
  # Calculate the mean
  avg500 <- r500/m500
  avg1000 <- r1000/m1000
  avg2500 <- r2500/m2500
  avg5000 <- r5000/m5000
  
  # lists of outputs to return
  combined_results <- list(avg500, avg1000, avg2500, avg5000)
  save(combined_results, file = "ykl17_q20_results.rda")
}

plot_cluster_results <- function()  {
  # clear any existing graphs and plot your graph within the R window
  # load combined_results from your rda file
  # plot the graphs
  
  graphics.off()
  
  load("ykl17_q20_results.rda")
  
  # Plotting barplots
  par(mfrow = c(2, 2))
  barplot(combined_results[[1]], ylim = c(0, max(combined_results[[1]]) * 1.2), col = "blue", 
          names.arg = 1:length(combined_results[[1]]), main = "Community size of 500", 
          ylab = "Number of species", xlab = "Species abundance")
  barplot(combined_results[[2]], ylim = c(0, max(combined_results[[2]]) * 1.2), col = "red", 
          names.arg = 1:length(combined_results[[2]]), main = "Community size of 1000", 
          ylab = "Number of species", xlab = "Species abundance")
  barplot(combined_results[[3]], ylim = c(0, max(combined_results[[3]]) * 1.2), col = "green", 
          names.arg = 1:length(combined_results[[3]]), main = "Community size of 2500", 
          ylab = "Number of species", xlab = "Species abundance")
  barplot(combined_results[[4]], ylim = c(0, max(combined_results[[4]]) * 1.2), col = "orange", 
          names.arg = 1:length(combined_results[[4]]), main = "Community size of 5000", 
          ylab = "Number of species", xlab = "Species abundance")
  mtext(expression(bold("Mean species abundance octave results")), side = 3, line = -18, outer = TRUE, cex = 1.2) # A main title in between subplots
  
  return(combined_results)
}

############ Fractals !! ############

# Question 21
# Return fractal dimension of the given object
question_21 <- function()  {
  
  frac <- log(8)/log(3)
  
  return("As the equation for calculatiing the fractal dimension is log(copies) divided by log(scale) and from the shown object, there are 8 copies with the scale of 3. Therefore, the fractal dimension is log(8)/log(3) = 1.893!")
}

# Question 22
# Return fractal dimension of the given object
question_22 <- function()  {
  
  frac <- log(20)/log(3)
  
  return("Using the equation, the shown object is constructed with 20 cubes with a scale 3 (as the length increases by 3). Therefore, the fractal dimension is log(20)/log(3) = 2.727!")
}

# Question 23
# Draws a Sierpiński triangle under 30 seconds
chaos_game <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  ABC <- list(c(0,0), c(3,4), c(4,1))
  
  X <- c(0,0)
  
  plot(x = X[1], y = X[2], xlim = c(-1, 5), ylim = c(-1, 5), cex = 0.5, main = "Sierpiński triangle")
  
  start1 <- proc.time()[3]
  
  while (proc.time()[3] - start1 < 30){
    random <- sample(ABC, size = 1)
    X <- (X + random[[1]])/2
    points(x = X[1], y = X[2], cex = 0.5)
  }
  
  return("This function produces a Sierpiński triangle, the triangle gets more densed with time but the time limit is set to not exceed 30 seconds.")
}

# Question 24
# Turtle draws a line of a given length from a given point
turtle <- function(start_position, direction, length)  {
  
  point_x <- start_position[1] + (cos(direction) * length) #cah
  point_y <- start_position[2] + (sin(direction) * length) #soh
  
  point_xy <- c(point_x, point_y) # Endpoint
  
  l1 <- cbind(start_position, point_xy)
  lines(l1[1,], l1[2,])
  
  return(point_xy) # you should return your endpoint here.
}

# Question 25
# Calls turtle twice to draw a pair of lines that joins together
elbow <- function(start_position, direction, length)  {
  join_line <- turtle(start_position, direction, length)
  turtle(join_line, direction - (pi/4), length * 0.95)
}

# Question 26
# Draws spiral, calls itself after drawing the first line
spiral <- function(start_position, direction, length)  {
  
  point_xy <- turtle(start_position, direction, length) # Endpoint
  
  if (length > 0.01){ # Making it work
    # Calls the function itself, drawing line rotated by pi/4 clockwise and length reduced by 5% 
    spiral(point_xy, direction-pi/4, length*0.95)
  
  }
  return("As this is a recursive function, it will keep drawing a new line with a reduced length, an error will occur when the length can no longer be reduced")
}

# Question 27
# Plot spiral
draw_spiral <- function()  {
  # clear any existing graphs and plot your graph within the R window c(-1.5, 1.5), xlim = c(0, 3)
  graphics.off()
  
  plot(1, xlab = "", ylab = "", ylim = c(-1.5, 1.5), xlim = c(0, 3), type = "n", main = "Spiral") # Blank plot
  spiral <- spiral(c(1,1), 2*pi, 1) 
  return(spiral) 
}

# Question 28
# Drawing a tree
tree <- function(start_position, direction, length)  {
  # Draw line and record end position
  point_xy <- turtle(start_position, direction, length)
  # If length threshold not passed
  if (length > 0.01){
    # Calls itself to draw anticlockwise rotating line 
    tree(point_xy, direction - pi/4, length * 0.65)
    # Calls itself to draw clockwise rotating line 
    tree(point_xy, direction + pi/4, length * 0.65)
  }
}


draw_tree <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  plot(1, xlab = "", ylab = "", ylim = c(-2, 4), xlim = c(0, 4), type = "n", main = "Tree") # Blank plot
  tree(c(1,1), 2 * pi, 1)
  
}

# Question 29
# Draw fern
fern <- function(start_position, direction, length)  {
  point_xy <- turtle(start_position, direction, length) # Endpoint
  
  if (length > 0.01){
    # Calls itself and goes left anticlockwise, 45 degrees
    fern(point_xy, direction - pi/4, length * 0.38)
    # Calls itself and goes straight
    fern(point_xy, direction, length * 0.87)
  }
  
}

draw_fern <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  plot(1, xlab = "", ylab = "", ylim = c(-2, 1.5), xlim = c(0, 9), type = "n", main = "Fern") # Blank plot
  fern(c(1,1), 2 * pi, 1)
}

# Question 30
# Draws a better looking fern
fern2 <- function(start_position, direction, length, dir)  {
  point_xy <- turtle(start_position, direction, length)
  
  if (length > 0.01){
    if (dir == -1){ # When dir = -1, goes up and left, dir alternates between 1 and -1
      # Calls itself and make fern, then alternates dir
      fern2(point_xy, direction, length * 0.87, dir * -1)
      # Calls itself and make the other half, then alternates dir
      fern2(point_xy, direction - (pi/4), length * 0.38, dir * -1)
    }
    else { # If dir = 1, goes up and right
      # Calls itself and make fern, then alternates dir
      fern2(point_xy, direction, length * 0.87, dir * -1)
      # Calls itself and makes the right half,  then alternates dir
      fern2(point_xy, direction + (pi/4), length * 0.38, dir * -1)
    }
  }
}
draw_fern2 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  plot(1, xlab = "", ylab = "", ylim = c(-2, 4), xlim = c(0, 9), type = "n", main = "A better fern") # Blank plot
  fern2(c(1,1), 2 * pi, 1, -1)
}

# Challenge questions - these are optional, substantially harder, and a maximum of 16% is available for doing them.  

# Challenge question A
Challenge_A <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  # Initialize parameters for the model simulation
  mincomm <- init_community_min(100) 
  maxcomm <- init_community_max(100)
  speciation_rate <- 0.1
  burnin <- 200
  confidence_interval <- 0.972
  
  # Initialize vectors for spp richneess mean values
  max_mean_spprich <- c()
  min_mean_spprich <- c()
  max_conf_l <- c()
  min_conf_l <- c()
  max_conf_r <- c()
  min_conf_r <- c()
  
  # Generate a data frame for recording spp richness over generations
  max_spprich <- matrix(ncol = burnin, nrow=0)
  min_spprich <- matrix(ncol = burnin, nrow=0)
  
  # Record spp richness over generations
  repeat {
    max_spprich <- rbind(max_spprich, neutral_time_series_speciation(maxcomm, speciation_rate, burnin))
    min_spprich <- rbind(min_spprich, neutral_time_series_speciation(mincomm, speciation_rate, burnin))
    if (nrow(max_spprich) == 60) {break}
  }
  
  for (i in 1:burnin){
    # Confidence interval calculations for max
    mean_max_a <- mean(max_spprich[,i])
    max_mean_spprich <- c(max_mean_spprich, mean_max_a)
    max_error <- qnorm(confidence_interval) * sd(max_spprich)/sqrt(ncol(max_spprich))
    max_conf_l <- c(max_conf_l, mean_max_a - max_error)
    max_conf_r <- c(max_conf_r, mean_max_a + max_error)
    
    # Confidence interval calculations for min
    mean_min_a <- mean(min_spprich[,i])
    min_mean_spprich <- c(min_mean_spprich, mean_min_a)
    min_error <- qnorm(confidence_interval) * sd(min_spprich)/sqrt(ncol(min_spprich))
    min_conf_l <- c(min_conf_l, mean_min_a - min_error)
    min_conf_r <- c(min_conf_r, mean_min_a + min_error)
  }

  
  # Plots maximum spp richness
  plot(max_mean_spprich, type = 'l', col = "purple", ylim = c(0, 100), main = "Neutral model simulation" , 
       ylab = "Mean speecies richness", xlab = "Generations")
  # confidence intervals
  polygon(c(seq(burnin), rev(seq(burnin))),c(max_conf_r, rev(max_conf_l)), col = "blue", border = FALSE)
  # Plotting minimum spp richness
  lines(min_mean_spprich, col = "red")
  # confidence intervals
  polygon(c(seq(burnin), rev(seq(burnin))),c(min_conf_r, rev(min_conf_l)), col = "orange", border = FALSE)
  legend("topright", legend = c("Initial max species richness with 97.2% ci", "Initial min species richness with 97.2% ci"), 
         col = c("blue", "orange"), pch = c(4, 4))
  return("Dynamic equilibrium reached around 40")
}

# Challenge question B
Challenge_B <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  # Initiale parameters
  size <- 100
  speciation_rate <- 0.1
  burnin <- 200
  
  # Blank plot
  plot(1, xlab = "", ylab = "", ylim = c(0, 100), xlim = c(0, 200), type = "n", main = "Time series of different initial species richness")
  
  
}

# Challenge question C
Challenge_C <- function() {
  # clear any existing graphs and plot your graph within the R window
  
}

# Challenge question D
Challenge_D <- function() {
  # clear any existing graphs and plot your graph within the R window
  
  return("type your written answer here")
}

# Challenge question E
Challenge_E <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  ABC <- list(c(0,0), c(3,4), c(4,1))

  X <- c(2,2) # Different initial starting position

  # Plot starting position and legends
  plot(x = X[1], y = X[2], xlim = c(-1, 5), ylim = c(-1, 5), cex = 0.5, 
       main = "Sierpiński triangle with a different initial position", col = "black")
  legend("topleft", legend = c("X", "First 75 steps", "75+ steps"), 
         col = c("black", "red", "blue"), pch = c(1, 1, 1))
  
  counter <- 0
  
  start1 <- proc.time()[3]

  # For the first 75 steps, plot in red
  repeat {
    random <- sample(ABC, size = 1)
    X <- (X + random[[1]])/2
    points(x = X[1], y = X[2], cex = 0.5, col = "red")
    counter <- counter + 1
    if (counter <= 75){break}
  }
  
  # For the rest of the steps, plot in blue until it reaches 30 seconds
  repeat {
      random <- sample(ABC, size = 1)
      X <- (X + random[[1]])/2
      points(x = X[1], y = X[2], cex = 0.5, col = "blue")
      if (proc.time()[3] - start1 > 30){break}
  }
  
  return("Although the starting position causes the first few points to become weirdly out of place, but as more points are plotted, they are plotted closer and closer to the first three points, making the first few points inside or next to the triangle.")
}

# Challenge question F
Challenge_F <- function() {
  # clear any existing graphs and plot your graph within the R window
  
  return("type your written answer here")
}

# Challenge question G should be written in a separate file that has no dependencies on any functions here.

