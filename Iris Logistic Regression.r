source("Hamiltonian MCMC.r")

VERSICOLOR_LABEL = 1
VIRGINICA_LABEL = 0

# HMC Parameters
STEP_SIZE = 0.01
NUMBER_OF_STEPS = 40
CLASSIFICATION_BOUNDARY_MEAN = 0
CLASSIFICATION_BOUNDARY_VARIANCE = 1
BURN_IN_ITERATIONS = 10
NUMBER_OF_ITERATIONS = 10

## Potential energy function 
## classification_boundary_vector: position vector
##
## Will return the potential energy scalar
potential_energy_function <- function (classification_boundary_vector) {
  
  total_energy = 0.0
  
  number_of_data_rows = length(training_data[["bias"]])
  for (row_counter in 1:number_of_data_rows) {
    
    vector_dot_product = sum(training_data[1, 1:length(training_data[1, ])-1] * classification_boundary_vector)
    
    total_energy = total_energy + (1 - training_data[row_counter, "Species"]) * vector_dot_product + log(1 + exp(-1 * vector_dot_product))
    
  }
  
  total_energy = total_energy + 0.5 * sum(classification_boundary_vector^2) / CLASSIFICATION_BOUNDARY_VARIANCE  

  return(total_energy)
  
}

## Potential energy gradient function 
## classification_boundary_vector: position vector
##
## Will return the potential energy gradient at the position vector passed in
potential_energy_gradient <- function (classification_boundary_vector) {
  
  energy_gradient = rep(0, length(training_data[1,]))
  
  number_of_data_rows = length(training_data[["bias"]])
  for (row_counter in 1:number_of_data_rows) {
    
    vector_dot_product = sum(training_data[1, 1:length(training_data[1, ])-1] * classification_boundary_vector)
    
    energy_gradient = energy_gradient + 
      (1 - training_data[row_counter, "Species"]) * training_data[1, 1:length(training_data[1, ])-1]  - 
      training_data[1, 1:length(training_data[1, ])-1] / (1 + exp(vector_dot_product))
    
  }
  
  energy_gradient = energy_gradient + (classification_boundary_vector / CLASSIFICATION_BOUNDARY_VARIANCE)
  
  return(energy_gradient)
  
}

# Get iris data for species versicolor and virginica
library(datasets)
iris_data = iris[c(which(iris[["Species"]] == "versicolor"), which(iris[["Species"]] == "virginica")), ]
iris_data = data.frame(bias = rep(1, length(iris_data[,1])), iris_data)

# Change labels from [versicolor, virginica] to [1, 0]
iris_data[["Species"]] = ifelse(iris_data[["Species"]] == "versicolor", VERSICOLOR_LABEL, VIRGINICA_LABEL)

#Separate into training and test data
training_data = rbind(iris_data[which(iris_data[["Species"]] == VERSICOLOR_LABEL), ][1:30,],
                      iris_data[which(iris_data[["Species"]] == VIRGINICA_LABEL), ][1:30,])
test_data = rbind(iris_data[which(iris_data[["Species"]] == VERSICOLOR_LABEL), ][31:50,],
                  iris_data[which(iris_data[["Species"]] == VIRGINICA_LABEL), ][31:50,])

# Generate a randow vector for classification
next_classification_boundary = rnorm(n=length(training_data[1,]), mean=CLASSIFICATION_BOUNDARY_MEAN, sd=sqrt(CLASSIFICATION_BOUNDARY_VARIANCE))

# Burn in
for (run_counter in 1:BURN_IN_ITERATIONS) {

    next_classification_boundary = Hamiltonian_Monte_Carlo(potential_energy_function=potential_energy_function, 
                                                         potential_energy_gradient=potential_energy_gradient, 
                                                         step_size=STEP_SIZE, 
                                                         number_of_steps=NUMBER_OF_STEPS, 
                                                         initial_position=next_classification_boundary)
}

classification_boundary_trend = next_classification_boundary

# Run HMC Simulation
for (run_counter in 1:NUMBER_OF_ITERATIONS) {
  
  next_classification_boundary = Hamiltonian_Monte_Carlo(potential_energy_function=potential_energy_function, 
                                                         potential_energy_gradient=potential_energy_gradient, 
                                                         step_size=STEP_SIZE, 
                                                         number_of_steps=NUMBER_OF_STEPS, 
                                                         initial_position=next_classification_boundary)
  
  classification_boundary_trend = rbind(classification_boundary_trend, next_classification_boundary)
  
}

classification_boundary_trend
