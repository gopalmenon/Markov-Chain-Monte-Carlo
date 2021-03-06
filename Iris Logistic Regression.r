source("Hamiltonian MCMC.r")

VERSICOLOR_LABEL = 1
VIRGINICA_LABEL = 0

# HMC Parameters
STEP_SIZE = 0.04
NUMBER_OF_STEPS = 40
CLASSIFICATION_BOUNDARY_MEAN = 0
CLASSIFICATION_BOUNDARY_VARIANCE = 1
BURN_IN_ITERATIONS = 10
NUMBER_OF_ITERATIONS = 400

prediction_error = 0
## Potential energy function 
## classification_boundary_vector: position vector
##
## Will return the potential energy scalar
potential_energy_function <- function (classification_boundary_vector) {
  
  total_energy = t(as.matrix(1 - training_data[, "Species"])) %*% as.matrix(training_data[, 1:length(training_data[1, ])-1]) %*% t(as.matrix(classification_boundary_vector)) +
    sum(log(1+exp(-1*as.matrix(training_data[, 1:length(training_data[1, ])-1])%*% t(as.matrix(classification_boundary_vector))))) +
    0.5 * sum(classification_boundary_vector^2) / CLASSIFICATION_BOUNDARY_VARIANCE
  
  return(total_energy)
  
}

## Potential energy gradient function 
## classification_boundary_vector: position vector
##
## Will return the potential energy gradient at the position vector passed in
potential_energy_gradient <- function (classification_boundary_vector) {
  
  energy_gradient = t(as.matrix(1 - training_data[, "Species"])) %*% as.matrix(training_data[, 1:length(training_data[1, ])-1]) -
    rowSums(t(training_data[, 1:length(training_data[1, ])-1]/(1+exp(as.matrix(training_data[, 1:length(training_data[1, ])-1])%*% t(as.matrix(classification_boundary_vector)))))) +
    (classification_boundary_vector / CLASSIFICATION_BOUNDARY_VARIANCE)
  
  return(energy_gradient)
  
}

zero_one_loss = 0

run_iris_logistic_regression = function() {
    
  # Get iris data for species versicolor and virginica
  library(datasets)
  
  start_time = Sys.time()
  
  iris_data = iris[c(which(iris[["Species"]] == "versicolor"), which(iris[["Species"]] == "virginica")), ]
  iris_data = data.frame(bias = rep(1, length(iris_data[,1])), iris_data)
  
  # Change labels from [versicolor, virginica] to [1, 0]
  iris_data[["Species"]] = ifelse(iris_data[["Species"]] == "versicolor", VERSICOLOR_LABEL, VIRGINICA_LABEL)
  
  #Separate into training and test data
  training_data <<- rbind(iris_data[which(iris_data[["Species"]] == VERSICOLOR_LABEL), ][1:30,],
                        iris_data[which(iris_data[["Species"]] == VIRGINICA_LABEL), ][1:30,])
  test_data <<- rbind(iris_data[which(iris_data[["Species"]] == VERSICOLOR_LABEL), ][31:50,],
                    iris_data[which(iris_data[["Species"]] == VIRGINICA_LABEL), ][31:50,])
  
  # Generate a randow vector for classification
  previous_classification_boundary = t(as.matrix(rnorm(n=length(training_data[1,])-1, mean=CLASSIFICATION_BOUNDARY_MEAN, sd=sqrt(CLASSIFICATION_BOUNDARY_VARIANCE))))
  
  # Burn in
  for (run_counter in 1:BURN_IN_ITERATIONS) {
  
    previous_classification_boundary = Hamiltonian_Monte_Carlo(potential_energy_function=potential_energy_function, 
                                                             potential_energy_gradient=potential_energy_gradient, 
                                                             step_size=STEP_SIZE, 
                                                             number_of_steps=NUMBER_OF_STEPS, 
                                                             initial_position=previous_classification_boundary)
  }
  
  classification_boundary_trend = previous_classification_boundary
  
  # Run HMC Simulation
  rejection_counter = 0
  for (run_counter in 1:NUMBER_OF_ITERATIONS) {
  
    next_classification_boundary = Hamiltonian_Monte_Carlo(potential_energy_function=potential_energy_function, 
                                                           potential_energy_gradient=potential_energy_gradient, 
                                                           step_size=STEP_SIZE, 
                                                           number_of_steps=NUMBER_OF_STEPS, 
                                                           initial_position=previous_classification_boundary)
    
    if (all(previous_classification_boundary == next_classification_boundary)) {
      rejection_counter = rejection_counter + 1
    } else {
      previous_classification_boundary = next_classification_boundary
    }
    
    classification_boundary_trend = rbind(classification_boundary_trend, next_classification_boundary)
    
  }
  
  end_time = Sys.time()
  
  # Plot the components of the decision boundary vector
  for (plot_counter in 1:length(classification_boundary_trend[1,])) {
    plot(rep(1:length(classification_boundary_trend[,plot_counter])), classification_boundary_trend[,plot_counter], type='l', xlab='Iteration', ylab='Value', main=paste('Trend for', names(classification_boundary_trend[1,])[plot_counter], 'multiplier'))
    hist(classification_boundary_trend[,plot_counter], freq=FALSE, main=paste('Histogram for', names(classification_boundary_trend[1,])[plot_counter], 'multiplier'), xlab=paste(names(classification_boundary_trend[1,])[plot_counter], 'multiplier'), ylab="Density", col="blue")
  }
  
  # Classification boundary is the average vector
  classification_boundary = colSums(classification_boundary_trend) / length(classification_boundary_trend[, 1])
  
  # Make test data predictions based on classification boundary
  predicted_elements = as.matrix(test_data[, 1:length(test_data[1, ])-1])%*%as.matrix(t(classification_boundary_trend))
  predicted_labels = ifelse(rowSums(predicted_elements)/dim(predicted_elements)[2] >= 0.5, 1, 0)
  actual_labels = test_data[, length(test_data[1, ])]
  prediction_error = actual_labels - predicted_labels
  average_prediction_error = sum(abs(prediction_error)) / length(prediction_error)
  zero_one_loss <<- sum(abs(prediction_error))
  
}

print_results = function() {
  print(paste("The zero-one loss was", zero_one_loss))
}
