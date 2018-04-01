## Run Hamiltonian Monte Carlo Simulation 
## From: MCMC using Hamiltonian dynamics, Radford M. Neal, University of Toronto, 2012
## potential_energy_function: function to be sampled
## potential_energy_gradient: gradient of function to be sampled
## step_size: size of each step taken
## number_of_steps: number of steps taken
## initial_position: initial value of the variable
##
## Will return the final position of the variable
Hamiltonian_Monte_Carlo <- function (potential_energy_function, potential_energy_gradient, step_size, number_of_steps, initial_position) {
  
  variable_position = initial_position
  variable_momentum = rnorm(n=length(variable_position), mean=0, sd=1) # independent standard normal variates 
  initial_momentum = variable_momentum
  
  # Make a half step for momentum at the beginning 
  variable_momentum = variable_momentum - step_size * potential_energy_gradient(variable_position) / 2
  
  # Alternate full steps for position and momentum
  for (step_counter in 1:number_of_steps) {
    
    # Make a full step for the position
    variable_position = variable_position + step_size * variable_momentum
    
    # Make a full step for the momentum, except at end of trajectory
    if (step_counter != number_of_steps) {
      variable_momentum = variable_momentum - step_size * potential_energy_gradient(variable_position)
    }
    
  }
  
  # Make a half step for momentum at the end.
  variable_momentum = variable_momentum - step_size * potential_energy_gradient(variable_position) / 2
  
  # Negate momentum at end of trajectory to make the proposal symmetric
  variable_momentum = -variable_momentum
  
  # Evaluate potential and kinetic energies at start and end of trajectory
  initial_potential_energy = potential_energy_function(initial_position) 
  initial_kinetic_energy = sum(initial_momentum^2) / 2 
  proposed_potential_energy = potential_energy_function(variable_position)
  proposed_kinetic_energy = sum(variable_momentum^2) / 2
  
  # Accept or reject the state at end of trajectory, returning either 
  # the position at the end of the trajectory or the initial position
  if (runif(n=1) < exp((initial_potential_energy + initial_kinetic_energy) - (proposed_potential_energy + proposed_kinetic_energy))) {
    return (variable_position) # accept
  } else {
    return (initial_position) # reject
  }
}