library(imager)

# Image dimension constants
IMAGE_WIDTH_DIMENSION = 1
IMAGE_HEIGHT_DIMENSION = 2

# Image constants
BLACK_PIXEL = -1.0
WHITE_PIXEL = 1.0

# Model parameters
BLACK_TO_WHITE_PIXEL_RATIO = 0
PIXEL_SIMILARITY_TO_NEIGHBORS = 1
PIXEL_DISSIMILARITY_TO_NOISY_COUNTERPART = 1

# Run parameters
BURN_IN_ITERATIONS = 10
DENOISING_ITERATIONS = 20
NORTH_NEIGHBOR = "North"
EAST_NEIGHBOR = "East"
SOUTH_NEIGHBOR = "South"
WEST_NEIGHBOR = "West"
NOISE_SD_INITIAL_ESTIMATE = 5

std_dev_estimates = list()

## Retrieve Image 
## image_name: image file name
##
## Will return the image
retrieve_image <- function (image_name) {
  
  image_array = load.image(image_name)
  image_array = image_array * 20 - 10
  return(image_array)
  
}

## Get half the checkerboard representation of the image 
## which is all pixels corresponding to checkerboard squares of one color
## white_squares: boolean signifying whether white squares are needed
## pixel_index: pixel index starting at 1
## image_width: image width (NUin pixels
##
## Will return the pixel pixel indexes for the white or black part of the checkerboard
get_half_checkerboard = function(white_squares, image_pixel_indexes, image_width) {
  
  even_row = (floor((image_pixel_indexes - 1) / image_width) + 1) %% 2 == 0
  even_pixel_column = (((image_pixel_indexes - 1 ) %% image_width) + 1) %% 2 == 0
  
  if (isTRUE(white_squares)) {
    return(xor(even_row, even_pixel_column))
  } else {
    return(!xor(even_row, even_pixel_column))
  }
  
}

## Retrieve pixel neighbors 
## image_array: image array representation
## pixel_index: pixel index starting at 1
##
## Will return the pixel north, east, south and west neighbors
get_pixel_neighbors = function(pixel_index, image_width, image_height) {
  
  on_first_row = floor((pixel_index - 1) / image_width) == 0
  neighbor_to_north = ifelse(on_first_row, NA, pixel_index - image_width)
  
  on_right_edge = pixel_index %% image_width == 0
  neighbor_to_east = ifelse(on_right_edge, NA, pixel_index + 1)
  
  on_last_row = pixel_index + image_width > image_width * image_height
  neighbor_to_south = ifelse(on_last_row, NA, pixel_index + image_width)
  
  on_left_edge = (pixel_index - 1) %% image_width == 0
  neighbor_to_west = ifelse(on_left_edge, NA, pixel_index - 1)
  
  pixel_neighbors = list(neighbor_to_north, neighbor_to_east, neighbor_to_south, neighbor_to_west)
  names(pixel_neighbors) = c(NORTH_NEIGHBOR, EAST_NEIGHBOR, SOUTH_NEIGHBOR, WEST_NEIGHBOR)
  return(pixel_neighbors)
  
}

get_pixel_energy_level = function(pixel_index, pixel_neighbors, denoised_image, noisy_image, black_pixel, ising_prior_only, black_white_ratio, pixel_neighbor_similarity, pixel_similarity_to_noisy) {

  # Get pixel neighbors
  pixel_north_neighbor = pixel_neighbors[[NORTH_NEIGHBOR]] 
  pixel_east_neighbor = pixel_neighbors[[EAST_NEIGHBOR]] 
  pixel_south_neighbor = pixel_neighbors[[SOUTH_NEIGHBOR]] 
  pixel_west_neighbor = pixel_neighbors[[WEST_NEIGHBOR]] 
  
  # Compute pixel energy based on pixel color parameter
  pixel_color = WHITE_PIXEL
  if (isTRUE(black_pixel)) {
    pixel_color = BLACK_PIXEL
  }
  
  # Energy based on proportion of black to white pixels
  pixel_energy = black_white_ratio * pixel_color * rep(1:length(pixel_index))
  
  # Energy due to similarity with neighbors
  pixel_energy = pixel_energy + 
    ifelse(is.na(pixel_north_neighbor), 1, pixel_neighbor_similarity * pixel_color * denoised_image[pixel_north_neighbor]) + 
    ifelse(is.na(pixel_east_neighbor), 1, pixel_neighbor_similarity * pixel_color * denoised_image[pixel_east_neighbor]) +
    ifelse(is.na(pixel_south_neighbor), 1, pixel_neighbor_similarity * pixel_color * denoised_image[pixel_south_neighbor]) +
    ifelse(is.na(pixel_west_neighbor), 1, pixel_neighbor_similarity * pixel_color * denoised_image[pixel_west_neighbor])
  
  # Energy due to closeness to noisy image
  if (!isTRUE(ising_prior_only)) {
    pixel_energy = pixel_energy - (0.5 * (pixel_color - noisy_image[pixel_index]) ^ 2) / pixel_similarity_to_noisy
  }
  
  return(pixel_energy)
  
}

## Denoise half the image in a checkerboard pattern
## denoised_image: denoised image 
## noisy_image: grayscale noisy image
## white_squares: boolean for white squares of checkerboard
##
## Will return the the image after denoising
denoise_half_checkerboard = function(denoised_image, noisy_image, white_squares, ising_prior_only, show_animation, black_white_ratio, pixel_neighbor_similarity, pixel_similarity_to_noisy, denoising_iteration) {
  
  # Get pixel indexes corresponding to half the checkerboard
  pixel_present = get_half_checkerboard(white_squares, 
                                        image_pixel_indexes=seq(1, length(noisy_image)), 
                                        image_width=dim(noisy_image)[IMAGE_WIDTH_DIMENSION])
  pixel_index = seq(1, length(noisy_image))[pixel_present]
  
  # Get get the 4 neighboring pixels for each pixel in the checkerboard
  pixel_neighbors = get_pixel_neighbors(pixel_index, 
                                        image_width=dim(noisy_image)[IMAGE_WIDTH_DIMENSION], 
                                        image_height=dim(noisy_image)[IMAGE_HEIGHT_DIMENSION])
  
  # Get energy level of pixels
  black_pixel_energy_level = exp(get_pixel_energy_level(pixel_index, pixel_neighbors, denoised_image, noisy_image, black_pixel=TRUE, ising_prior_only, black_white_ratio, pixel_neighbor_similarity, pixel_similarity_to_noisy))
  white_pixel_energy_level = exp(get_pixel_energy_level(pixel_index, pixel_neighbors, denoised_image, noisy_image, black_pixel=FALSE, ising_prior_only, black_white_ratio, pixel_neighbor_similarity, pixel_similarity_to_noisy))

  # Compute probability of pixel being black
  black_pixel_probability = black_pixel_energy_level / (black_pixel_energy_level + white_pixel_energy_level)
  
  # Sample from the probability distribution and update mean image
  denoised_image[pixel_index] = 
    (denoised_image[pixel_index] * denoising_iteration + ifelse(black_pixel_probability >= runif(n=length(black_pixel_energy_level)), BLACK_PIXEL, WHITE_PIXEL)) / (denoising_iteration + 1)
  
  if (isTRUE(show_animation)) {
    plot(denoised_image)
    ani.pause()
  }
  
  # Return the denoised image corresponding to half the checkerboard
  return(denoised_image)
  
}

## Estimate the noise variance
## denoised_image: denoised image
## noisy_image: grayscale noisy image
## previous_variance_estimate: previous estimate of the variqnce
##
## Will return the estimate of the noise variance
estimate_noise_variance = function(denoised_image, noisy_image, previous_std_dev_estimate) {
  
  if (!is.null(previous_std_dev_estimate)) {
    current_std_dev_estimate = previous_std_dev_estimate
    std_dev_range_start = current_std_dev_estimate * 0.9
  } else {
    current_std_dev_estimate = NOISE_SD_INITIAL_ESTIMATE
    std_dev_range_start = ceiling(current_std_dev_estimate * 0.1)
  }
  std_dev_range_step = (current_std_dev_estimate - std_dev_range_start)/5
  std_dev_range_end = std_dev_range_start + 10 * std_dev_range_step
  
  log_likelihood_trend = NULL
  for (noise_std_dev in seq(from=std_dev_range_start, to=std_dev_range_end, by=std_dev_range_step)) {
    
    # Compute log likelihood of the current denoised image
    log_likelihood = sum(log(dnorm(x=noisy_image-denoised_image, sd=noise_std_dev)))
    if (!is.null(log_likelihood_trend)) {
      log_likelihood_trend = rbind(log_likelihood_trend, log_likelihood)
    } else {
      log_likelihood_trend = log_likelihood
    }
    
  }
  
  # Return the variance corresponding to maximum log likelihood
  return(seq(from=std_dev_range_start, to=std_dev_range_end, by=std_dev_range_step)[which(log_likelihood_trend == max(log_likelihood_trend))])
  
}

## Denoise the image using Gibbs Sampling
## noisy_image: grayscale noisy image
##
## Will return the image after denoising
gibbs_sampling = function(noisy_image, ising_prior_only=FALSE, initialize_random=FALSE, show_animation, black_white_ratio, pixel_neighbor_similarity, pixel_similarity_to_noisy, estimate_noise_variance) {
  
  # Initialize denoised image
  if (isTRUE(initialize_random)) {
    denoised_image = runif(n=length(noisy_image), min=-1, max=1)
    dim(denoised_image) = dim(noisy_image)
    denoised_image = cimg(denoised_image)
  } else {
    denoised_image = noisy_image
  }
  
  # Burn in
  for (iteration_counter in 1:BURN_IN_ITERATIONS) {
    denoised_image = denoise_half_checkerboard(denoised_image, noisy_image, white_squares=TRUE, ising_prior_only, show_animation, black_white_ratio, pixel_neighbor_similarity, pixel_similarity_to_noisy, denoising_iteration=iteration_counter)
    denoised_image = denoise_half_checkerboard(denoised_image, noisy_image, white_squares=FALSE, ising_prior_only, show_animation, black_white_ratio, pixel_neighbor_similarity, pixel_similarity_to_noisy, denoising_iteration=iteration_counter)
  }
  
  std_dev_estimate = NULL
  
  # Denoise the image
  for (iteration_counter in 1:DENOISING_ITERATIONS) {
    denoised_image = denoise_half_checkerboard(denoised_image, noisy_image, white_squares=TRUE, ising_prior_only, show_animation, black_white_ratio, pixel_neighbor_similarity, pixel_similarity_to_noisy, denoising_iteration=BURN_IN_ITERATIONS+iteration_counter)
    denoised_image = denoise_half_checkerboard(denoised_image, noisy_image, white_squares=FALSE, ising_prior_only, show_animation, black_white_ratio, pixel_neighbor_similarity, pixel_similarity_to_noisy, denoising_iteration=BURN_IN_ITERATIONS+iteration_counter)
    if (isTRUE(estimate_noise_variance)) {
      std_dev_estimate = estimate_noise_variance(denoised_image, noisy_image, std_dev_estimate)
    }

  }
  
  std_dev_estimates[length(std_dev_estimates) + 1] <<- std_dev_estimate
  
  return(denoised_image)
  
}

denoise_image = function(image_file, ising_prior_only=FALSE, initialize_random=TRUE, show_animation=FALSE, black_white_ratio=BLACK_TO_WHITE_PIXEL_RATIO, pixel_neighbor_similarity=PIXEL_SIMILARITY_TO_NEIGHBORS, pixel_similarity_to_noisy=PIXEL_DISSIMILARITY_TO_NOISY_COUNTERPART, estimate_noise_variance=TRUE) {
  
  if (isTRUE(show_animation)) {
    library(animation)
  }
  
  image_array = retrieve_image(image_file)

  if (isTRUE(show_animation)) {
    oopt <- ani.options(interval = 0.2, nmax = BURN_IN_ITERATIONS + DENOISING_ITERATIONS)
  }
  
  denoised_image = gibbs_sampling(image_array, ising_prior_only, initialize_random, show_animation, black_white_ratio, pixel_neighbor_similarity, pixel_similarity_to_noisy, estimate_noise_variance)
  
  if (isTRUE(show_animation)) {
    ani.options(oopt)
  } else {
    plot(denoised_image, main=paste(image_file, ", Alpha =", black_white_ratio, ", Beta =", pixel_neighbor_similarity, ", Variance =", pixel_similarity_to_noisy))
  }
  
}

denoise_message = function() {
  denoise_image(image_file="noisy-message.png", ising_prior_only=FALSE, initialize_random=TRUE, show_animation=TRUE)
}


denoise_yinyang = function() {
  denoise_image(image_file="noisy-yinyang.png", ising_prior_only=FALSE, initialize_random=TRUE, show_animation=TRUE)
}

generate_gifs = function() {
  library(animation)
  saveGIF({ani.options(interval = 0.2, nmax = BURN_IN_ITERATIONS + DENOISING_ITERATIONS)
    par(mar = c(4, 4, .1, 0.1), mgp = c(2, 0.7, 0))
    denoise_message()}, movie.name = "noisy-message.gif", img.name = "Rplot",
    convert = "convert", cmd.fun = system, clean = TRUE)
  
  saveGIF({ani.options(interval = 0.2, nmax = BURN_IN_ITERATIONS + DENOISING_ITERATIONS)
    par(mar = c(4, 4, .1, 0.1), mgp = c(2, 0.7, 0))
    denoise_yinyang()}, movie.name = "noisy-yinyang.gif", img.name = "Rplot",
    convert = "convert", cmd.fun = system, clean = TRUE)
}

one_a_ising_prior_only = function() {

  for (alpha in c(0.0000001, 0)) {
    for (beta in c(0.75, 1.0, 1.25)) {
      
      denoise_image(image_file="noisy-message.png", ising_prior_only=TRUE, initialize_random=TRUE, show_animation=FALSE, black_white_ratio=alpha, pixel_neighbor_similarity=beta, estimate_noise_variance=FALSE)
      
      denoise_image(image_file="noisy-yinyang.png", ising_prior_only=TRUE, initialize_random=TRUE, show_animation=FALSE, black_white_ratio=alpha, pixel_neighbor_similarity=beta, estimate_noise_variance=FALSE)
      
    }
  }

}

full_posterior = function() {
  
  denoise_image(image_file="noisy-message.png", ising_prior_only=FALSE, initialize_random=TRUE, show_animation=FALSE, black_white_ratio=BLACK_TO_WHITE_PIXEL_RATIO, pixel_neighbor_similarity=PIXEL_SIMILARITY_TO_NEIGHBORS, estimate_noise_variance=TRUE)

  denoise_image(image_file="noisy-yinyang.png", ising_prior_only=FALSE, initialize_random=TRUE, show_animation=FALSE, black_white_ratio=BLACK_TO_WHITE_PIXEL_RATIO, pixel_neighbor_similarity=PIXEL_SIMILARITY_TO_NEIGHBORS, estimate_noise_variance=TRUE)

}

print_std_dev_estimates = function() {
  
  print(paste("Standard deviation estimate for noisy message:", std_dev_estimates[1]))
  
  print(paste("Standard deviation estimate for noisy yinyang:", std_dev_estimates[2]))
  
}
