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
DENOISING_ITERATIONS = 40
NORTH_NEIGHBOR = "North"
EAST_NEIGHBOR = "East"
SOUTH_NEIGHBOR = "South"
WEST_NEIGHBOR = "West"

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
  even_pixel_column = (image_pixel_indexes %% image_width) %% 2 == 0
  
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

get_pixel_energy_level = function(pixel_index, pixel_neighbors, denoised_image, noisy_image, black_pixel) {

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
  pixel_energy = BLACK_TO_WHITE_PIXEL_RATIO * pixel_color
  
  # Energy due to similarity with neighbors
  for (pixel_neighbor in c(pixel_north_neighbor, pixel_east_neighbor, pixel_south_neighbor, pixel_west_neighbor)) {
    if (!is.na(pixel_neighbor)) {
      pixel_energy = pixel_energy + PIXEL_SIMILARITY_TO_NEIGHBORS * pixel_color * pixel_neighbor
    }
  }
  
  # Energy due to closeness to noisy image
  pixel_energy = pixel_energy - (0.5 * (pixel_color - noisy_image[pixel_index]) ^ 2) / PIXEL_DISSIMILARITY_TO_NOISY_COUNTERPART
  
  return(pixel_energy)
  
}

## Denoise half the image in a checkerboard pattern
## denoised_image: denoised image 
## noisy_image: grayscale noisy image
## white_squares: boolean for white squares of checkerboard
##
## Will return the the image after denoising
denoise_half_checkerboard = function(denoised_image, noisy_image, white_squares) {
  
  # Get pixel indexes corresponding to half the checkerboard
  pixel_index = get_half_checkerboard(white_squares, 
                                      image_pixel_indexes=rep(1, length(noisy_image)), 
                                      image_width=dim(noisy_image)[IMAGE_WIDTH_DIMENSION])
  
  # Get get the 4 neighboring pixels for each pixel in the checkerboard
  pixel_neighbors = get_pixel_neighbors(pixel_index, 
                                        image_width=dim(noisy_image)[IMAGE_WIDTH_DIMENSION], 
                                        image_height=dim(noisy_image)[IMAGE_HEIGHT_DIMENSION])
  
  # Get energy level of pixels
  black_pixel_energy_level = get_pixel_energy_level(pixel_index, pixel_neighbors, denoised_image, noisy_image, black_pixel=TRUE)
  white_pixel_energy_level = get_pixel_energy_level(pixel_index, pixel_neighbors, denoised_image, noisy_image, black_pixel=FALSE)

  # Compute probability of pixel being black
  black_pixel_probability = black_pixel_energy_level / (black_pixel_energy_level + white_pixel_energy_level)
  
  # Sample from the probability distribution
  denoised_image[pixel_index] = ifelse(black_pixel_energy_level >= runif(n=length(black_pixel_energy_level)), BLACK_PIXEL, WHITE_PIXEL)
  
  # Return the denoised image corresponding to half the checkerboard
  return(denoised_image)
  
}

## Denoise the image
## noisy_image: grayscale noisy image
##
## Will return the the image after denoising
denoise_image = function(noisy_image) {
  
  # Initialize denoised image
  denoised_image = noisy_image
  
  # Burn in
  for (iteration_counter in 1:BURN_IN_ITERATIONS) {
    denoised_image = denoise_half_checkerboard(denoised_image, noisy_image, white_squares=TRUE)
    denoised_image = denoise_half_checkerboard(denoised_image, noisy_image, white_squares=FALSE)
  }
  
  # Denoise the image
  for (iteration_counter in 1:DENOISING_ITERATIONS) {
    denoised_image = denoise_half_checkerboard(denoised_image, noisy_image, white_squares=TRUE)
    denoised_image = denoise_half_checkerboard(denoised_image, noisy_image, white_squares=FALSE)
  }
  
  return(denoised_image)
  
}

noisy_message = retrieve_image("noisy-message.png")
denoised_message = denoise_image(noisy_message)
