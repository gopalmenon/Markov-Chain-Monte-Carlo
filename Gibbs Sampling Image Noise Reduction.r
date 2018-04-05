library(imager)

# Image dimension constants
IMAGE_WIDTH_DIMENSION = 1
IMAGE_HEIGHT_DIMENSION = 2

# Model parameters
BLACK_TO_WHITE_PIXEL_RATION = 0
PIXEL_SIMILARITY_TO_NEIGHBORS = 1
PIXEL_DISSIMILARITY_TO_NOISY_COUNTERPART = 1

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
get_half_checkerboard = function(white_squares, pixel_index, image_width) {
  
  even_row = (floor((pixel_index - 1) / image_width) + 1) %% 2 == 0
  even_pixel_column = (pixel_index %% image_width) %% 2 == 0
  
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
  names(pixel_neighbors) = c("North", "East", "South", "West")
  return(pixel_neighbors)
  
}

## Compute the probability of the pixel being black
## image_array: image array representation
## pixel_index: pixel index starting at 1
##
## Will return the the probability of the pixel being black
get_black_pixel_probability = function(pixel_index, denoised_image, noisy_image) {
  
  pixel_neighbors = get_pixel_neighbors(pixel_index=pixel_index, 
                                        image_width=dim(noisy_image)[IMAGE_WIDTH_DIMENSION], 
                                        image_height=dim(noisy_image)[IMAGE_HEIGHT_DIMENSION])
  
  
  
  
  
  
}

noisy_message = retrieve_image("noisy-message.png")
tst = get_pixel_neighbors(rep(1:length(noisy_message))[wcb], dim(noisy_message)[IMAGE_WIDTH_DIMENSION], dim(noisy_message)[IMAGE_HEIGHT_DIMENSION])