library(imager)

IMAGE_WIDTH_DIMENSION = 1
IMAGE_HEIGHT_DIMENSION = 2

## Retrieve Image 
## image_name: image file name
##
## Will return the image
retrieve_image <- function (image_name) {
  
  image_array = load.image(image_name)
  image_array = image_array * 20 - 10
  return(image_array)
  
}

## Retrieve pixel neighbors 
## image_array: image array representation
## pixel_index: pixel index starting at 1
##
## Will return the pixel north, east, south and west neighbors
get_pixel_neighbors(image_array, pixel_index) {
  
  image_width = dim(image_array)[IMAGE_WIDTH_DIMENSION]
  image_height = dim(image_array)[IMAGE_HEIGHT_DIMENSION]
  
  
  
  
  
  
  
  
}

noisy_message = retrieve_image("noisy-message.png")
