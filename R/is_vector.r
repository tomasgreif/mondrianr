#' Check if argument is vector with given properties
#'
#' Checks if argument is vector with given length and mode. Used to control arguments. Returns error when object
#' does not match given properties.
#' 
#' @param vector Input object.
#' @param length Required length of vector.
#' @param mode Required mode of vector


is_vector <- function(vector, length=1, mode='character') {
  if(!is.vector(vector)) {
    
    stop('Incorrect argument value: ', deparse(substitute(vector)),' is not vector.')
    
  } else if (is.vector(vector) & length(vector) != length) {
    
    stop('Incorrect argument value: ',deparse(substitute(vector)),' is vector with wrong length. Expected length is: ', length)
    
  } else if (!all(is.na(vector)) & !is.na(mode) & mode != mode(vector)) {
    
    stop('Incorrect vector mode for ',deparse(substitute(vector)) ,': ', mode(vector), ' Expected mode is: ', mode )
  }    
  
  return(invisible(TRUE))
}
