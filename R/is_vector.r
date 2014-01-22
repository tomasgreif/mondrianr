#' Check if argument is vector with given properties
#'
#' Checks if argument is vector with given length and mode. Used to control arguments. Returns error when object
#' does not match given properties.
#' 
#' @param vector Input object.
#' @param length Required length of vector.
#' @param mode Required mode of vector
#' @examples
#' is_vector(c(1:9),9,'numeric')
#' is_vector('a')
#' is_vector(TRUE,mode='logical')
#' 
#' \dontrun{
#' is_vector(TRUE,mode='character')
#' is_vector(c('a','b'),length=3)
#' is_vector(iris)}

is_vector <- function(vector, length=1, mode='character') {
  if(!is.vector(vector)) {
    
    stop('Argument "', deparse(substitute(vector)),'" is not vector. Argument expects ', mode,
         ' vector of length ', length ,', not ', mode(vector),'.')
    
  } else if (is.vector(vector) & length(vector) != length) {
    
    stop('Incorrect length of argument "',deparse(substitute(vector)),
         '". Expected length is ', length, ' not ', length(vector),'.')
    
  } else if (!all(is.na(vector)) & !is.na(mode) & mode != mode(vector)) {
    
    stop('Incorrect vector mode for "',deparse(substitute(vector)) ,'". Expected mode is ', mode, ' not ', mode(vector) ,'.')
  }    
  
  return(invisible(TRUE))
}
