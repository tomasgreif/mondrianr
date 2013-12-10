#' Standardize name
#' 
#' Clean name into more human-readable form. Removes undescores, makes first letter upper-case.
#' 
#' @param name String to be standardized.
#' @param debug Print additional information useful for debugging.
#' @examples 
#' standardize_name("column_name")
#' @export 

standardize_name <- function(name=NA, debug=FALSE) {
     substr(name, 1, 1) <- toupper(substr(name, 1, 1))
     name <- gsub('_', ' ', name, fixed=TRUE)
     return(name)
}  


