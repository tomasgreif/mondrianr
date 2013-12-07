#' Standardize name
#' 
#' Clean name into more human-readable form. Removes undescores, makes first letter upper-case.
#' 
#' @param Name String to be standardized.
#' @examples 
#' standardize_name("column_name")
#' @export 

standardize_name <- function(name=NA) {
     substr(name, 1, 1) <- toupper(substr(name, 1, 1))
     name <- gsub('_', ' ', name, fixed=TRUE)
     return(name)
}  


