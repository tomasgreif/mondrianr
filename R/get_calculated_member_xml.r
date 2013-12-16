#' Get XML for calculated member
#'
#' Returns XML for calculated members.
#' 
#' @param engine See \code{\link{create_schema}} for details.
#' @param calculated_member See \code{\link{create_schema}} for details.
#' @param debug See \code{\link{create_schema}} for details.
#' 
#' @export 

get_calculated_member_xml <- function(engine, calculated_member=NA, debug=FALSE) {
  
  if(debug) cat('Creating XML for calculated member. \n')
  
  calculated_member_xml <- character(0)
  calculated_members <- 0
  
  if(!all(is.na(calculated_member))) {
    if(!is.list(calculated_member)) {
      stop('Calculated member have to be defined as list - see documentation for details')
    }
    
    if(!all(sapply(calculated_member, length) == 3)) {
      stop('Every list component should be a vector with 3 elements.')      
    }
    
    df <- do.call(rbind.data.frame, calculated_member)
    calculated_members <- nrow(df)
    names(df) <- c('name','formula','format')
    
    df$format <- ifelse(!is.na(df$format), paste0('formatString="',df$format,'"'),'')
    
    calculated_member_xml <- paste0(
      '<CalculatedMember name=" ',df$name,'" ', df$format,' formula="',df$formula,'" dimension="Measures" visible="true"></CalculatedMember>'
      ,collapse='\n')    

  }
    
    if(debug) cat('   Number of calculated members: ',calculated_members,'\n')    
    if(debug) cat('   XML for calculated members created. \n')    
    
  calculated_member_xml

}
