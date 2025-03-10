#' Test if an EI data object is an EI data object
#'
#' @param data A potential EI data object
#'
#' @return True if an EI data object was passed, False otherwise
#' @export
#'

is_ei_data <- function(data){
  if(!is.list(data)){
    return(FALSE)
  }
  if(length(names(data)) != 3){
    return(FALSE)
  }
  if(sum(names(data) == c("Xn", "Yn", "wt")) != 3){ # if it doesn't have right list names
    return(FALSE)
  }
  if(length(data$Xn) != length(data$Yn)){
    return(FALSE)
  }
  if(!is.numeric(data$Xn) || !is.numeric(data$Yn)){
    return(FALSE)
  }
  return(TRUE)
}
