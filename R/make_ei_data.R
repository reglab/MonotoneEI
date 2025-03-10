#' Create an EI data object
#'
#' @param data A data.frame with columns for Xn, Yn (and weight if desired)
#' @param Xn A vector for Xn
#' @param Yn A vector for Yn
#' @param wt A vector for weight, if desired
#'
#' @return An EI data object with features Xn and Yn (and weight if applicable)
#' @export
#'

make_ei_data <- function(data = NA, Xn = NA, Yn = NA, wt = NA) {
  ei_data <- list(Xn = NA, Yn = NA, wt = NA)

  # data frame was passed
  if(is.data.frame(data)){
    # check data is correctly formatted
    if (ncol(data) < 2){
      stop("Error: data does not have sufficient columns. Did you mean to pass Xn and Yn vectors?")
    }
    else{
      x_col = colnames(data)[1]
      y_col = colnames(data)[2]
      ei_data$Xn = data[,x_col]
      ei_data$Yn = data[,y_col]

      if(ncol(data) == 2){
        print(paste0("Using column '", x_col, "' as Xn, and column '", y_col, "' as Yn. No weights passed."))
      } else{
        wt_col = colnames(data)[3]
        print(paste0("Using column '", x_col, "' as Xn, column '", y_col, "' as Yn, and column '", wt_col, "' as weights."))
        ei_data$wt = data[,wt_col]
      }
    }
  } else if(is.vector(Xn) && is.vector(Yn) &&
            (length(Xn)!=sum(is.na(Xn))) &&
            (length(Yn)!=sum(is.na(Yn)))){ # vectors were passed
    if(length(Xn) != length(Yn)){
      stop("Error: 'Xn' and 'Yn' must be vectors of the same length.")
    }

    ei_data$Xn = Xn
    ei_data$Yn = Yn

    if(is.vector(wt) && (length(wt)!=sum(is.na(wt)))){ # making sure wt is not just "NA"
      if(length(Xn) != length(wt)){
        stop("Error: 'wt' must be a vector of the same length as 'Xn' and 'Yn'")
      } else{
        ei_data$wt = wt
      }
    }
  } else{
    stop("Error: You must pass either a data.frame, 'data', or vectors 'Xn' and 'Yn'.")
  }

  # check for NAs
  if(sum(is.na(ei_data$Xn) + is.na(ei_data$Yn))>0){
    stop("Error: Xn or Yn contain NAs.")
  } else if(length(ei_data$wt) > 1 && sum(is.na(ei_data$wt)) > 0){ # wt contains NAs and is not just NA
    stop("Error: wt contains NAs.")
  }

  return(ei_data)
}
