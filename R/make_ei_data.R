make_ei_data <- function(data = NA, Xn = NA, Yn = NA, wt = NA) {
  # data frame was passed
  if(!is.na(data)){
    # check data is correctly formatted
    if(!is.data.frame(data)){
      stop("Error: data must be a data.frame. Did you mean to pass Xn and Yn vectors?")
    }
    if (ncol(data) < 2){
      stop("Error: data does not have sufficient columns. Did you mean to pass Xn and Yn vectors?")
    }else if(ncol(data) == 2){
      x_col = colnames(data)[1]
      y_col = colnames(data)[2]
      print(paste0("Using column ", x_col, " as Xn, and column ", y_col, " as Yn. No weights passed."))
    } else{
      x_col = colnames(data)[1]
      y_col = colnames(data)[2]
      wt_col = colnames(data)[3]
      print(paste0("Using column ", x_col, " as Xn, column ", y_col, " as Yn, and column ", wt_col, " as weights."))
    }


  }

  # vectors were passed
  else{
    if(is.na(Xn) | is.na(Yn)){
      stop("Error: Missing data, Xn, or Yn. You must pass either data or Xn and Yn")
    }
  }
}
