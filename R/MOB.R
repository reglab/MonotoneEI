#' Calculate method of bounds
#'
#' @param data An EI data object
#' @param n_bootstrap_replicates The optional number of bootstrap replicates to obtain 95\% confidence intervals
#'
#' @return A list of: (MOB_1: (MOB lower bound for group 1, MOB upper bound for group 1),
#'                     MOB_0: (MOB lower bound for group 0, MOB upper bound for group 0),
#'                     MOB_disparity: (MOB lower bound for disparity, MOB upper bound for disparity),
#'                     MOB_1_conf: (MOB confidence interval lower bound for group 1, MOB confidence interval upper bound for group 1),
#'                     MOB_0_conf: (MOB confidence interval lower bound for group 0, MOB confidence interval upper bound for group 0),
#'                     MOB_disparity_conf: (MOB confidence interval lower bound for disparity, MOB confidence interval upper bound for disparity))
#' @export
#'

MOB <- function(data, n_bootstrap_replicates = 0){
  if(!is_ei_data(data)){
    stop("Error: This function takes only an EI data object. Please use make_ei_data() to make the data object.")
  }

  MOB_helper <- function(data){
    MOB_result = list(MOB_1 = NA,
                      MOB_0 = NA,
                      MOB_disparity = NA)

    max_X0 = pmin((1-data$Xn), data$Yn)
    min_X1 = pmax(data$Yn-max_X0, 0)
    max_X1 = pmin(data$Xn, data$Yn)
    min_X0 = pmax(data$Yn-max_X1, 0)

    MOB_result$MOB_1 = c(sum(min_X1)/sum(data$Xn), sum(max_X1)/sum(data$Xn))
    MOB_result$MOB_0 = c(sum(min_X0)/sum(1-data$Xn), sum(max_X0)/sum(1-data$Xn))
    MOB_result$MOB_disparity = c(sum(min_X1)/sum(data$Xn)-sum(max_X0)/sum(1-data$Xn),
                      sum(max_X1)/sum(data$Xn)-sum(min_X0)/sum(1-data$Xn))
    return(MOB_result)
  }

  MOB_result <- MOB_helper(data)

  MOB_1_lower_vec = rep(0, n_bootstrap_replicates)
  MOB_1_upper_vec = rep(0, n_bootstrap_replicates)
  MOB_0_lower_vec = rep(0, n_bootstrap_replicates)
  MOB_0_upper_vec = rep(0, n_bootstrap_replicates)
  MOB_disparity_lower_vec = rep(0, n_bootstrap_replicates)
  MOB_disparity_upper_vec = rep(0, n_bootstrap_replicates)

  if(n_bootstrap_replicates > 0){
    for(i in 1:n_bootstrap_replicates){
      bootstrap_data_rows = sample(1:length(data$Xn), length(data$Xn), replace = TRUE)
      bootstrap_data <- data
      bootstrap_data$Xn = bootstrap_data$Xn[bootstrap_data_rows]
      bootstrap_data$Yn = bootstrap_data$Yn[bootstrap_data_rows]
      bootstrap_data$wt = bootstrap_data$wt[bootstrap_data_rows]

      bootstrap_MOB <- MOB_helper(bootstrap_data)
      MOB_1_lower_vec[i] <- bootstrap_MOB$MOB_1[1]
      MOB_1_upper_vec[i] <- bootstrap_MOB$MOB_1[2]
      MOB_0_lower_vec[i] <- bootstrap_MOB$MOB_0[1]
      MOB_0_upper_vec[i] <- bootstrap_MOB$MOB_0[2]
      MOB_disparity_lower_vec[i] <- bootstrap_MOB$MOB_disparity[1]
      MOB_disparity_upper_vec[i] <- bootstrap_MOB$MOB_disparity[2]
    }

    MOB_result$MOB_1_conf = list(MOB_1_lower_vec, MOB_1_upper_vec)#c(unname(stats::quantile(MOB_1_lower_vec, .05)),
                              #unname(stats::quantile(MOB_1_upper_vec, .95)))

    MOB_result$MOB_0_conf = list(MOB_0_lower_vec, MOB_0_upper_vec) #c(unname(stats::quantile(MOB_0_lower_vec, .05)),
                              #unname(stats::quantile(MOB_0_upper_vec, .95)))

    MOB_result$MOB_disparity_conf = list(MOB_disparity_lower_vec, MOB_disparity_upper_vec) #c(unname(stats::quantile(MOB_disparity_lower_vec, .05)),
                                      #unname(stats::quantile(MOB_disparity_upper_vec, .95)))
  }

  return(MOB_result)
}


