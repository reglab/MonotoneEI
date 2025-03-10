#' Calculate neighborhood model estimate
#'
#' @param data An EI data object
#' @param n_bootstrap_replicates The optional number of bootstrap replicates to obtain 95\% confidence intervals
#'
#' @return A list of: (NM_1: Neighborhood Model estimate for group 1,
#'                     NM_0: Neighborhood Model estimate for group 0,
#'                     NM_disparity: Neighborhood Model disparity estimate,
#'                     NM_1_conf: 90\% Confidence interval around group 1,
#'                     NM_0_conf: 90\% Confidence interval around group 0,
#'                     NM_disparity_conf: 90\% Confidence interval around disparity)
#' @export
#'

NM <- function(data, n_bootstrap_replicates = 0){
  if(!is_ei_data(data)){
    stop("Error: This function takes only an EI data object. Please use make_ei_data() to make the data object.")
  }

  NM_helper <- function(data){
    NM_result = list(NM_1 = NA, NM_0 = NA, NM_disparity = NA)

    if(sum(is.na(data$wt)) == length(data$wt)){ # no weights
      NM_result$NM_1 = sum(data$Yn*data$Xn)/sum(data$Xn)
      NM_result$NM_0 = sum(data$Yn*(1-data$Xn))/sum(1-data$Xn)
    } else{ # weights are present
      NM_result$NM_1 = sum(data$Yn*data$Xn*data$wt)/sum(data$Xn*data$wt)
      NM_result$NM_0 = sum(data$Yn*(1-data$Xn)*data$wt)/sum((1-data$Xn)*data$wt)
    }
    NM_result$NM_disparity = NM_result$NM_1-NM_result$NM_0
    return(NM_result)
  }

  NM_result <- NM_helper(data)

  NM_1_vec = rep(0, n_bootstrap_replicates)
  NM_0_vec = rep(0, n_bootstrap_replicates)
  NM_disparity_vec = rep(0, n_bootstrap_replicates)

  if(n_bootstrap_replicates > 0){
    for(i in 1:n_bootstrap_replicates){
      bootstrap_data_rows = sample(1:length(data$Xn), length(data$Xn), replace = TRUE)
      bootstrap_data <- data
      bootstrap_data$Xn = bootstrap_data$Xn[bootstrap_data_rows]
      bootstrap_data$Yn = bootstrap_data$Yn[bootstrap_data_rows]
      bootstrap_data$wt = bootstrap_data$wt[bootstrap_data_rows]

      bootstrap_ER <- NM_helper(bootstrap_data)
      NM_1_vec[i] <- bootstrap_ER$NM_1
      NM_0_vec[i] <- bootstrap_ER$NM_0
      NM_disparity_vec[i] <- bootstrap_ER$NM_disparity
    }

    NM_result$NM_1_conf = c(unname(stats::quantile(NM_1_vec, .05)),
                            unname(stats::quantile(NM_1_vec, .95)))

    NM_result$NM_0_conf = c(unname(stats::quantile(NM_0_vec, .05)),
                            unname(stats::quantile(NM_0_vec, .95)))

    NM_result$NM_disparity_conf = c(unname(stats::quantile(NM_disparity_vec, .05)),
                                    unname(stats::quantile(NM_disparity_vec, .95)))
  }

  return(NM_result)

}
