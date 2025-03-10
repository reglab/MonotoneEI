#' Calculate ecological regression estimate
#'
#' @param data An EI data object
#' @param n_bootstrap_replicates The optional number of bootstrap replicates to obtain 95\% confidence intervals
#'
#' @return A list of: (ER_1: Ecological regression estimate for group 1,
#'                     ER_0: Ecological regression estimate for group 0,
#'                     ER_disparity: Ecological regression disparity estimate,
#'                     ER_1_conf: 90\% Confidence interval around group 1,
#'                     ER_0_conf: 90\% Confidence interval around group 0,
#'                     ER_disparity_conf: 90\% Confidence interval around disparity)
#' @export
#'

ER <- function(data, n_bootstrap_replicates = 0){
  if(!is_ei_data(data)){
    stop("Error: This function takes only an EI data object. Please use make_ei_data() to make the data object.")
  }

  ER_helper <- function(data){
    ER_result = list(ER_1 = NA, ER_0 = NA, ER_disparity = NA)

    if(sum(is.na(data$wt)) == length(data$wt)){ # no weights
      mod <- stats::lm(data$Yn ~ data$Xn)
    } else{ # weights are present
      mod <- stats::lm(data$Yn ~ data$Xn, weights = data$wt)
    }

    ER_result$ER_0 <- unname(mod$coefficients[1])
    ER_result$ER_1 <- unname(mod$coefficients[2])+unname(mod$coefficients[1])
    ER_result$ER_disparity <- unname(mod$coefficients[2])

    return(ER_result)
  }

  ER_result <- ER_helper(data)

  ER_1_vec = rep(0, n_bootstrap_replicates)
  ER_0_vec = rep(0, n_bootstrap_replicates)
  ER_disparity_vec = rep(0, n_bootstrap_replicates)

  if(n_bootstrap_replicates > 0){
    for(i in 1:n_bootstrap_replicates){
      bootstrap_data_rows = sample(1:length(data$Xn), length(data$Xn), replace = TRUE)
      bootstrap_data <- data
      bootstrap_data$Xn = bootstrap_data$Xn[bootstrap_data_rows]
      bootstrap_data$Yn = bootstrap_data$Yn[bootstrap_data_rows]
      bootstrap_data$wt = bootstrap_data$wt[bootstrap_data_rows]

      bootstrap_ER <- ER_helper(bootstrap_data)
      ER_1_vec[i] <- bootstrap_ER$ER_1
      ER_0_vec[i] <- bootstrap_ER$ER_0
      ER_disparity_vec[i] <- bootstrap_ER$ER_disparity
    }

    ER_result$ER_1_conf = c(unname(stats::quantile(ER_1_vec, .05)),
                              unname(stats::quantile(ER_1_vec, .95)))

    ER_result$ER_0_conf = c(unname(stats::quantile(ER_0_vec, .05)),
                              unname(stats::quantile(ER_0_vec, .95)))

    ER_result$ER_disparity_conf = c(unname(stats::quantile(ER_disparity_vec, .05)),
                                      unname(stats::quantile(ER_disparity_vec, .95)))
  }

  return(ER_result)
}

