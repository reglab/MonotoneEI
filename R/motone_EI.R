#' Calculate monotone ecological inference bounds based on the neighborhood model,
#' ecological regression, and the method of bounds.
#'
#' @param data An EI data object
#' @param n_bootstrap_replicates The optional number of bootstrap replicates to obtain 95\% confidence intervals
#'
#' @return A list of: (monotone_1: (lower bound for group 1, upper bound for group 1),
#'                     monotone_0: (lower bound for group 0, upper bound for group 0),
#'                     monotone_disparity: (lower bound for disparity, upper bound for disparity),
#'                     monotone_1_conf: (lower 90\% confidence interval bound for group 1, upper 90\% confidence interval bound for group 1),
#'                     monotone_0_conf: (lower 90\% confidence interval bound for group 0, upper 90\% confidence interval bound for group 0),
#'                     monotone_disparity_conf: (lower 90\% confidence interval bound for disparity, upper 90\% confidence interval bound for disparity))
#' @export
#'

monotone_EI <- function(data, n_bootstrap_replicates = 0){
  if(!is_ei_data(data)){
    stop("Error: This function takes only an EI data object. Please use make_ei_data() to make the data object.")
  }
  monotone_EI_result <- list(monotone_1 = NA,
                             monotone_0 = NA,
                             monotone_disparity = NA)

  NM_res = NM(data, n_bootstrap_replicates)
  ER_res = ER(data, n_bootstrap_replicates)
  MOB_res = MOB(data, n_bootstrap_replicates)

  # intersect NM and ER with MOB
  monotone_EI_result$monotone_1 = c(max(min(NM_res$NM_1, ER_res$ER_1), MOB_res$MOB_1[1]),
                                    min(max(NM_res$NM_1, ER_res$ER_1), MOB_res$MOB_1[2]))

  monotone_EI_result$monotone_0 = c(max(min(NM_res$NM_0, ER_res$ER_0), MOB_res$MOB_0[1]),
                                    min(max(NM_res$NM_0, ER_res$ER_0), MOB_res$MOB_0[2]))

  monotone_EI_result$monotone_disparity = c(max(min(NM_res$NM_disparity, ER_res$ER_disparity), MOB_res$MOB_disparity[1]),
                                            min(max(NM_res$NM_disparity, ER_res$ER_disparity), MOB_res$MOB_disparity[2]))



  if(n_bootstrap_replicates > 0){
    # group 1 min
    if (which.max(c(min(NM_res$NM_1, ER_res$ER_1), MOB_res$MOB_1[1])) == 1){
      if (which.min(c(NM_res$NM_1, ER_res$ER_1)) == 1){
        lower_grp_1 = NM_res$NM_1_conf[1]
      } else{
        lower_grp_1 = ER_res$ER_1_conf[1]
      }
    } else{
      lower_grp_1 = MOB_res$MOB_1_conf[1] # lower conf int of grp 1 from MOB
    }

    # group 1 max
    if (which.min(c(max(NM_res$NM_1, ER_res$ER_1), MOB_res$MOB_1[2])) == 1){
      if (which.max(c(NM_res$NM_1, ER_res$ER_1)) == 1){
        upper_grp_1 = NM_res$NM_1_conf[2]
      } else{
        upper_grp_1 = ER_res$ER_1_conf[2]
      }
    } else{
      upper_grp_1 = MOB_res$MOB_1_conf[2] # upper conf int of grp 1 from MOB
    }

    # group 0 min
    if (which.max(c(min(NM_res$NM_0, ER_res$ER_0), MOB_res$MOB_0[1])) == 1){
      if (which.min(c(NM_res$NM_0, ER_res$ER_0)) == 1){
        lower_grp_0 = NM_res$NM_0_conf[1]
      } else{
        lower_grp_0 = ER_res$ER_0_conf[1]
      }
    } else{
      lower_grp_0 = MOB_res$MOB_0_conf[1] # lower conf int of grp 0 from MOB
    }

    # group 0 max
    if (which.min(c(max(NM_res$NM_0, ER_res$ER_0), MOB_res$MOB_0[2])) == 1){
      if (which.max(c(NM_res$NM_0, ER_res$ER_0)) == 1){
        upper_grp_0 = NM_res$NM_0_conf[2]
      } else{
        upper_grp_0 = ER_res$ER_0_conf[2]
      }
    } else{
      upper_grp_0 = MOB_res$MOB_0_conf[2] # upper conf int of grp 0 from MOB
    }

    # group 1 min
    if (which.max(c(min(NM_res$NM_disparity, ER_res$ER_disparity), MOB_res$MOB_disparity[1])) == 1){
      if (which.min(c(NM_res$NM_disparity, ER_res$ER_disparity)) == 1){
        lower_grp_disparity = NM_res$NM_disparity_conf[1]
      } else{
        lower_grp_disparity = ER_res$ER_disparity_conf[1]
      }
    } else{
      lower_grp_disparity = MOB_res$MOB_disparity_conf[1] # lower conf int of grp 1 from MOB
    }

    # group 1 max
    if (which.min(c(max(NM_res$NM_disparity, ER_res$ER_disparity), MOB_res$MOB_disparity[2])) == 1){
      if (which.max(c(NM_res$NM_disparity, ER_res$ER_disparity)) == 1){
        upper_grp_disparity = NM_res$NM_disparity_conf[2]
      } else{
        upper_grp_disparity = ER_res$ER_disparity_conf[2]
      }
    } else{
      upper_grp_disparity = MOB_res$MOB_disparity_conf[2] # upper conf int of grp 1 from MOB
    }

    monotone_EI_result$monotone_1_conf = c(lower_grp_1, upper_grp_1)
    monotone_EI_result$monotone_0_conf = c(lower_grp_0, upper_grp_0)
    monotone_EI_result$monotone_disparity_conf = c(lower_grp_disparity, upper_grp_disparity)
  }
  return(monotone_EI_result)
}
