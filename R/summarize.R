#' Summarize monotone ecological inference, neighborhood model, ecological
#' regression, and method of bounds by group and for the disparity.
#'
#' @param data An EI data object
#' @param n_bootstrap_replicates The optional number of bootstrap replicates to obtain 95\% confidence intervals
#'
#'
#' @export
#'

summarize <- function(data, n_bootstrap_replicates = 0){
  if(!is_ei_data(data)){
    stop("Error: This function takes only an EI data object. Please use make_ei_data() to make the data object.")
  }

  monotone_EI_res <- monotone_EI(data, n_bootstrap_replicates)
  MOB_res <- MOB(data, n_bootstrap_replicates)
  ER_res <- ER(data, n_bootstrap_replicates)
  NM_res <- NM(data, n_bootstrap_replicates)

  if(n_bootstrap_replicates == 0){
    group_1 <- paste0(" Group 1: [", round(monotone_EI_res$monotone_1[1], 4), ", ", round(monotone_EI_res$monotone_1[2], 4), "]\n",
                      "  NM: ", round(NM_res$NM_1,4), "\n",
                      "  ER: ", round(ER_res$ER_1,4), "\n",
                      "  MOB: [", round(MOB_res$MOB_1[1], 4), ", ", round(MOB_res$MOB_1[2], 4), "]\n\n")
    group_0 <- paste0("Group 0: [", round(monotone_EI_res$monotone_0[1], 4), ", ", round(monotone_EI_res$monotone_0[2], 4), "]\n",
                      "  NM: ", round(NM_res$NM_0,4), "\n",
                      "  ER: ", round(ER_res$ER_0,4), "\n",
                      "  MOB: [", round(MOB_res$MOB_0[1], 4), ", ", round(MOB_res$MOB_0[2], 4), "]\n\n")
    disparity <- paste0("Disparity: [", round(monotone_EI_res$monotone_disparity[1], 4), ", ", round(monotone_EI_res$monotone_disparity[2], 4), "]\n",
                        "  NM: ", round(NM_res$NM_disparity,4), "\n",
                        "  ER: ", round(ER_res$ER_disparity,4), "\n",
                        "  MOB: [", round(MOB_res$MOB_disparity[1], 4), ", ", round(MOB_res$MOB_disparity[2], 4), "]\n\n")
    cat(group_1, group_0, disparity)
  } else{
    group_1 <- paste0(" Group 1: [", round(monotone_EI_res$monotone_1[1], 4), ", ", round(monotone_EI_res$monotone_1[2], 4), "]\n",
                      "\t (", round(monotone_EI_res$monotone_1_conf[1], 4), ", ", round(monotone_EI_res$monotone_1_conf[2], 4), ")\n",

                      "  NM: ", round(NM_res$NM_1,4), "\n",
                      "\t (", round(NM_res$NM_1_conf[1], 4), ", ", round(NM_res$NM_1_conf[2], 4), ")\n",

                      "  ER: ", round(ER_res$ER_1,4), "\n",
                      "\t (", round(ER_res$ER_1_conf[1], 4), ", ", round(ER_res$ER_1_conf[2], 4), ")\n",

                      "  MOB: [", round(MOB_res$MOB_1[1], 4), ", ", round(MOB_res$MOB_1[2], 4), "]\n",
                      "\t (", round(MOB_res$MOB_1_conf[1], 4), ", ", round(MOB_res$MOB_1_conf[2], 4), ")\n\n")

    group_0 <- paste0(" Group 0: [", round(monotone_EI_res$monotone_0[1], 4), ", ", round(monotone_EI_res$monotone_0[2], 4), "]\n",
                      "\t (", round(monotone_EI_res$monotone_0_conf[1], 4), ", ", round(monotone_EI_res$monotone_0_conf[2], 4), ")\n",

                      "  NM: ", round(NM_res$NM_0,4), "\n",
                      "\t (", round(NM_res$NM_0_conf[1], 4), ", ", round(NM_res$NM_0_conf[2], 4), ")\n",

                      "  ER: ", round(ER_res$ER_0,4), "\n",
                      "\t (", round(ER_res$ER_0_conf[1], 4), ", ", round(ER_res$ER_0_conf[2], 4), ")\n",

                      "  MOB: [", round(MOB_res$MOB_0[1], 4), ", ", round(MOB_res$MOB_0[2], 4), "]\n",
                      "\t (", round(MOB_res$MOB_0_conf[1], 4), ", ", round(MOB_res$MOB_0_conf[2], 4), ")\n\n")

    disparity <- paste0(" Disparity: [", round(monotone_EI_res$monotone_disparity[1], 4), ", ", round(monotone_EI_res$monotone_disparity[2], 4), "]\n",
                      "\t (", round(monotone_EI_res$monotone_disparity_conf[1], 4), ", ", round(monotone_EI_res$monotone_disparity_conf[2], 4), ")\n",

                      "  NM: ", round(NM_res$NM_disparity,4), "\n",
                      "\t (", round(NM_res$NM_disparity_conf[1], 4), ", ", round(NM_res$NM_disparity_conf[2], 4), ")\n",

                      "  ER: ", round(ER_res$ER_disparity,4), "\n",
                      "\t (", round(ER_res$ER_disparity_conf[1], 4), ", ", round(ER_res$ER_disparity_conf[2], 4), ")\n",

                      "  MOB: [", round(MOB_res$MOB_disparity[1], 4), ", ", round(MOB_res$MOB_disparity[2], 4), "]\n",
                      "\t (", round(MOB_res$MOB_disparity_conf[1], 4), ", ", round(MOB_res$MOB_disparity_conf[2], 4), ")\n\n")

    cat(group_1, group_0, disparity)
  }
}
