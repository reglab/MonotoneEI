#' Summarize monotone ecological inference, neighborhood model, ecological
#' regression, and method of bounds by group and for the disparity.
#'
#' @param data An analyzed EI data object from analyze_ei_data()
#'
#'
#' @export
#'

summarize <- function(data){
  # potential check for if it is an EI data frame from analyze_ei_data

  monotone_EI_res <- data$`Monotone EI`
  MOB_res <- data$MOB
  ER_res <- data$ER
  NM_res <- data$NM

  if(length(NM_res) <= 3){ # only > 3 if we have bootstraps
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
                      "\t (", round(unname(stats::quantile(monotone_EI_res$monotone_1_conf[[1]], .05)), 4) , ", ",
                      round(unname(stats::quantile(monotone_EI_res$monotone_1_conf[[2]], .95)), 4), ")\n",
                      "  NM: ", round(NM_res$NM_1,4), "\n",
                      "\t (", round(unname(stats::quantile(NM_res$NM_1_conf, .025)), 4) , ", ",
                      round(unname(stats::quantile(NM_res$NM_1_conf, .975)), 4), ")\n",
                      "  ER: ", round(ER_res$ER_1,4), "\n",
                      "\t (", round(unname(stats::quantile(ER_res$ER_1_conf, .025)), 4), ", ",
                      round(unname(stats::quantile(ER_res$ER_1_conf, .975)), 4), ")\n",

                      "  MOB: [", round(MOB_res$MOB_1[1], 4), ", ", round(MOB_res$MOB_1[2], 4), "]\n",
                      "\t (", round(unname(stats::quantile(MOB_res$MOB_1_conf[[1]], .05)), 4) , ", ",
                      round(unname(stats::quantile(MOB_res$MOB_1_conf[[2]], .95)), 4), ")\n\n")

    group_0 <- paste0(" Group 1: [", round(monotone_EI_res$monotone_0[1], 4), ", ", round(monotone_EI_res$monotone_0[2], 4), "]\n",
                      "\t (", round(unname(stats::quantile(monotone_EI_res$monotone_0_conf[[1]], .05)), 4) , ", ",
                      round(unname(stats::quantile(monotone_EI_res$monotone_0_conf[[2]], .95)), 4), ")\n",
                      "  NM: ", round(NM_res$NM_0,4), "\n",
                      "\t (", round(unname(stats::quantile(NM_res$NM_0_conf, .025)), 4) , ", ",
                      round(unname(stats::quantile(NM_res$NM_0_conf, .975)), 4), ")\n",
                      "  ER: ", round(ER_res$ER_0,4), "\n",
                      "\t (", round(unname(stats::quantile(ER_res$ER_0_conf, .025)), 4), ", ",
                      round(unname(stats::quantile(ER_res$ER_0_conf, .975)), 4), ")\n",

                      "  MOB: [", round(MOB_res$MOB_0[1], 4), ", ", round(MOB_res$MOB_0[2], 4), "]\n",
                      "\t (", round(unname(stats::quantile(MOB_res$MOB_0_conf[[1]], .05)), 4) , ", ",
                      round(unname(stats::quantile(MOB_res$MOB_0_conf[[2]], .95)), 4), ")\n\n")

    disparity <- paste0(" Dispairty: [", round(monotone_EI_res$monotone_disparity[1], 4), ", ", round(monotone_EI_res$monotone_disparity[2], 4), "]\n",
                      "\t (", round(unname(stats::quantile(monotone_EI_res$monotone_disparity_conf[[1]], .05)), 4) , ", ",
                      round(unname(stats::quantile(monotone_EI_res$monotone_disparity_conf[[2]], .95)), 4), ")\n",
                      "  NM: ", round(NM_res$NM_disparity,4), "\n",
                      "\t (", round(unname(stats::quantile(NM_res$NM_disparity_conf, .025)), 4) , ", ",
                      round(unname(stats::quantile(NM_res$NM_disparity_conf, .975)), 4), ")\n",
                      "  ER: ", round(ER_res$ER_disparity,4), "\n",
                      "\t (", round(unname(stats::quantile(ER_res$ER_disparity_conf, .025)), 4), ", ",
                      round(unname(stats::quantile(ER_res$ER_disparity_conf, .975)), 4), ")\n",

                      "  MOB: [", round(MOB_res$MOB_disparity[1], 4), ", ", round(MOB_res$MOB_disparity[2], 4), "]\n",
                      "\t (", round(unname(stats::quantile(MOB_res$MOB_disparity_conf[[1]], .05)), 4) , ", ",
                      round(unname(stats::quantile(MOB_res$MOB_disparity_conf[[2]], .95)), 4), ")\n\n")

    cat(group_1, group_0, disparity)
  }
}
