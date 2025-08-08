#' Plot monotone ecological inference, neighborhood model, ecological
#' regression, and method of bounds by group and for the disparity.
#'
#' @param data An analyzed EI data object from analyze_ei_data()

#'
#' @export
#'

plot <- function(data){
  # potential check for if it is an EI data frame from analyze_ei_data

  monotone_EI_res <- data$`Monotone EI`
  MOB_res <- data$MOB
  ER_res <- data$ER
  NM_res <- data$NM

  group_1_data <- data.frame(type = c("Monotone EI", "MOB", "ER", "NM"),
                             vals_lower = c(stats::quantile(monotone_EI_res$monotone_1_conf[[1]], .05),
                                            unname(stats::quantile(MOB_res$MOB_1_conf[[1]], .05)),
                                            unname(stats::quantile(ER_res$ER_1_conf, .025)),
                                            unname(stats::quantile(NM_res$NM_1_conf, .025))),
                             vals_upper = c(stats::quantile(monotone_EI_res$monotone_1_conf[[2]], .95),
                                            unname(stats::quantile(MOB_res$MOB_1_conf[[2]], .95)),
                                            unname(stats::quantile(ER_res$ER_1_conf, .975)),
                                            unname(stats::quantile(NM_res$NM_1_conf, .975))))
  group_0_data <- data.frame(type = c("Monotone EI", "MOB", "ER", "NM"),
                             vals_lower = c(stats::quantile(monotone_EI_res$monotone_0_conf[[1]], .05),
                                            unname(stats::quantile(MOB_res$MOB_0_conf[[1]], .05)),
                                            unname(stats::quantile(ER_res$ER_0_conf, .025)),
                                            unname(stats::quantile(NM_res$NM_0_conf, .025))),
                             vals_upper = c(stats::quantile(monotone_EI_res$monotone_0_conf[[2]], .95),
                                            unname(stats::quantile(MOB_res$MOB_0_conf[[2]], .95)),
                                            unname(stats::quantile(ER_res$ER_0_conf, .975)),
                                            unname(stats::quantile(NM_res$NM_0_conf, .975))))
  disparity_data <- data.frame(type = c("Monotone EI", "MOB", "ER", "NM"),
                               vals_lower = c(stats::quantile(monotone_EI_res$monotone_disparity_conf[[1]], .05),
                                              unname(stats::quantile(MOB_res$MOB_disparity_conf[[1]], .05)),
                                              unname(stats::quantile(ER_res$ER_disparity_conf, .025)),
                                              unname(stats::quantile(NM_res$NM_disparity_conf, .025))),
                               vals_upper = c(stats::quantile(monotone_EI_res$monotone_disparity_conf[[2]], .95),
                                              unname(stats::quantile(MOB_res$MOB_disparity_conf[[2]], .95)),
                                              unname(stats::quantile(ER_res$ER_disparity_conf, .975)),
                                              unname(stats::quantile(NM_res$NM_disparity_conf, .975))))
  make_plot <- function(data, Y){
    data$type <- factor(data$type, levels = c("MOB", "NM", "ER", "Monotone EI"), ordered = TRUE)
    plot <- ggplot(data, aes(x = type)) +
      geom_linerange(data = data, aes(ymin = vals_lower, ymax = vals_upper))+
      geom_point(aes(y = vals_lower))+
      geom_point(aes(y = vals_upper))+
      theme_classic()+
      ylab(Y)+
      xlab("")+
      coord_flip()
    return (plot)
  }

  group_1_plot <- make_plot(group_1_data, "Group 1")
  group_0_plot <- make_plot(group_0_data, "Group 0")
  disparity_plot <- make_plot(disparity_data, "Disparity")

  if(length(NM_res) > 3){ # only > 3 if we have bootstraps

    group_1_data$conf_lower = c(unname(stats::quantile(monotone_EI_res$monotone_1_conf[[1]], .05)),
                                              unname(stats::quantile(MOB_res$MOB_1_conf[[1]], .05)),
                                              unname(stats::quantile(ER_res$ER_1_conf, .025)),
                                              unname(stats::quantile(NM_res$NM_1_conf, .025)))
    group_1_data$conf_upper = c(unname(stats::quantile(monotone_EI_res$monotone_1_conf[[2]], .95)),
                                              unname(stats::quantile(MOB_res$MOB_1_conf[[2]], .95)),
                                              unname(stats::quantile(ER_res$ER_1_conf, .975)),
                                              unname(stats::quantile(NM_res$NM_1_conf, .975)))
    group_0_data$conf_lower = c(unname(stats::quantile(monotone_EI_res$monotone_0_conf[[1]], .05)),
                                              unname(stats::quantile(MOB_res$MOB_0_conf[[1]], .05)),
                                              unname(stats::quantile(ER_res$ER_0_conf, .025)),
                                              unname(stats::quantile(NM_res$NM_0_conf, .025)))
    group_1_data$conf_upper = c(unname(stats::quantile(monotone_EI_res$monotone_0_conf[[2]], .95)),
                                              unname(stats::quantile(MOB_res$MOB_0_conf[[2]], .95)),
                                              unname(stats::quantile(ER_res$ER_0_conf, .975)),
                                              unname(stats::quantile(NM_res$NM_0_conf, .975)))
    disparity_data$conf_lower = c(unname(stats::quantile(monotone_EI_res$monotone_disparity_conf[[1]], .05)),
                                                unname(stats::quantile(MOB_res$MOB_disparity_conf[[1]], .05)),
                                                unname(stats::quantile(ER_res$ER_disparity_conf, .025)),
                                                unname(stats::quantile(NM_res$NM_disparity_conf, .025)))
    disparity_data$conf_upper = c(unname(stats::quantile(monotone_EI_res$monotone_disparity_conf[[2]], .95)),
                                                unname(stats::quantile(MOB_res$MOB_disparity_conf[[2]], .95)),
                                                unname(stats::quantile(ER_res$ER_disparity_conf, .975)),
                                                unname(stats::quantile(NM_res$NM_disparity_conf, .975)))

    group_1_data$conf_lower <- c(monotone_EI_res$monotone_1_conf[1], MOB_res$MOB_1_conf[1], ER_res$ER_1_conf[1], NM_res$NM_1_conf[1])
    group_1_data$conf_upper <- c(monotone_EI_res$monotone_1_conf[2], MOB_res$MOB_1_conf[2], ER_res$ER_1_conf[2], NM_res$NM_1_conf[2])

    group_0_data$conf_lower <- c(monotone_EI_res$monotone_0_conf[1], MOB_res$MOB_0_conf[1], ER_res$ER_0_conf[1], NM_res$NM_0_conf[1])
    group_0_data$conf_upper <- c(monotone_EI_res$monotone_0_conf[2], MOB_res$MOB_0_conf[2], ER_res$ER_0_conf[2], NM_res$NM_0_conf[2])

    disparity_data$conf_lower <- c(monotone_EI_res$monotone_disparity_conf[1], MOB_res$MOB_disparity_conf[1], ER_res$ER_disparity_conf[1], NM_res$NM_disparity_conf[1])
    disparity_data$conf_upper <- c(monotone_EI_res$monotone_disparity_conf[2], MOB_res$MOB_disparity_conf[2], ER_res$ER_disparity_conf[2], NM_res$NM_disparity_conf[2])

    group_1_plot <- group_1_plot +
      geom_linerange(data = group_1_data, aes(ymin = conf_lower, ymax = conf_upper))
    group_0_plot <- group_0_plot +
      geom_linerange(data = group_0_data, aes(ymin = conf_lower, ymax = conf_upper))
    disparity_plot <- disparity_plot +
      geom_linerange(data = disparity_data, aes(ymin = conf_lower, ymax = conf_upper))
  }


  return(ggarrange(disparity_plot, group_1_plot, group_0_plot, nrow = 3))

}
