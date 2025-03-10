#' Plot monotone ecological inference, neighborhood model, ecological
#' regression, and method of bounds by group and for the disparity.
#'
#' @param data An EI data object
#' @param n_bootstrap_replicates The optional number of bootstrap replicates to obtain 95\% confidence intervals
#'
#'
#' @export
#'

plot <- function(data, n_bootstrap_replicates = 0){
  if(!is_ei_data(data)){
    stop("Error: This function takes only an EI data object. Please use make_ei_data() to make the data object.")
  }

  monotone_EI_res <- monotone_EI(data, n_bootstrap_replicates)
  MOB_res <- MOB(data, n_bootstrap_replicates)
  ER_res <- ER(data, n_bootstrap_replicates)
  NM_res <- NM(data, n_bootstrap_replicates)

  group_1_data <- data.frame(type = c("Monotone EI", "MOB", "ER", "NM"),
                             vals_lower = c(monotone_EI_res$monotone_1[1], MOB_res$MOB_1[1], ER_res$ER_1, NM_res$NM_1),
                             vals_upper = c(monotone_EI_res$monotone_1[2], MOB_res$MOB_1[2], ER_res$ER_1, NM_res$NM_1))
  group_0_data <- data.frame(type = c("Monotone EI", "MOB", "ER", "NM"),
                             vals_lower = c(monotone_EI_res$monotone_0[1], MOB_res$MOB_0[1], ER_res$ER_0, NM_res$NM_0),
                             vals_upper = c(monotone_EI_res$monotone_0[2], MOB_res$MOB_0[2], ER_res$ER_0, NM_res$NM_0))
  disparity_data <- data.frame(type = c("Monotone EI", "MOB", "ER", "NM"),
                             vals_lower = c(monotone_EI_res$monotone_disparity[1], MOB_res$MOB_disparity[1], ER_res$ER_disparity, NM_res$NM_disparity),
                             vals_upper = c(monotone_EI_res$monotone_disparity[2], MOB_res$MOB_disparity[2], ER_res$ER_disparity, NM_res$NM_disparity))

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

  if(n_bootstrap_replicates>0){
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
