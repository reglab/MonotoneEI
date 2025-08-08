#' Evaluate NM, ER, MOB, and monotone EI bounds (with optional confidence intervals)
#' to be summarized in plot() or summarize()
#'
#' @param data An EI data object, created with make_ei_data()
#' @param n_bootstrap_replicates The optional number of bootstrap replicates to obtain 95\% confidence intervals
#'
#' @return A list of all methods and their estimates and bootstrap draws.
#' @export
#'


analyze_ei_data <- function(data, n_bootstrap_replicates = 0) {
  if(!is_ei_data(data)){
    stop("Error: This function takes only an EI data object. Please use make_ei_data() to make the data object.")
  }

  monotone_EI_res <- monotone_EI(data, n_bootstrap_replicates)
  MOB_res <- MOB(data, n_bootstrap_replicates)
  ER_res <- ER(data, n_bootstrap_replicates)
  NM_res <- NM(data, n_bootstrap_replicates)

  list_res <- list("Monotone EI" = monotone_EI_res, "MOB" = MOB_res, "ER" = ER_res, "NM" = NM_res)

  return(list_res)
}
