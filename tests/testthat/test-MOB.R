test_that("MOB works for test dataset", {
  MOB_result = list(MOB_1 = NA,
                    MOB_0 = NA,
                    MOB_disparity = NA)

  max_X0 = pmin((1-voter_vax$prop_republican), voter_vax$prop_covid_vax)
  min_X1 = pmax(voter_vax$prop_covid_vax-max_X0, 0)
  max_X1 = pmin(voter_vax$prop_republican, voter_vax$prop_covid_vax)
  min_X0 = pmax(voter_vax$prop_covid_vax-max_X1, 0)

  MOB_result$MOB_1 = c(sum(min_X1)/sum(voter_vax$prop_republican), sum(max_X1)/sum(voter_vax$prop_republican))
  MOB_result$MOB_0 = c(sum(min_X0)/sum(1-voter_vax$prop_republican), sum(max_X0)/sum(1-voter_vax$prop_republican))
  MOB_result$MOB_disparity = c(sum(min_X1)/sum(voter_vax$prop_republican)-sum(max_X0)/sum(1-voter_vax$prop_republican),
                               sum(max_X1)/sum(voter_vax$prop_republican)-sum(min_X0)/sum(1-voter_vax$prop_republican))

  expect_equal(MOB(make_ei_data(voter_vax)), MOB_result)
})

test_that("MOB throws error when used incorrectly", {
  expect_error(MOB(voter_vax))
})
