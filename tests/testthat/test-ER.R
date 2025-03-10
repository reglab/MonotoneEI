test_that("ER works for test dataset", {
  ER_lm = lm(voter_vax$prop_covid_vax ~ voter_vax$prop_republican, weights = voter_vax$total_votes)

  expect_equal(ER(make_ei_data(voter_vax))$ER_1, unname(ER_lm$coefficients[2]) + unname(ER_lm$coefficients[1]),tolerance=0.00001)
  expect_equal(ER(make_ei_data(voter_vax))$ER_0, unname(ER_lm$coefficients[1]),tolerance=0.00001)
  expect_equal(ER(make_ei_data(voter_vax))$ER_disparity, unname(ER_lm$coefficients[2]),tolerance=0.00001)
})

test_that("ER throws error when used incorrectly", {
  expect_error(ER(voter_vax))
})
