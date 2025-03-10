test_that("Returns TRUE for real EI data objects", {
  expect_equal(TRUE, is_ei_data(make_ei_data(Xn = voter_vax$prop_republican,
                                             Yn = voter_vax$prop_covid_vax)))
  expect_equal(TRUE, is_ei_data(make_ei_data(Xn = voter_vax$prop_republican,
                                             Yn = voter_vax$prop_covid_vax,
                                             wt = voter_vax$total_votes)))
})

test_that("Returns FALSE for real EI data objects", {
  expect_equal(FALSE, is_ei_data(voter_vax))
  expect_equal(FALSE, is_ei_data(list(Xn = "a", Yn = "b", wt = "c")))
})

