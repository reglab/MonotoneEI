

test_that("Passing an example data.frame works (with wt)", {
  expect_no_error(make_ei_data(voter_vax))
  ei_data = make_ei_data(voter_vax)
  expect_equal(voter_vax$prop_republican, ei_data$Xn)
  expect_equal(voter_vax$prop_covid_vax, ei_data$Yn)
  expect_equal(voter_vax$total_votes, ei_data$wt)
})

test_that("Passing an example non data.frame works (with wt)", {
  expect_no_error(make_ei_data(Xn = voter_vax$prop_republican,
                               Yn = voter_vax$prop_covid_vax,
                               wt = voter_vax$total_votes))
  ei_data = make_ei_data(Xn = voter_vax$prop_republican,
                         Yn = voter_vax$prop_covid_vax,
                         wt = voter_vax$total_votes)
  expect_equal(voter_vax$prop_republican, ei_data$Xn)
  expect_equal(voter_vax$prop_covid_vax, ei_data$Yn)
  expect_equal(voter_vax$total_votes, ei_data$wt)
})

test_that("Passing an example data.frame works (no wt)", {
  expect_no_error(make_ei_data(voter_vax[,c(1,2)]))
  ei_data = make_ei_data(voter_vax[,c(1,2)])

  expect_equal(voter_vax$prop_republican, ei_data$Xn)
  expect_equal(voter_vax$prop_covid_vax, ei_data$Yn)
  expect_equal(is.na(ei_data$wt), TRUE)
})

test_that("Passing an example non data.frame works (no wt)", {
  expect_no_error(make_ei_data(Xn = voter_vax$prop_republican,
                               Yn = voter_vax$prop_covid_vax))
  ei_data = make_ei_data(Xn = voter_vax$prop_republican,
                         Yn = voter_vax$prop_covid_vax)
  expect_equal(voter_vax$prop_republican, ei_data$Xn)
  expect_equal(voter_vax$prop_covid_vax, ei_data$Yn)
  expect_equal(is.na(ei_data$wt), TRUE)
})

test_that("Passing incorrectly formed requests throws errors",{
  # passing only one column
  expect_error(make_ei_data(Xn = voter_vax$prop_republican))
  expect_error(make_ei_data(voter_vax[,1]))

  # passing different length columns
  expect_error(make_ei_data(Xn = voter_vax$prop_republican,
                            Yn = head(voter_vax$prop_republican)))
  expect_error(make_ei_data(Xn = voter_vax$prop_republican,
                            Yn = voter_vax$prop_republican,
                            wt = head(voter_vax$prop_republican)))

  # passing nothing
  expect_error(make_ei_data())
})
