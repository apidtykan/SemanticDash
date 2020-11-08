library(testthat)
ship <- fread("ships.csv")
ship[, DATETIME := as.POSIXct(DATETIME, "%Y-%m-%d %H:%M:%S")]

source("R/Fun.R")

test_that("str_length is number of characters", {
  
  # Test on NULL
  dat <- distnM(x = ship, time = "DATETIME", shipT = "Unspecified" , 
                shipN = "261535003")
  expect_is(dat, "NULL")
  
  # Test on date and numeric coordinats
  dat <- distnM(x = ship, time = "DATETIME", shipT = "Cargo", 
                shipN = "ADASTRA")
  expect_equal(length(dat), 6)
  expect_equal(nrow(dat), 1)
  expect_is(dat$LON2, "numeric")
  expect_is(dat$LON, "numeric")
  
  dat <- distnM(x = ship, time = "DATETIME", shipT = "Cargo", 
                shipN = "CHAROITE", up_units = 30, ret = "first")
  expect_equal(round(dat$dist),  607)
  expect_is(dat$LON, "numeric")
  expect_is(dat$LAT, "numeric")
  
  dat <- distnM(x = ship, time = "DATETIME", shipT = "Cargo", 
                shipN = "CHAROITE", up_units = 120, ret = "first")
  expect_equal(round(dat$dist), 1212)
  expect_is(dat$LON, "numeric")
  expect_is(dat$LAT, "numeric")
  
})
