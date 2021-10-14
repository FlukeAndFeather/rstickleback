lunges <- NULL

test_that("no errors", {
  expect_error(lunges <<- load_lunges(), NA)
})

test_that("data contains Sensors and Events", {
  expect_equal(length(lunges), 2)
  expect_named(lunges, c("sensors", "events"))
  expect_s4_class(lunges$sensors, "Sensors")
  expect_s4_class(lunges$events, "Events")
})

test_that("sensor snapshot unchanged", {
  sensors_df <- as.data.frame(lunges$sensors)
  withr::with_options(list(max.print = 50), {
    expect_snapshot(lunges$sensors)
    expect_snapshot(sensors_df)
  })
})

test_that("event snapshot unchanged", {
  events_df <- as.data.frame(lunges$events)
  withr::with_options(list(max.print = 50), {
    expect_snapshot(lunges$events)
    expect_snapshot(events_df)
  })
})
