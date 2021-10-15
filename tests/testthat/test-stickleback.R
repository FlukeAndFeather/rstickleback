c(lunge_sensors, lunge_events) %<-% load_lunges()

test_deployids <- deployments(lunge_sensors)[1:3]
c(sensors_test, sensors_train) %<-% divide(lunge_sensors, test_deployids)
c(events_test, events_train) %<-% divide(lunge_events, test_deployids)

tsc <- NULL
sb <- NULL
predictions <- NULL
outcomes <- NULL

test_that("TSC constructor throws no errors on valid input", {
  expect_error(
    tsc <<- compose_tsc(module = "interval_based",
                        algorithm = "SupervisedTimeSeriesForest",
                        params = list(n_estimators = 2L, random_state = 4321L),
                        columns = columns(lunge_sensors)),
    NA
  )
})

test_that("TSC is composed of correct estimators", {
  validate_estimator <- function(est, col) {
    expect_equal(est[[1]], paste0("est_", col))
    expect_s3_class(est[[2]], "sktime.classification.interval_based._stsf.SupervisedTimeSeriesForest")
    expect_equal(est[[2]]$n_estimators, 2)
    expect_equal(est[[2]]$random_state, 4321)
  }

  purrr::walk2(tsc$estimators, columns(lunge_sensors), validate_estimator)
})

test_that("constructor throws no errors on valid input", {
  expect_error(sb <<- Stickleback(tsc,
                                  win_size = 50,
                                  tol = 5,
                                  nth = 10,
                                  n_folds = 4,
                                  seed = 1234),
               NA)
})

test_that("fitting throws no errors", {
  expect_error(sb_fit(sb, sensors_train, events_train), NA)
})

test_that("prediction throws no errors", {
  expect_error(predictions <<- sb_predict(sb, sensors_test), NA)
})

test_that("assessment throws no errors", {
  expect_error(outcomes <<- sb_assess(sb, predictions, events_test), NA)
})

test_that("predictions and outcomes match snapshots", {
  withr::with_options(list(max.print = 50),
                      expect_snapshot_output(predictions))
  withr::with_options(list(max.print = 50),
                      expect_snapshot_output(outcomes))
  withr::with_options(list(max.print = 50),
                      expect_snapshot_output(as.data.frame(predictions)))
  withr::with_options(list(max.print = 50),
                      expect_snapshot_output(as.data.frame(outcomes)))
})
