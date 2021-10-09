c(stickleback,
  sensors_train,
  events_train,
  sensors_test,
  events_test,
  predictions,
  outcomes) %<-% load_fitted("testdata")

deployid <- deployments(sensors_test)[1]

wrong_sensors <- Sensors(
  sensor_data = data.frame(A = "A",
                           B = as.POSIXct("1970-01-01", tz = "UTC"),
                           C = 1,
                           D = 2),
  deployid_col = "A",
  datetime_col = "B",
  sensor_cols = c("C", "D")
)
wrong_events <- Events(
  event_data = data.frame(A = "A",
                          B = as.POSIXct("1970-01-01", tz = "UTC")),
  deployid_col = "A",
  datetime_col = "B"
)

test_that("sb_plot_data() snapshot is unchanged", {
  expect_snapshot(sb_plot_data(deployid, sensors_test, events_test))
})

test_that("sb_plot_data() fails on bad input", {
  expect_error(sb_plot_data(iris, sensors_test, events_test),
               "deployid %in% deployments(sensors) are not all TRUE",
               fixed = TRUE)
  expect_error(sb_plot_data(deployid, iris, events_test),
               "inherits(sensors, \"Sensors\") is not TRUE",
               fixed = TRUE)
  expect_error(sb_plot_data(deployid, sensors_test, iris),
               "inherits(events, \"Events\") is not TRUE",
               fixed = TRUE)

  expect_error(sb_plot_data(deployid, wrong_sensors, events_test),
               "deployid %in% deployments(sensors) is not TRUE",
               fixed = TRUE)
  expect_error(sb_plot_data(deployid, sensors_test, wrong_events),
               "deployid %in% deployments(events) is not TRUE",
               fixed = TRUE)
})

test_that("sb_plot_predictions() snapshot is unchanged", {
  expect_snapshot(
    sb_plot_predictions(
      deployid,
      sensors_test,
      predictions,
      outcomes
    )
  )
})

test_that("sb_plot_predictions() fails on bad input", {
  expect_error(sb_plot_predictions(iris, sensors_test, predictions, outcomes),
               "deployid %in% deployments(sensors) are not all TRUE",
               fixed = TRUE)
  expect_error(sb_plot_predictions(deployid, iris, predictions, outcomes),
               "inherits(sensors, \"Sensors\") is not TRUE",
               fixed = TRUE)
  expect_error(sb_plot_predictions(deployid, sensors_test, iris, outcomes),
               "inherits(predictions, \"Predictions\") is not TRUE",
               fixed = TRUE)
  expect_error(sb_plot_predictions(deployid, sensors_test, predictions, iris),
               "inherits(outcomes, \"Outcomes\") is not TRUE",
               fixed = TRUE)

  expect_error(
    sb_plot_predictions(deployid,
                        wrong_sensors,
                        predictions,
                        outcomes),
    "deployid %in% deployments(sensors) is not TRUE",
    fixed = TRUE
  )
})


