#### Data ####
# Valid data
c(lunge_sensors, lunge_events) %<-% load_lunges()
test_deployids <- deployments(lunge_sensors)[1:3]
c(sensors_test, sensors_train) %<-% divide(lunge_sensors, test_deployids)
c(events_test, events_train) %<-% divide(lunge_events, test_deployids)
deployid <- deployments(sensors_test)[1]

# Invalid data
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

#### Test sb_plot_data() ####

test_that("sb_plot_data produces a plotly object", {
  expect_s3_class(sb_plot_data(deployid, sensors_test, events_test), "plotly")
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

#### Generate predictions ####

tsc <- compose_tsc(module = "interval_based",
                   algorithm = "SupervisedTimeSeriesForest",
                   params = list(n_estimators = 2L, random_state = 4321L),
                   columns = columns(lunge_sensors))
sb <- Stickleback(tsc,
                  win_size = 20,
                  tol = 5,
                  nth = 19,
                  n_folds = 2,
                  seed = 1234)
sb_fit(sb, sensors_train, events_train)
predictions <- sb_predict(sb, sensors_test)
outcomes <- sb_assess(sb, predictions, events_test)

#### Test sb_plot_predictions() ####

test_that("sb_plot_predictions() produces a plotly object", {
  expect_s3_class(
    sb_plot_predictions(
      deployid,
      sensors_test,
      predictions,
      outcomes
    ),
    "plotly"
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


