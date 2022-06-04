c(lunge_sensors, lunge_events) %<-% load_lunges()
test_deployids <- deployments(lunge_sensors)[1:4]
c(sensors_test, sensors_train) %<-% divide(lunge_sensors, test_deployids)
c(events_test, events_train) %<-% divide(lunge_events, test_deployids)

run_stickleback <- function() {
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
  list(predictions, outcomes)
}

# Run the define-fit-predict-assess pipeline twice
c(pred1, out1) %<-% run_stickleback()
c(pred2, out2) %<-% run_stickleback()

test_that("predictions are reproducible", {
  expect_equal(pred1@.data$`bw180905-53`[[1]]$values,
               pred2@.data$`bw180905-53`[[1]]$values)
  expect_equal(pred1@.data$`bw180905-53`[[2]]$values,
               pred2@.data$`bw180905-53`[[2]]$values)
})

test_that("outcomes are reproducible", {
  expect_equal(out1@.data$`bw180905-53`$values,
               out2@.data$`bw180905-53`$values)
})
