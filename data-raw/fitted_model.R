# Creates and serializes a pre-fitted Stickleback model.
# For use with load_fitted()
c(lunge_sensors, lunge_events) %<-% load_lunges()
test_deployids <- deployments(lunge_sensors)[1:3]
c(sensors_test, sensors_train) %<-% split(lunge_sensors, test_deployids)
c(events_test, events_train) %<-% split(lunge_events, test_deployids)

tsc <- compose_tsc(module = "interval_based",
                   algorithm = "SupervisedTimeSeriesForest",
                   params = list(n_estimators = 2L, random_state = 4321L),
                   columns = columns(lunge_sensors))
sb <- Stickleback(tsc,
                  win_size = 50,
                  tol = 5,
                  nth = 10,
                  n_folds = 4,
                  seed = 1234)

sb_fit(sb, sensors_train, events_train)
predictions <- sb_predict(sb, sensors_test)
outcomes <- sb_assess(sb, predictions, events_test)

save_py <- function(obj, pkl_name) {
  reticulate::py_save_object(obj, paste0("tests/testthat/testdata/", pkl_name))
}
save_py(sb@.stickleback, "prefitted.pkl")
save_py(sb@local_clf, "prefitted_tsc.pkl")
save_py(predictions@.data, "prefitted_pred.pkl")
save_py(outcomes@.data, "prefitted_outcome.pkl")
save_py(sensors_train@.data, "prefitted_trainX.pkl")
save_py(sensors_test@.data, "prefitted_testX.pkl")
save_py(events_train@.data, "prefitted_trainy.pkl")
save_py(events_test@.data, "prefitted_testy.pkl")

