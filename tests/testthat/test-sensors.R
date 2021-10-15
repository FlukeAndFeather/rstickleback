sensor_df <- data.frame(
  id = rep(c("A", "B", "C"), each = 4),
  dt = rep(as.POSIXct("1970-01-01", tz = "UTC") + 0:3, 3),
  x = seq(0, 100, length.out = 12)
)
sensors <- NULL

test_that("Sensors() throws no errors for valid input", {
  expect_error(sensors <<- Sensors(sensor_df, "id", "dt", "x"), NA)
})

test_that("Sensors conversion preserves input", {
  sensor_df2 <- as.data.frame(sensors)
  sensor_df_copy <- sensor_df %>%
    dplyr::rename(deployid = id,
                  datetime = dt)
  expect_equal(sensor_df2, sensor_df_copy)
})

test_that("Sensors() catches bad input", {
  bad_data <- rnorm(5)

  good_id_col <- "id"
  bad_id_col <- "foo"

  good_dt_col <- "dt"
  bad_dt_col <- "bar"

  good_sensor_cols <- "x"
  bad_sensor_cols <- "bowie"

  expect_error(Sensors(bad_data, good_id_col, good_dt_col, good_sensor_cols),
               "inherits(sensor_data, \"data.frame\") is not TRUE",
               fixed = TRUE)
  expect_error(Sensors(sensor_df, bad_id_col, good_dt_col, good_sensor_cols),
               "deployid_col %in% colnames(sensor_data) is not TRUE",
               fixed = TRUE)
  expect_error(Sensors(sensor_df, good_id_col, bad_dt_col, good_sensor_cols),
               "datetime_col %in% colnames(sensor_data) is not TRUE",
               fixed = TRUE)
  expect_error(Sensors(sensor_df, good_id_col, good_dt_col, bad_sensor_cols),
               "all(sensor_cols %in% colnames(sensor_data)) is not TRUE",
               fixed = TRUE)
})

test_that("show() displays events properly", {
  withr::with_options(list(max.print = 50), expect_snapshot_output(sensors))
})

test_that("accessors works properly", {
  expect_equal(deployments(sensors), unique(sensor_df$id))
  expect_equal(columns(sensors), "x")
})

test_that("dividing returns two mutually exclusive Sensors", {
  c(sensors1, sensors2) %<-% divide(sensors, "A")
  expect_equal(deployments(sensors1), "A")
  expect_equal(deployments(sensors2), setdiff(unique(sensor_df$id), "A"))
})


