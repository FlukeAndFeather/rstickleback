df <- data.frame(
  the_tag = c(rep("A", 3), rep("B", 2)),
  the_time = as.POSIXct("1970-01-01", tz = "UTC") + 0:4
)
ev <- NULL

test_that("no errors on valid input", {
  expect_error(ev <<- Events(df, "the_tag", "the_time"), NA)
})

test_that("event conversion preserves input", {
  ev_df <- as.data.frame(ev)
  expect_equal(df$the_tag, ev_df$deployid, ignore_attr = TRUE)
  expect_equal(df$the_time, ev_df$datetime, ignore_attr = TRUE)
})

test_that("Events() catches bad input", {
  good_data <- data.frame(deployid = c("A", "B"),
                          datetime = as.POSIXct("1970-01-01", tz = "UTC") + 0:1)
  bad_data <- rnorm(5)

  good_deployid_col <- "deployid"
  bad_deployid_col <- "foo"

  good_datetime_col <- "datetime"
  bad_datetime_col <- "bar"

  expect_error(Events(bad_data, good_deployid_col, good_datetime_col),
               "inherits(event_data, \"data.frame\") is not TRUE",
               fixed = TRUE)
  expect_error(Events(good_data, bad_deployid_col, good_datetime_col),
               "deployid_col %in% colnames(event_data) is not TRUE",
               fixed = TRUE)
  expect_error(Events(good_data, good_deployid_col, bad_datetime_col),
               "datetime_col %in% colnames(event_data) is not TRUE",
               fixed = TRUE)
})

test_that("show() displays events properly", {
  expect_snapshot_output(ev)
})

test_that("accessor works properly", {
  expect_equal(deployments(ev), unique(df$the_tag))
})

test_that("splitting returns two mutually exclusive events", {
  c(ev1, ev2) %<-% split(ev, "A")
  expect_equal(deployments(ev1), "A")
  expect_equal(deployments(ev2), setdiff(unique(df$the_tag), "A"))
})
