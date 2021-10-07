
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `rstickleback`

<!-- badges: start -->
<!-- badges: end -->

> A machine learning pipeline for detecting fine-scale behavioral events
> in bio-logging data.

## Installation

You can install the development version of rstickleback from
[GitHub](https://github.com/FlukeAndFeather/rstickleback) with:

``` r
# install.packages("devtools")
devtools::install_github("FlukeAndFeather/rstickleback")
```

## Key concepts

-   Behavioral events are brief behaviors that can be represented as a
    point in time, e.g. feeding or social interactions.
-   High-resolution bio-logging data (e.g. from accelerometers and
    magnetometers) are multi-variate time series. Traditional
    classifiers struggle with time series data.
-   `stickleback` takes a time series classification approach to detect
    behavioral events in longitudinal bio-logging data.

## Quick start

### Load sample data

The included sensor data contains the depth, pitch, roll, and speed of
six blue whales at 10 Hz, and the event data contains the times of
lunge-feeding behaviors.

``` r
library(rstickleback)
#> 
#> Attaching package: 'rstickleback'
#> The following object is masked from 'package:base':
#> 
#>     split
# `load_lunges()` returns a list of sensors and events, so we use the multiple
# assignment operator (%<-%) to destruct the list into separate `lunge_sensors`
# and `lunge_events` objects.
c(lunge_sensors, lunge_events) %<-% load_lunges()

# Again we use %<-%, to split the sensors and events into test and train sets
test_deployids <- deployments(lunge_sensors)[1:3]
c(sensors_test, sensors_train) %<-% split(lunge_sensors, test_deployids)
c(events_test, events_train) %<-% split(lunge_events, test_deployids)
```

### Visualize sensor and event data

`sb_plot_data()` produces an interactive figure for exploring bio-logger
data.

``` r
deployid <- deployments(lunge_sensors)[1]
sb_plot_data(deployid, lunge_sensors, lunge_events)
```

![Animated loop of interactively exploring data with
plot\_sensors\_events()](inst/img/plot-sensors-events.gif)

### Define model

Initialize a `Stickleback` model using Supervised Time Series Forests
and a 5 s window.

``` r
tsc <- compose_tsc(module = "interval_based", 
                   algorithm = "SupervisedTimeSeriesForest",
                   params = list(n_estimators = 2L),
                   columns = columns(lunge_sensors))
sb <- Stickleback(tsc, win_size = 50, tol = 5, nth = 10, n_folds = 4)
```

### Fit model

Fit the `Stickleback` object to the training data.

``` r
sb_fit(sb, sensors_train, events_train)
```
