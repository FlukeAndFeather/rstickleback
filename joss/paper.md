---
title: 'rstickleback: supervised behavior detection in bio-logging data'
tags:
  - R
  - Python
  - ecology
  - behavior
  - bio-logging
  - machine learning
  - time series annotation
authors:
  - name: Max F. Czapanskiy
    orcid: 0000-0000-0000-0000
    affiliation: 1
  - name: Ariana Mann
    orcid: 
    affiliation: 2
affiliations:
 - name: Department of Biology, Stanford University, USA
   index: 1
 - name: Department of Electrical Engineering, Stanford University, USA
   index: 2
date: 10 June 202022
bibliography: references.bib
---

# Summary

The `rstickleback` **R** package provides a machine learning pipeline for detecting behavioral events, such as prey capture, in data collected with animal-borne sensors (e.g., accelerometers).

# Statement of need

Bio-logging is a research method that allows field biologists to remotely observe movement, behavior, physiology, and the environment using miniaturized electronics attached to animals [@rutz2009; @wilmers2015]. The high-resolution accelerometers, magnetometers, and other sensors used in modern bio-loggers capture behavior at ever finer scales, but new computational techniques are needed for processing, visualizing, and analyzing the large amount of data [@nathan2022; @williams2017; @cade2021]. For example, detecting behavioral events in bio-logging sensor data, such as feeding or social interactions, requires sifting through hours of high-resolution data - a laborious and potentially error-prone process. Existing methods for automating behavioral event detection typically rely on signal processing [@sweeney2019], machine/deep learning [@ngô2021; @bidder2020], or a combination of the two [@chakravarty2020]. However, bio-logging data are time series, which are difficult to classify using traditional methods [@keogh2003]. Fortunately, the data mining research community developed new algorithms specifically designed for time series [@bagnall2017; @ruiz2021], which they published in a standardized Python package, `sktime` [@löning2019].

`stickleback`, named for the classical animal behavior model organism, is a machine learning pipeline for automating behavioral event detection in bio-logging data. It interfaces with `sktime` to provide bio-logging scientists access to the latest developments in time series learning. The user interface was designed to solve many of the computational challenges facing bio-logging scientists. For example, interactive visualizations facilitate inspection of high-resolution, multi-variate bio-logging data, and users can define a temporal tolerance for "close enough" predictions. This package, `rstickleback`, solves another critical problem for bio-logging scientists. Ecology as a field preferentially uses R [@lai2019], but machine learning tools are most often developed in Python. This package, `rstickleback`, solves the language-domain mismatch by providing an R interface to Python-based tools.

# Algorithm description

`stickleback` is a supervised learning pipeline that operates in three steps. The local step trains a machine learning classifier on a subset of the data to differentiate events from non-events. The global step uses a sliding window and cross validation to identify a prediction confidence threshold for events. Finally, the boosting step uses prediction errors identified during the global step to augment the training data for the local step.

## Data structure

`stickleback` requires two types of data: bio-logging sensor data, $S$, and labeled behavioral events, $E$. $S$ can be raw data, such as tri-axial acceleration, or derived variables such as pitch or overall dynamic body acceleration [@gleiss2010; @wilson2006]. $E$ must be *points* in time. Contrast events with segmentation, where behaviors are *periods* of time, which is usually accomplished through unsupervised methods like hidden Markov models [@langrock2012]. Both $S$ and $E$ must be associated with bio-logger deployments, $d$.

## Local step

The goal of the local step is to train a time series classifier to differentiate behavioral events from the background (non-events). From the user's perspective, the critical inputs are (1) a time series classification model $M$ and (2) a window size $w$. $w$ determines the length of the time series extracted for training $M$.

`stickleback` extracts training data, $D_L$ for $M$ composed of $2n$ windows from $S$, where $n$ is the number of events in $E$. The training data includes (1) the windows in $S$ centered on all $n$ events in $E$ (class `events`) and (2) a non-overlapping random sample of windows in $S$ (class `non-events`).

Using a subset of $S$ for $D_L$ addresses the imbalanced class issue in behavioral event detection. For high resolution bio-logging data, behavioral events can be outnumbered by non-events by a factor on the order of 100-1000x or more. Therefore, a random sample of $n$ non-events undersamples the majority class, improving performance on the minority class [@haibohe2009]. This can lead to increased false positive rates, however, which is addressed later by the boosting step.

$M$ must be a time series classification model from the `sktime` package, which the local step fits to the local training data, $D_L$.

## Global step

The bio-logging sensor data, $S$, is longitudinal, but the time series classification model, $M$, is trained on windows of length $w$, so the global step connects the two time scales. The critical inputs are the temporal tolerance, $\epsilon$, and the number of folds for cross validation, $f$.

1.  First, the global step makes predictions using $M$ with a sliding window to produce a new time series, the local probability of an event, $p_l$. $p_l$ is a continuous time series, but recall that behavioral events are represented as points in time. So additional steps are required to extract predicted events from $p_1$.
2.  Then, the global step extracts all the peaks in $p_l$ and calculates their prominences $r$. Prominence is defined as the height of a peak relative to the lowest point between it and a higher peak, which represents how much a peak stands out relative to the nearby topography of the time series.
3.  Finally, the global step assesses the prediction outcome of the $p_l$ peaks at different prominence thresholds. Predicted events are considered true positives if they are the closest prediction in time to an event in $E$ *and* they are within the tolerance, $\epsilon$. Therefore, the outcome (true or false positive) of a $p_l$ peak depends on the prominence threshold, $\hat{r}$. Consider two $p_l$ peaks and an $\epsilon$ of 10 s. The first peak has a prominence of 0.75 and is 8 s from the nearest event in $E$; the second peak has a prominence of 0.5 and is 5 s from the nearest event in $E$. Both peaks are within $\epsilon$ of a known event, so if $\hat{r}$ is less than 0.5, then the second peak is a true positive and the first peak is a false positive. However, if the $\hat{r}$ is between 0.5 and 0.75, then the second peak is not a predicted event and the first peak is a true positive. The global step chooses $\hat{r}$ that maximizes the $F_1$ score of the predicted events.

The global step as described uses the same data to train $M$ and select $\hat{r}$, which will probably bias $\hat{r}$ too high. This is because the $p_l$ output of $M$ for out-of-sample data will likely be lower than for in-sample data. Therefore, the global step actually partitions $S$ and $E$ into $f$ folds and uses cross validation to choose $\hat{r}$. For each fold, a copy of $M$, $M'$, is trained on the other $f-1$ folds of data. Step 1 uses $M'$ to generate $p_l$ for the held out fold. The $p_l$ series for each fold are then merged, and steps 2 and 3 use the combined $p_l$ for selecting $\hat{r}$.

## Boosting step

Undersampling the majority class (`non-event`) can lead to increased false positives. These false positives are "near misses", where the animal's movement was similar enough to the behavior of interest to fool the time series classifier, $M$. These windows of time contain important information for differentiating between `event` and `non-event` windows, and are valuable for training $M$, but `stickleback` cannot know when they are *a priori*. Therefore, in the boosting step, all windows centered on the false positive predictions are added to $D_L$, the training data set for $M$. Then the local and global steps are repeated.

# Package overview

The Stickleback pipeline was developed for ecologists, who generally use R, but it required machine learning algorithms only implemented in Python. We addressed this language-domain mismatch by developing Stickleback in two packages. `stickleback` is a Python package, available on [PyPI](https://pypi.org/project/stickleback/), with classes, methods, and utility functions for formatting and visualizing data, defining and fitting models, and generating predictions. `rstickleback` is an R package, available on [GitHub](https://github.com/FlukeAndFeather/rstickleback), that provides wrapper functions and S4 classes around `stickleback`. A primary design goal was to require little to no Python experience from the ecologist end user. To accomplish that goal, we used [`reticulate`](https://rstudio.github.io/reticulate/) to manage environment configuration and Python package dependencies.

In `rstickleback`, use `Stickleback()` to define a model. The argument `tsc` (Time Series Classifier) corresponds to $M$. Use `compose_tsc()` or `create_tsc()` to define `tsc`. Arguments `win_size`, `tol`, and `n_folds` correspond to $w$, $\epsilon$, and $f$, respectively. `nth` modifies how the global step generates $p_l$. If `nth = 2`, for example, then $p_l$ is evaluated for every other window. Gaps are filled with cubic spline interpolation. `sb_fit()` runs all three steps in the method: local, global, and boosting. `sb_assess()` applies the temporal tolerance ($\epsilon$) to evaluate prediction outcomes (true positive, false positive, false negative).

`rstickleback` also provides interactive data visualization tools and sample data for exploring and developing familiarity. Use `sb_plot_data()` to audit bio-logging sensor and event data, and `sb_plot_predictions()` to explore model predictions. Sample data for six foraging blue whales (*Balaenoptera musculus*) are available through the `load_lunges()` function.

# Acknowledgements

Both authors were partially supported by Data Science Scholar fellowships from the Stanford Data Science Institute. We thank Nicole Nova and Salma Abdel-Raheem for their input and feedback on the Stickleback method.

# References
