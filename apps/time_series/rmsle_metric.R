################################################################################
# Custom yardstick metric implementations of root mean squared log error (rmsle)
#
# Author: João Nogueira
# Data: 14/11/2019
# Ref: https://tidymodels.github.io/yardstick/articles/custom-metrics.html
################################################################################

library(yardstick)
library(rlang)


# VECTORIZED IMPLEMENTATION ----
# EXAMPLE: rmsle_vec(y_test, y_pred)

rmsle_vec <- function(truth, estimate, na_rm = TRUE, ...) {
    
    rmsle_impl <- function(truth, estimate) {
        sqrt(mean((log1p(truth) - log1p(estimate))^2))
    }
    
    metric_vec_template(
        metric_impl = rmsle_impl,
        truth = truth, 
        estimate = estimate,
        na_rm = na_rm,
        cls = "numeric",
        ...
    )
}


# DATA FRAME IMPLEMENTATION ---- 
# EXAMPLE: tbl %>% rmse(truth = y_test, estimate = y_pred)

rmsle <- function(data, truth, estimate, na_rm = TRUE, ...) {
    UseMethod("rmsle")
}

rmsle.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
    metric_summarizer(
        metric_nm = "rmsle",
        metric_fn = rmsle_vec,
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        na_rm = na_rm,
        ...
    )
}
