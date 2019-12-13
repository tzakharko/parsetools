# SPDX-License-Identifier: MIT
# Copyright (c) 2019 Taras Zakharko


#' @include src-buffer.R
NULL


parse.success <- function(value, location) {
  structure(list(value = value, location = location), class=c("parse.result", "parse.success"))
}


parse.failure <- function(payload = list()) {
  structure(payload, class=c("parse.result", "parse.failure"))
}

.parse.failure <- parse.failure()



msg.env <- function(failure) {
  assert_that(is.parse.failure(failure), info="msg.env() expects a parse failure")
  
  environment(attr(failure, "msg"))
}

#' Check parse results
#'
#' @param x
#'
#' @rdname parse.success
#' @export
is.parse.success <- function(x) inherits(x, "parse.success")


#' @rdname parse.success
#' @export
is.parse.failure <- function(x) inherits(x, "parse.failure")