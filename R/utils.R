# SPDX-License-Identifier: MIT
# Copyright (c) 2019 Taras Zakharko


# utils.R
# -------
#
# A set of utilities used throught the package
#


#' @importFrom crayon %+% red bold underline
#' @importFrom purrr map_chr
NULL

glue <- glue::glue

#' @rdname fmt
#'
#' @param str
#'
#' @export
nofmt <- function(str) {
  structure(as.character(str), .nofmt = TRUE)
}

#' Reexport of glue::glue
#'
#' @param ...
#' @param collapse
#'
#' @rdname fmt
#' @export
fmt <- function(..., collapse=NULL) {
  # perform string interpolation on each argument,
  # disabling inerpolation if string is marked as .nofmt
  # everythign is then joined together into a flat structure
  env <- parent.frame()
  out <- unlist(map(list(...), ~ {
    if(isTRUE(attr(.x, ".nofmt"))) .x else map_chr(.x, ~ glue(.x, .envir=env))
  }))
    
  if(!is.null(collapse)) out <- paste0(out, collapse=collapse)
    
  out    
}




#' Print formatted text
#'
#' @param text
#'
#' @rdname fmt
#' @export
fmt_print <- function(text) {
  text <- substitute(text)
  writeLines(eval(bquote(fmt(.(text))), parent.frame()))
}

#' Print an error message and quit execution, bypassing error handling
#'
#' @param msg
#'
#' @export
panic <- function(msg) {
  msg <- eval(bquote(fmt(.(msg))), parent.frame())
  msg <- red$bold$underline("Panic") %+% red(":", msg) 
    
  # get traceback
  tryCatch(
    stop(),
    error=function(e) {
      cat(msg, "\n")
      traceback(6)
      quit()
    } 
  )
  
  eval(bquote({
    stop(fmt(.(str)), call. = FALSE)
  }), parent.frame())
}


`%c%` <- c


wrap.closure <- function(body, env=parent.frame(), args = alist()) {
  if(is.call(body) && identical(body[[1]], as.symbol("{"))) {
    f <- eval(bquote(function() .(body)), env)
    formals(f) <- args
    f
  } else {
    body <- tryCatch(eval(body, env), error=function( ) NULL)
    if(is.function(body)) body else NULL
  }
}



