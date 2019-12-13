# SPDX-License-Identifier: MIT
# Copyright (c) 2019 Taras Zakharko


#' grammar-decl.R 
#' -------
#' Implements a DSL for specifying grammars 
#'
#'
#' 
#' @name grammar
#' 
#' 
#' @importFrom purrr map map_chr map_lgl keep map2 map_int detect_index
#' @importFrom crayon %+% 
#' @importFrom stringr str_pad str_c str_detect
#' @importFrom assertthat assert_that on_failure<-
NULL

# ===================================================
#   AST types
# ===================================================
is.ast <- function(expr, type=NULL) {
  assert_that(inherits(expr, "grammar.ast"))
  is.null(type) || inherits(expr, paste0(type, ".ast"))
}

is.parsing.expression.ast <- function(expr) {
  is.ast(expr, c("symbol", "lookahead", "optional", "sequence", "label", 
                 "precedence", "marker", "text.pattern"))
}

is.expansion.expression.ast <- function(expr) {
  is.parsing.expression.ast(expr) || 
  is.ast(expr, c("transform", "report.failure", "requires"))
}

on_failure(is.ast) <- function(call, env) {
  paste0(deparse(call$x), " is not a grammar AST node")
}

map_ast <- function(ast, ...) {
  dtable <- as.list(substitute(list(...)))[-1]
  
  # check if default block exists
  default <- if(names(dtable)[length(dtable)] == "") { 
    length(dtable)
  } else {
    NA
  }
  
  # match the handler block
  i <- detect_index(names(dtable), ~ is.ast(ast, .x))
  if(i==0) i <- default
  
  assert_that(!is.na(i))
  
  # construct and execute the handler
  #fmt_print("dispatching {names(dtable)[[i]]}")
  handler <- as.function(c(alist(), dtable[[i]]), parent.frame())  
  handler() 
}

RESERVED.SYMBOLS <- c("LINE.START", "LINE.END", "FILE.END", "LINE.REMAINING", "...")


# ===================================================
#   AST constructors
# ===================================================
symbol.ast <- function(name) {
  assert_that(
    is.character(name) && isTRUE(str_detect(name, "^[a-zA-Z][0-9a-zA-Z._]*$") || name==".space"), 
    msg = fmt("{deparse(name)} is not a valid grammar rule name")
  )
    
  structure(
    list(name=name), 
    class=c("symbol.ast", "grammar.ast")
  )  
}

lookahead.ast <- function(expr, type=c("positive", "negative")) {
  assert_that(
    is.ast(expr, c("symbol", "label", "precedence")), 
    msg = fmt("lookahead can only be applied to a rule match expression")
  )
  type <- match.arg(type)
  
  structure(
    list(expr = expr, type=type), 
    class=c("lookahead.ast", "grammar.ast")
  )  
}

optional.ast <- function(expr) {
  assert_that(
    is.ast(expr, c("symbol", "label", "precedence")), 
    msg = fmt("lookahead can only be applied to a symbol match expression")
  )
  
  structure(
    list(expr = expr), 
    class=c("optional.ast", "grammar.ast")
  )  
}


label.ast <- function(label, expr) {
  assert_that(
    is.character(label) && isTRUE(str_detect(label, "^[a-zA-Z][0-9a-zA-Z._]*$")), 
    msg = fmt("{deparse(label)} is not a valid label name")
  )
  
  assert_that(
    is.ast(expr, c("symbol", "precedence", "lookahead", "optional", "text.pattern")), 
    msg = fmt("labels can only be applied to a symbol match expression")
  )
  
  structure(
    list(label=label, expr=expr), 
    class=c("label.ast", "grammar.ast")
  )  
}

precedence.ast <- function(level, expr) {
  assert_that(
    is.numeric(level), 
    msg = fmt("{deparse(level)} is not a valid precedence level, number expected")
  )
  
  assert_that(is.ast(expr, c("symbol", "label")), 
    msg = fmt("precedence levels can only be applied to a symbol match expression")
  )
  
  structure(
    list(level=level, expr=expr),
    class=c("precedence.ast", "grammar.ast")
  )  
}

marker.ast <- function(name) {
  assert_that(
    is.character(name) && isTRUE(str_detect(name, "^[a-zA-Z][0-9a-zA-Z._]*$")), 
    msg = fmt("{deparse(name)} is not a valid location marker name")
  )
    
  structure(
    list(name=name), 
    class=c("marker.ast", "grammar.ast")
  )  
}

sequence.ast <- function(expr0, expr1) {
  assert_that(
    is.parsing.expression.ast(expr0) && is.parsing.expression.ast(expr1),
    msg = "expected an expression"
  )
  
  if(is.ast(expr0, "sequence")) {
    expr0 <- expr0$sequence
  } else {
    expr0 <- list(expr0)
  }

  if(is.ast(expr1, "sequence")) {
    expr1 <- expr1$sequence
  } else {
    expr1 <- list(expr1)
  }

  structure(
    list(sequence = c(expr0, expr1)), 
    class=c("sequence.ast", "grammar.ast")
  )  
}

text.pattern.ast <- function(pattern, type = c("regex", "exact"), ignore.case = TRUE) {
  assert_that(is.character(pattern) && length(pattern)==1)
  assert_that(is.logical(ignore.case))
  type <- match.arg(type)
  
  if(!ignore.case) pattern = tolower(pattern)
  
  structure(
    list(pattern = pattern, type = type, ignore.case = ignore.case),
     class=c("text.pattern.ast", "grammar.ast")
  )  
}


transform.ast <- function(expr, body) {
  assert_that(is.parsing.expression.ast(expr), 
    msg="value transformer must be the first transformer following an expanion"
  )
  assert_that(is.language(body), 
    msg="invalid transformer body"
  )
  
  structure(
    list(expr = expr, body = body),
     class=c("transform.ast", "grammar.ast")
  )  
}

report.failure.ast <- function(expr, label, scope, body, arguments) {
  assert_that(is.expansion.expression.ast(expr), 
    msg="failure reporter can only be applied to an expansion sequence"
  )
  assert_that(is.language(body), 
    msg="invalid failure reporter body"
  )
  assert_that(is.null(label) || (is.character(label) && (label %in% get_labels(expr))),
    msg=fmt("unknown label {deparse(label)[[1]]}")
  )
  assert_that(scope %in% c("expansion", "symbol", "parser"),
    msg=fmt("invalid scope {scope}")
  )
  assert_that(all(is.character(arguments)),
    msg="invalid failure reporter arguments"
  )
  # ensure that the final reporter is always the last one
  assert_that(!is.null(label) || !(is.ast(expr, "report.failure") && is.null(expr$label)),
    msg="the final failure reporter must be the last failure reporter specified"
  )

    
  structure(
    list(expr = expr, label=label, scope=scope, body = body, arguments=arguments),
     class=c("report.failure.ast", "grammar.ast")
  )  
}

requires.ast <- function(expr, scope, body) {
  assert_that(is.parsing.expression.ast(expr) || is.ast(expr, c("requires", "transform")), 
    msg="requires() must follow the value transformer but precede the any failure reporters"
  )
    
  assert_that(is.language(body), 
    msg="invalid failure reporter body"
  )
  assert_that(scope %in% c("expansion", "symbol", "parser"),
    msg="invalid scope value"
  )
  
  # move the requires expression down the tree, so that is immediately
  # follows the expansion sequence itself
  # this will eventually break the transform.ast constructoon invariant
  # but it maintains all semantic invariants and makes it easier for us
  # to compile the expressions
  push_ast <- function(expr) {
    if(is.ast(expr, c("transform"))) {
      # push into the subtree
      expr$expr <- push_ast(expr$expr)
      expr
    } else {
      # construct the requires ast node
      structure(
        list(expr = expr, scope = scope, body = body),
        class=c("requires.ast", "grammar.ast")
      )    
    }
  }
  
  push_ast(expr)
}

rule.ast <- function(symbol, expansion) {
  assert_that(is.ast(symbol, "symbol"))
  assert_that(is.expansion.expression.ast(expansion))
  
  assert_that(!(symbol$name %in% c("EOF", "EOL", "LINE.REMAINING")), 
    msg = fmt("grammar symbol '{symbol.name}' has a special meaning and is reserved")
  )
    
  structure(
    list(symbol=symbol, expansion=expansion), 
    class=c("rule.ast", "grammar.ast")
  )  
}

recursive.decl.ast <- function(symbol) {
  assert_that(is.character(symbol))
  
  structure(
    list(symbol=symbol), 
    class=c("recursive.decl.ast", "grammar.ast")
  )    
}

# ===================================================
#   AST manipulation
# ===================================================
get_labels <- function(ast){
  assert_that(is.ast(ast))
  
  if(is.ast(ast, "sequence")) {
    unlist(map(ast$sequence, get_labels))
  } else 
  if(is.ast(ast, "marker")) {
    ast$name
  } else 
  if(is.ast(ast, "label")) {
    ast$label
  } else
  if(!is.null(e <- ast$expr)) {
    get_labels(e)
  } else {
    character()
  }
}

# ===================================================
#   AST formatting
# ===================================================
format.symbol.ast <- function(x, ...) {
  x$name
}

format.lookahead.ast <- function(x, ...) {
  op <- switch(x$type, positive="", negative="~", stop("unknown lookahead type"))
  fmt("lookahead({op}{format(x$expr, ...)})")
}

format.optional.ast <- function(x, ...) {
  fmt("?{format(x$expr, ...)}")
}

format.marker.ast <- function(x, ...) {
  fmt("!{x$name}")
}


format.label.ast <- function(x, ...) {
  if(isTRUE(list(...)$short)) {
    format(x$expr, ...)
  } else {
    fmt("{x$label}:{format(x$expr, ...)}")
  }
}

format.precedence.ast <- function(x, ...) {
  if(isTRUE(list(...)$short)) {
    format(x$expr, ...)
  } else {
    fmt("{format(x$expr, ...)}^{x$level}")
  }
}

format.sequence.ast <- function(x, ...) {
  seq <- map_chr(x$sequence, ~ format(.x, ...))
  str_c(seq, collapse=" ")
}


format.hook.ast <- function(x, ...) {
  #fmt("transformed ({format(x$expr, ...)})")
  
  format(x$expr, ...)
}

format.text.pattern.ast <- function(x, ...) {
  fun <- switch(x$type, exact="text", regex="regex", stop("unknown text pattern type"))
  arg <- if(x$ignore.case) ", ignore.case=TRUE" else ""
  
  fmt("{fun}('{x$pattern}'{arg})")
}

format.rule.ast <- function(x, ...) {
  fmt("{format(x$symbol, ...)} -> {format(x$expansion, ...)}")
}

format.recursive.decl.ast <- function(x, ...) {
  fmt("recursive({x$symbol})")
}


format.transform.ast <- function(x, ...) {
  fmt("{format(x$expr)} %% {{ ... }")
}

format.report.failure.ast <- function(x, ...) {
  label <- if(!is.null(x$label)) fmt("@{x$label}") else ""
  args <- if(length(x$arguments)>0) fmt("({str_c(x$arguments, collapse=',')})") else ""
  scope <- if(x$scope == "expansion") "" else fmt("^{x$scope}")
  
  fmt("{format(x$expr)} %% report_failure{label}{scope}{args} {{ ... }")
}

format.requires.ast <- function(x, ...) {
  scope <- if(x$scope == "expansion") "" else fmt("^{x$scope}")
  fmt("{format(x$expr)} %% requires{scope} {{ ... }")
}


print.grammar.ast <- function(x, ...) {
  writeLines(format(x, ...))
}


