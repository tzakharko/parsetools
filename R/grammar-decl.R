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
##' @importFrom rlang abort with_handlers trace_back calling
#' @import rlang
#'
#' @include grammar-ast.R
NULL


# ===================================================
#   Helpers
# ===================================================


# The operator precedence in R is different from what we want it to be in the grammar DSL
# In particular, we want ! and ? to have lower precedence than +, and %% to have higher
# precedence than +
#
# In this function we fix it
adjust.operator.precedence <- function(expr) {
  # check if an expression is an operator
  is.operatoexpr <- function(expr) {
    is.call(expr) && length(expr)>1 && (
      as.character(expr[[1]]) %in% c("+", "?", "!", "^", ":")
    )
  }
  
  # get the indices of arguments that are transparent for lowering/rising operations
  transparent.arguments <- function(expr) {
   # assert_that(is.call(expr) && length(expr)>1)

    if(expr[[1]] == quote(`%%`)) {
      # %% is a barrier, only left operand should be processed
      2L
    } else {
      as.integer(2:length(expr))
    }
  }
  
  # make sure that %% has precedence over everythign else
  raise.operators <- function(expr) {
    if(is.call(expr) && length(expr)>1) {
      # raise the operators in all subexpressions
      for(i in transparent.arguments(expr)) {
        expr[[i]] <- raise.operators(expr[[i]])
      }
      
      # if expr is an operator expression, check if last argument is a %% b
      n <- length(expr)
      if(is.operatoexpr(expr) && is.call(expr[[n]]) && expr[[n]][[1]] ==  quote(`%%`)) {      
        # o(a, .., b %% c)  -> o(a, ..., b) %% c
        expr0 <- expr
        
        expr <- expr[[n]]
        expr0[[n]] <- expr[[2]]
        
        expr[[2]] <- raise.operators(expr0)        
      }
    }
    
    expr
  }
  
  
  # make sure that ! and ? have the precedence lower than +
  lower.operators <- function(expr) {
    if(is.call(expr) && length(expr)>1) {
      # lower all subexpressions
      for(i in transparent.arguments(expr)) {
        expr[[i]] <- lower.operators(expr[[i]])
      }
      
      # !?(a + b) -> !?a+b
      if( (expr[[1]] == quote(`?`) || expr[[1]] == quote(`!`))  && 
          is.call(expr[[2]]) && expr[[2]][[1]] == quote(`+`)) {
            
        operator <- as.character(expr[[1]])
            
        expr <- expr[[2]]
        # lower the operator to the left-most argument
        expr[[2]] <- lower.operators(call(operator, expr[[2]]))
      }
    }  
        
    expr
  }
  
  expr %>% lower.operators() %>% raise.operators()
}

map_call <- function(expr, ...) {
  dtable <- as.list(substitute(list(...)))[-1]
  
  # check if default block exists
  default <- if(names(dtable)[length(dtable)] == "") { 
    length(dtable)
  } else {
    NA
  }
  
  # symbol to dispatch on
  sym <- if(is.call(expr) && length(expr)>0) {
    as.character(expr[[1]])
  } else {
    ""
  }
  
  # match the handler block
  i <- match(sym, names(dtable), nomatch = default)
  
  assert_that(!is.na(i))
  
  # construct and execute the handler
  handler <- as.function(c(alist(), dtable[[i]]), parent.frame())  
  handler() 
}

# ===================================================
#   AST builders
# ===================================================
make.hook.evaluators <- function(expr0, scope, label) {  
  list(
      requires = function(expr) {
        requires.ast(expr0, scope, substitute(expr))
      },
      "^" = function(prefix, suffix) {
        prefix <- substitute(prefix)
        suffix <- substitute(suffix)
      
        assert_that(is.call(suffix))      
        assert_that((scope <- as.character(suffix[[1]])) %in% c("expansion", "symbol", "parser"),
          msg=fmt("failure scope must be one of 'expansion', 'symbol' or 'parser', found '{scope}'")
        )
            
        call <- suffix
        call[[1]] <- prefix
      
        eval(call, make.hook.evaluators(expr0, scope, label))
      },
      "@" = function(hook, label) {
        assert_that(identical(substitute(hook), quote(report_failure)), 
          msg=fmt("@label syntax can only be used with report_failure()")
        )      
        assert_that(is.symbol(label <- substitute(label)), 
          msg=fmt("expected a label name, found {deparse(label)[1]}")
        )      
       
        # build a function that will inject the hook into the following context
        function(...) {
          call <- substitute(list(...))
          call[[1]] <- quote(report_failure)
          
          eval(call, make.hook.evaluators(expr0, scope, as.character(label)))
        }
      },
      report_failure = function(...) {
        args <- as.list(substitute(list(...)))[-1]
        assert_that(length(args)>0, 
          msg = "last argument to report_failure() must be an expression"
        )
            
        reporter <- args[[length(args)]]
        params <- args[-length(args)]
            
        for(p in params) {
          assert_that(is.symbol(p),
            msg = "expected a variable name instead of '{deparse(p)[1]}'"
          )
        }
      
        report.failure.ast(expr0, label, scope, reporter, map_chr(params, as.character))       
      }, 
      "{" = function(...) {
        e <- substitute(list(...))
        e[[1]] <- quote(`{`)
      
        transform.ast(expr0, e)
      }
    )
}



build.hook.ast <- function(expr0, hook.expr) {
  assert_that(is.ast(expr0))
   #
  # evaluators <- list(
  #   requires = function(expr) {
  #     requires.ast(expr0, "expansion", substitute(expr))
  #   },
  #   "^" = function(prefix, suffix) {
  #     prefix <- substitute(prefix)
  #     suffix <- substitute(suffix)
  #
  #     assert_that(is.call(suffix))
  #     assert_that((scope <- as.character(suffix[[1]])) %in% c("expansion", "symbol", "parser"),
  #       msg=fmt("failure scope must be one of 'expansion', 'symbol' or 'parser', found '{scope}'")
  #     )
  #
  #     call <- suffix
  #     call[[1]] <- prefix
  #
  #     e <- eval(call, list(
  #       requires = function(expr) { requires.ast(expr0, scope, substitute(expr))}
  #     ), parent.frame())
  #
  #     print(e)
  #     stop()
  #   },
  #   "@" = function(hook, label) {
  #     assert_that(identical(substitute(hook), quote(report_failure)),
  #       msg=fmt("@label syntax can only be used with report_failure()")
  #     )
  #     assert_that(is.symbol(label <- substitute(label)),
  #       msg=fmt("expected a label name, found {deparse(label)[1]}")
  #     )
  #
  #     function(...) {
  #       call <- substitute(list(...))
  #       call[[1]] <- quote(report_failure)
  #
  #       e <- eval.parent(call)
  #       assert_that(is.ast(e, "report.failure"))
  #
  #       report.failure.ast(e$expr, as.character(label), "expansion", e$body, e$arguments)
  #     }
  #   },
  #   report_failure = function(...) {
  #     args <- as.list(substitute(list(...)))[-1]
  #     assert_that(length(args)>0,
  #       msg = "last argument to report_failure() must be an expression"
  #     )
  #
  #     reporter <- args[[length(args)]]
  #     params <- args[-length(args)]
  #
  #     for(p in params) {
  #       assert_that(is.symbol(p),
  #         msg = "expected a variable name instead of '{deparse(p)[1]}'"
  #       )
  #     }
  #
  #     report.failure.ast(expr0, NULL, "expansion", reporter, map_chr(params, as.character))
  #   },
  #   "{" = function(...) {
  #     e <- substitute(list(...))
  #     e[[1]] <- quote(`{`)
  #
  #     transform.ast(expr0, e)
  #   }
  # )
  
  evaluators <- make.hook.evaluators(expr0, "expansion", NULL) 
  
  with_handlers({
      assert_that(is.call(hook.expr) && as.character(hook.expr[[1]]) %in% names(evaluators),
        msg= fmt("unknown hook type (value transformers must be blocks)")
      )
      eval(hook.expr, evaluators)
    },
    error=function(e) {
      abort(fmt("in {deparse(hook.expr)[[1]]}: {e$message}", collapse="\n"))
    }
  )
}


build.expansion.ast <- function(expr) {
  # the actual builder
  build <- function(expr) {
    # dispatch on the expression type
    map_call(expr, 
      # sequence expression
      "+" = {
        assert_that(length(expr)==3)
        
        # assemble the ast node 
        # make sure that the subnodes are built first
        ast0 <- build(expr[[2]])
        ast1 <- build(expr[[3]])
        sequence.ast(ast0, ast1)
      },
      # optional expression
      "?" = {
        assert_that(length(expr)==2)
        
        # assemble the ast node 
        # make sure that the subnodes are built first
        ast <- build(expr[[2]])
        optional.ast(ast)
      },
      # labeled expression
      ":" = {
        assert_that(length(expr)==3)
        assert_that(is.symbol(expr[[2]]),
          msg=fmt("'deparse{expr[[2]]}' is not a valid label name")
        )
        
        # assemble the ast node 
        # make sure that the subnodes are built first
        ast <- build(expr[[3]])
        label.ast(as.character(expr[[2]]), ast)
      },
      # location marker
      "!" = {
        assert_that(length(expr)==2)
        assert_that(is.symbol(expr[[2]]),
          msg=fmt("'deparse{expr[[2]]}' is not a valid label name")
        )
        
        marker.ast(as.character(expr[[2]]))
      },
      # precedence expression
      "^" = {
        assert_that(length(expr)==3)
        assert_that(is.numeric(expr[[3]]), 
          msg=fmt("'deparse{expr[[3]]}' is not a valid precedence level name")
        )

        # assemble the ast node 
        # make sure that the subnodes are built first
        ast <- build(expr[[2]])
        precedence.ast(expr[[3]], ast)
      },
      # lookahead
      "lookahead" = {
        assert_that(length(expr)==2,
          msg="lookahead expects a single grammar expansion argument, multiple arguments found"
        )
        
        # detect lookahead type (~ x) is negative
        arg <- expr[[2]]
        if(is.call(arg) && arg[[1]] == quote(`~`)) {
          type <- "negative"
          arg <- arg[[2]]
        } else {
          type <- "positive"
        }
        
        # assemble the ast node 
        # make sure that the subnodes are built first
        ast <- build(arg)
        lookahead.ast(ast, type=type)
      },
      # text 
      "text" = {
        assert_that(length(expr)>1, msg="missing text pattern")
        assert_that(is.character(expr[[2]]), msg="text pattern must be a character constant")
        
        ignore.case <- as.list(expr)$ignore.case
        if(is.null(ignore.case)) ignore.case <- FALSE
        assert_that(is.logical(ignore.case), msg="case.ignore.case must be TRUE or FALSE")
        
        text.pattern.ast(expr[[2]], type="exact", ignore.case=ignore.case)
      },
      # regex 
      "regex" = {
        assert_that(length(expr)>1, msg="missing text pattern")
        assert_that(is.character(expr[[2]]), msg="text pattern must be a character constant")
        
        ignore.case <- as.list(expr)$ignore.case
        if(is.null(ignore.case)) ignore.case <- FALSE
        assert_that(is.logical(ignore.case), msg="case.ignore.case must be TRUE or FALSE")
        
        text.pattern.ast(expr[[2]], type="regex", ignore.case=ignore.case)
      },
      # a hook 
      "%%" =  {
        assert_that(length(expr)==3)
        
        # assemble the ast node 
        # make sure that the subnodes are built first
        ast <- build(expr[[2]])
        build.hook.ast(ast, expr[[3]])
      },
      # other expression: must be a symbol or character at this point
      {
        if(is.character(expr)) {
          text.pattern.ast(expr, type="exact", ignore.case=FALSE)
        } else {
          assert_that(is.symbol(expr), msg="invalid grammar expansion expression")
          symbol.ast(as.character(expr)) 
        }        
      }
    )
  }

  # change the default operator precedence so that they
  # work with the grammar definition DSL
  expr <- adjust.operator.precedence(expr)
  
 
  # trasform R expressions into the grammar AST
  # catching local errors and converting them into informative messages
  with_handlers(
    build(expr),
    error = calling(function(cond) {
      message <- cond$message
      
      # find the last build call
      build.call.i <- detect_index(rev(sys.calls()), ~ {
        is.call(.x) && is.symbol(.x[[1]]) && .x[[1]] == quote(build)
      })
      
      # extract expr from that frame
      if(build.call.i != 0 && !is.null(expr <- sys.frame(-(build.call.i-1))$expr)) {
        fmt(
          "While processing grammar rule expansion",
          "",
          nofmt(str_c("  ", deparse(expr))),
          "",
          "an error has occurred: {message}",
          "",
          collapse="\n"
        ) -> message
      } 
      
      # forward throw the condition
      abort(message, trace=trace_back())
    })
  )
}



build.rule.ast <- function(expr) {
  # validate the rule declaration
  assert_that(is.call(expr) && expr[[1]] == quote(`<-`), 
    msg = fmt("in '{deparse(expr)}': a rule must have a form 'A -> B'")
  )
  
  # validate the left side
  assert_that(is.symbol(left <- expr[[3]]), 
    msg = fmt("'{deparse(left)}' is not a valid grammar rule name")
  )
  assert_that(!(left <- as.character(left)) %in% RESERVED.SYMBOLS,
    msg = fmt("'{left}' is a reserved rule name")
  )
  
  # build the ast
  left <- symbol.ast(left)
  right <- build.expansion.ast(expr[[2]])

  rule.ast(left, right)
}

build.recursive.rule.ast <- function(expr) {
  # validate the declaration
  assert_that(is.call(expr) && length(expr) == 2 && expr[[1]] == quote(`left_recursive`), 
    msg = fmt("in '{deparse(expr)}': not a recursive symbol declaration")
  )
  
  # validate the argument
  assert_that(is.symbol(sym <- expr[[2]]), 
    msg = fmt("'{deparse(sym)}' is not a valid grammar rule name")
  )
  
  recursive.decl.ast(as.character(sym))
}


#' Construct a parser
#'
#' @param grammar
#' @param compile
#'
#' @export
parser.grammar <- function(grammar, compile=TRUE) {
  # transform the grammar into the list of expressions
  grammar <- substitute(grammar)
  if(is.call(grammar) && grammar[[1]] == quote(`{`)) {
    grammar <- as.list(grammar)[-1]  
  } else {
    grammar <- list(grammar)
  }
  
  # build the AST for the grammar
  ast <- map(grammar, ~ {
    decl <- .
    
    with_handlers({
        dispatch <- if(is.call(decl)) {
          as.character(decl[[1]])
        } else {
          ""
        }
      
        switch(dispatch, 
          "<-" = build.rule.ast(decl),
          "left_recursive" = build.recursive.rule.ast(decl),
          abort("unknown parser grammar declaration ")
        )
      },
      error = function(cond) {
        fmt(
          nofmt(cond$message),
          "",
          "in parser grammar rule declaration",
          "",
          nofmt(str_c("  ", deparse(decl))),
          "",
          collapse="\n"
        ) -> message
        
        abort(message)
      }
    )
  })
  
  
  if(compile) {
    compile.grammar(ast, environment())
  } else {
    ast
  }
}



