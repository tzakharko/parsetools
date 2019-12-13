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
#' @importFrom purrr map map_chr map_lgl keep map2 map_int detect_index reduce modify_if imap rerun
#' @importFrom crayon %+% 
#' @importFrom stringr str_pad str_c str_detect str_trunc str_trim
#' @importFrom assertthat assert_that on_failure<-
#' @importFrom zeallot %<-%
#' @import rlang
#'
#' @include grammar-decl.R grammar-machine.R
NULL



# ===================================================
#   Parser program manupilation and analysis
# ===================================================

collect.jmp.labels <- function(program) {
  program %>%
    map(~ {      
      if(.$opcode %in% OP.CLASS.JUMP) {
        c(.$on.success, .$on.failure)
      } else 
      if(.$opcode %in% c(.$on.success, .$on.failure)) {
        .$address
      } else {
        character()
      }
    }) %>%
    unlist() %>%
    as.character() %>%
    unique()
}

join.programs <- function(programs) {
  program <- list()
  labels <- list()
  
  for(p in programs) {
    labels.p <- attr(p, "labels") %>% map(~ {
      .$line <- .$line + length(program)
      .
    })
    
    # join the fragment
    program <- c(program, p)
    labels <- c(labels, labels.p)
  }

  structure(program, class="parser.program", labels = labels)
}

link.program <- function(program) {
  # create a lookup-table for numbers
  labels <- attr(program, "labels")
  names(labels) <- map_chr(labels, "label")
  labels <- map(labels, "line")
  
  assert_that(!any(duplicated(names(labels))), 
    msg = {
      dp <- names(labels)
      dp <- dp[duplicated(dp)]
            
      fmt("Duplicated labels {str_c(dp, collapse=', ')} found in the program")
  })
  
  resolve_address <- function(pic, label) {
    if(is.numeric(label)) {
      label
    } else
    if(!is.na(addr <- suppressWarnings(as.integer(label)))) {
      pic + addr
    } else 
    if(!is.null(addr <- labels[[label]])) {
      addr
    } else {
      abort(fmt("Unknown jump label {label}"))
    }
  }
  
  
  # change the labels
  program <- imap(program, ~ {
    # resolve jump labels
    if(.x$opcode %in% OP.CLASS.JUMP) {
      .x$on.success <- resolve_address(.y, .x$on.success)
      .x$on.failure <- resolve_address(.y, .x$on.failure)
    }
    
    # resolve invoke labels
    if(.x$opcode %in% c(OPCODE.INVOKE.RECURSIVE, OPCODE.INVOKE)) {
      .x$address <- resolve_address(.y, .x$address)
    }
    
    .x
  }) %>% {
    structure(., class="parser.program", labels=attr(program,"labels"))
  }
  
  
  program
}

# ===================================================
#   Parser machine builder
# ===================================================
compile.expansion <- function(expansion.ast, label.prefix, decl.env) {
  # let us change the logic of this thing slightly...
  # first of all, i can just replace has.named.reporter by a arg to build since
  # the failure stuff is down-propagating anyway
  # second of all, the transformer is just bound to success
  # validators are a bit more tricky, since they need to come before the
  # success transformers
  # we will look at them in a second
  
  # # split the expression and the hooks
  # c(expansion.ast, hooks) %<-% extract.hooks(expansion.ast, decl.env)
  
  # a utility function for making failure label
  fail.label <- function(label) {
    if(is.null(label)) {
      fmt("{label.prefix}.fail.final")
    } else {
      fmt("{label.prefix}.fail@{label}")
    }
  }
  
  # a utility function for checking if there is a named
  # failure handlr
  # has.named.reporter <- function(label) {
  #   label %in% names(hooks$labeled.failure.reporters)
  # }
  
  # the program builder
  build <- function(ast, on.failure, failure.reporters) {
    map_ast(ast, 
      symbol = {
        # check for special symbols
        list(switch(ast$name, 
          LINE.START = op.sol("+1", on.failure),
          LINE.END = op.eol("+1", on.failure, TRUE),
          FILE.END = op.eof("+1", on.failure),
          LINE.REMAINING = op.line("+1", on.failure, TRUE),
          .space = op.regex("[[:space:]]*", FALSE, "+1", on.failure, TRUE),
          # for each other symbol, invoke its routine
          # we use recursive invoke here
          # and let the optimizer figure it out later
          op.invoke.recursive(paste0("@", ast$name), ast$name, 1, "+1", on.failure, TRUE)
        ))
      },
      text.pattern = {
        op <- switch(ast$type, exact=op.text,regex=op.regex, abort("unknown text pattern type"))
        
        list(op(ast$pattern, ast$ignore.case, "+1", on.failure, TRUE))
      },
      label = {
        # set failure jump labe, add the bind instruction
        if(ast$label %in% names(failure.reporters)) on.failure <- fail.label(ast$label)
        program <- build(ast$expr, on.failure, failure.reporters)
        
        join.programs(list(program, list(op.bind(ast$label))))
      },
      precedence = {
        program <- build(ast$expr, on.failure, failure.reporters)
        assert_that(program[[1]]$opcode == OPCODE.INVOKE.RECURSIVE)

        program[[1]]$precedence <- ast$level        
        program
      },
      optional = {
        program <- build(ast$expr, on.failure, failure.reporters)
        assert_that(program[[1]]$opcode %in% OP.CLASS.INPUT.MANIPULATION)
        
        # set the precedence level
        program[[1]]$on.failure <- "+1"
        program
      },    
      marker = {
        list(op.mark(ast$name))
      },
      lookahead = {
        program <-build(ast$expr, on.failure, failure.reporters)
        assert_that(program[[1]]$opcode %in% OP.CLASS.INPUT.MANIPULATION)

        program[[1]]$consume <- FALSE
        
        # swap the faiulure and success labels if the lookahead is negative
        if(ast$type == "negative") {
          t <- program[[1]]$on.failure
          program[[1]]$on.failure <- program[[1]]$on.success
          program[[1]]$on.success <- t
        }
        
        program
      },  
      sequence = {
        # build and combine the program sequence
        # we also want to track MARK commands so that we can set the
        # failure label properly
        map(ast$sequence, ~ {
          # set the failure jump label for subsequent operations
          if(is.ast(., "marker") && (.$name %in% names(failure.reporters))) {
            on.failure <<- fail.label(.$name)
          }
          build(., on.failure, failure.reporters)
        }) %>% join.programs()
      },
      transform = {
        # build the expansion program
        program <- build(ast$expr, on.failure, failure.reporters)
        
        # if there is just a single match command, we add the . bind
        if(length(program)==1 && program[[1]]$opcode %in% OP.CLASS.INPUT.MANIPULATION) {
          stub <- list(op.bind("."), op.eval(ast$body, decl.env))
        } else {
          stub <- list(op.eval(ast$body, decl.env))
        }
        
        join.programs(list(program, stub))  
      },
      requires = {
        # build the expansion program
        program <- build(ast$expr, on.failure, failure.reporters)
        
        # build the validation stub
        stub <- list(op.eval(ast$body, decl.env), op.test("+1", on.failure))
                           
        if(ast$scope != "expansion") {
          # we want to fail from the larger scope
          frames.to.skip <- switch(ast$scope, symbol=2, parser=-1, abort("bad validation scope"))
          stub[[2]]$on.success <- "+2"
          stub[[2]]$on.failure <- "+1"
          stub[[3]] <- op.fail(
            frames.to.skip, 
            list(failure.reporters[["@final"]]) %>% keep(~ !is.null(.))
          )
        }
        
        # if there is just a single match command, we add the . bind
        if(length(program)==1 && program[[1]]$opcode %in% OP.CLASS.INPUT.MANIPULATION) {
          stub <- join.programs(list(list(op.bind(".")), stub))
        } 
        
        join.programs(list(program, stub))  
      },
      report.failure = {
        # build the reporter function
        if(length(ast$arguments)>0) {
          args <- as.pairlist(rerun(length(ast$arguments), alist(x=)[[1]]))
          names(args) <- ast$arguments
        } else {
          args <- alist()
        }
        reporter.fun <- as.function(c(args, ast$body), decl.env)
        
        
        # add the reporter entry to the list 
        # also taking the final reporter into account
        entry <- structure(
          #list(reporter.fun, failure.reporters[["@final"]]) %>% keep(~ !is.null(.)), 
          list(reporter.fun),
          # set the name to @final if this is the final reporter
          names=if(is.null(ast$label)) "@final" else ast$label
        )
        
        # build the subtree, setting the on failure label
        # if this is the final reporter
        if(is.null(ast$label)) { on.failure <- fail.label(ast$label) }
        program <- build(ast$expr, on.failure, c(failure.reporters, entry))
        
        # add the success op if this is the first reporter
        if(!is.ast(ast$expr, "report.failure")) {
          program[[length(program)+1]] <- op.success()
        }
        
        # build the actual fail statement
        frames.to.skip <- switch(ast$scope, 
          expansion=1, 
          symbol=2, 
          parser=-1, 
          abort("bad failure scope")
        )
        stub <- list(op.fail(frames.to.skip, entry))
        attr(stub, "labels") <- list(make_label(1, fail.label(ast$label)))
        
        
        join.programs(list(program, stub))
      },
      {
        abort(fmt("Compilation for {class(ast)[1]} yet implemented"))
      }  
    )
  }
  
  program <- build(expansion.ast, "@fail.default", list())
  # add the success op if nessesary 
  # TODO: this can be inproved I am sure... 
  if(!is.ast(expansion.ast, "report.failure")) {
    program[[length(program)+1]] <- op.success()
  }
  
  # add the initial label
  attr(program, "labels") <- c(attr(program, "labels"), list(make_label(1, label.prefix)))
  
  # collect and validate the bindings
  bindings <- program %>% 
    keep(~ .$opcode %in% c(OPCODE.BIND, OPCODE.MARK)) %>%
    map_chr("name")
  
  assert_that(!any(duplicated(bindings)),
    msg = fmt("duplicated binding labels {str_c(labels[duplicated(bindings)], collapse=', ')}")
  )
  
  # # setup the binding list
  # bindings0 <- vector("list", length(bindings))
  # names(bindings0) <- bindings
  #
  # program[[1]]$bindings
  
  

  # return the program
  structure(program, class="parser.program", labels=attr(program, "labels"))
}

default.failure.reporter <- function() {
  c(
    "Parser failure at",
    "",
    format(.location),
    ""
  )
}

default.program.stub <- structure(
  list(op.fail(1, list())),
  class="parser.program",
  labels = list(make_label(1, "@fail.default"))
)

compile.rules <- function(symbol, rule.ast.list, decl.env) {
  # compile all expansions
  expansion.programs <- map(seq_along(rule.ast.list), ~ {
    label <- if(length(rule.ast.list)==1) {
      fmt("@{symbol}")
    } else {
      fmt("@{symbol}.{.}")
    }
    
    rule <- rule.ast.list[[.]]
    assert_that(is.ast(rule, "rule"))
    compile.expansion(rule$expansion, label, decl.env)
  })
  
  # build the program header
  if(length(rule.ast.list)>1) {
    success.label <- make_label(length(rule.ast.list)+1, fmt("@{symbol}.success"))
    
    # a series of calls to expansions
    program <- map(seq_along(rule.ast.list), ~ {
      # failures lead to the next expansion attempt, last failure is real
      on.failure <- if(. == length(rule.ast.list)) "@fail.default" else "+1"
      op.invoke(fmt("@{symbol}.{.}"), success.label$label, on.failure, TRUE)
    })
    
    program <- structure(
      c(program, list(op.success())),
      class = "parser.program",
      labels = list(make_label(1, fmt("@{symbol}")), success.label)
    )
  } else {
    program <- structure(list(), class = "parser.program")
  }
  
  
  # join the fragments and return the result
  join.programs(c(list(program), expansion.programs))  
}


compile.grammar <- function(grammar, decl.env) {
  # extract the rules and group expansions together
  rules <- grammar %>% keep(~ is.ast(., "rule"))
  names(rules) <- map_chr(rules, ~ .$symbol$name)
  rules <- split(rules, names(rules))
  
  assert_that("START" %in% names(rules), msg="Grammar is missing START")
  
  # sort the rules
  rules <- rules[order(!names(rules) == "START")]
  
  # compile the rules
  rules.compiled <- imap(rules, ~ {
    compile.rules(.y, .x, decl.env)
  })
    
  # get the recusive symbols
  recursive.symbols <- grammar %>% 
    keep(is.ast, "recursive.decl") %>% 
    map_chr("symbol")
  
  # combine all rules into a single program
  program <- join.programs(c(rules.compiled, list(default.program.stub))) 
  
  # optimize invoke
  program <- program %>%
    modify_if(
      ~ .$opcode == OPCODE.INVOKE.RECURSIVE &&  !.$address %in% paste0("@", recursive.symbols),
      ~ op.invoke(.$address, .$on.success, .$on.failure, .$consume)
    ) %>% 
    modify_if(
      ~ .$opcode == OPCODE.INVOKE.RECURSIVE, 
      ~ { .$slot <- match(.$slot, recursive.symbols); . }  
    )
    
  # link the program and add the recursive annotation
  program <- link.program(program)  
  attr(program, "recursive.symbols") <- recursive.symbols
  
  program
}


