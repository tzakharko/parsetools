# SPDX-License-Identifier: MIT
# Copyright (c) 2019 Taras Zakharko


#' grammar-machine.R 
#' -------
#' Implements a DSL for specifying grammars 
#'
#'
#' 
#' @name grammar
#' 
#' 
#' @importFrom purrr map map_chr map_lgl keep map2 map_int detect_index reduce modify_if
#' @importFrom crayon %+% 
#' @importFrom stringr str_pad str_c str_detect str_trunc str_trim
#' @importFrom assertthat assert_that on_failure<-
#' @importFrom zeallot %<-%
#' @import rlang
#'
#' @include grammar-decl.R
#' @include utils.R

# ===================================================
#   Parser machine instructions
# ===================================================
PARSER.INSTRUCTIONS = c(
  "TEXT", 
  "TEXT.IGNORE.CASE", 
  "REGEX",
  "SOL",
  "EOL",
  "EOF",
  "LINE",
  "INVOKE",
  "INVOKE.RECURSIVE",
  "EVAL",
  "BIND",
  "MARK",
  "TEST",
  "FAIL",
  "SUCCESS" 
)

OPCODE.TEXT              <- 1L
OPCODE.TEXT.IGNORE.CASE  <- 2L
OPCODE.REGEX             <- 3L
OPCODE.SOL               <- 4L
OPCODE.EOL               <- 5L
OPCODE.EOF               <- 6L
OPCODE.LINE              <- 7L
OPCODE.INVOKE            <- 8L
OPCODE.INVOKE.RECURSIVE  <- 9L
OPCODE.EVAL              <- 10L
OPCODE.BIND              <- 11L
OPCODE.MARK              <- 12L
OPCODE.TEST          <- 13L
OPCODE.FAIL              <- 14L
OPCODE.SUCCESS           <- 15L



OP.CLASS.INPUT.MANIPULATION  <- c(OPCODE.TEXT, OPCODE.TEXT.IGNORE.CASE, OPCODE.REGEX, OPCODE.SOL,
                                  OPCODE.EOL, OPCODE.EOF, OPCODE.LINE, OPCODE.INVOKE, 
                                  OPCODE.INVOKE.RECURSIVE)
                                  
OP.CLASS.JUMP                <- c(OP.CLASS.INPUT.MANIPULATION, OPCODE.TEST)


matchop <- function(op) match(deparse(substitute(op)), PARSER.INSTRUCTIONS)

assert_that(identical(OPCODE.TEXT             , match("TEXT"             , PARSER.INSTRUCTIONS)))
assert_that(identical(OPCODE.TEXT.IGNORE.CASE , match("TEXT.IGNORE.CASE" , PARSER.INSTRUCTIONS)))
assert_that(identical(OPCODE.REGEX            , match("REGEX"            , PARSER.INSTRUCTIONS)))
assert_that(identical(OPCODE.SOL              , match("SOL"              , PARSER.INSTRUCTIONS)))
assert_that(identical(OPCODE.EOL              , match("EOL"              , PARSER.INSTRUCTIONS)))
assert_that(identical(OPCODE.EOF              , match("EOF"              , PARSER.INSTRUCTIONS)))
assert_that(identical(OPCODE.LINE             , match("LINE"             , PARSER.INSTRUCTIONS)))
assert_that(identical(OPCODE.INVOKE           , match("INVOKE"           , PARSER.INSTRUCTIONS)))
assert_that(identical(OPCODE.INVOKE.RECURSIVE , match("INVOKE.RECURSIVE" , PARSER.INSTRUCTIONS)))
assert_that(identical(OPCODE.EVAL             , match("EVAL"             , PARSER.INSTRUCTIONS)))
assert_that(identical(OPCODE.BIND             , match("BIND"             , PARSER.INSTRUCTIONS)))
assert_that(identical(OPCODE.MARK             , match("MARK"             , PARSER.INSTRUCTIONS)))
assert_that(identical(OPCODE.TEST         , match("TEST"         , PARSER.INSTRUCTIONS)))
assert_that(identical(OPCODE.FAIL             , match("FAIL"             , PARSER.INSTRUCTIONS)))
assert_that(identical(OPCODE.SUCCESS          , match("SUCCESS"          , PARSER.INSTRUCTIONS)))

op.text <- function(pattern, ignore.case, on.success, on.failure, consume) {
  assert_that(is.character(pattern))
  assert_that(is.logical(ignore.case))
  assert_that(is.logical(consume))
  
  
  if(!ignore.case) {
    structure(list(
      opcode = OPCODE.TEXT,             # 1
      on.success = on.success,          # 2
      on.failure = on.failure,          # 3
      consume = consume,                # 4
      pattern = pattern                 # 5
    ), class="parser.instruction")    
  } else {
    structure(list(
      opcode = OPCODE.TEXT.IGNORE.CASE, # 1
      on.success = on.success,          # 2
      on.failure = on.failure,          # 3
      consume = consume,                # 4
      pattern = tolower(pattern)        # 5
    ), class="parser.instruction")
    
  }
}

op.regex <- function(pattern, ignore.case, on.success, on.failure, consume) {
  assert_that(is.character(pattern))
  assert_that(is.logical(ignore.case))
  assert_that(is.logical(consume))
  
  pattern <- str_c("^(", pattern, ")", sep="")
  
  structure(list(
    opcode = OPCODE.REGEX,            # 1
    on.success = on.success,          # 2
    on.failure = on.failure,          # 3
    consume = consume,                # 4
    pattern = pattern,                # 5
    ignore.case = ignore.case         # 6
  ), class="parser.instruction")
}

op.sol <- function(on.success, on.failure) {
  structure(list(
    opcode = OPCODE.SOL,              # 1
    on.success = on.success,          # 2
    on.failure = on.failure           # 3
  ), class="parser.instruction")
}


op.eol <- function(on.success, on.failure, consume) {
  assert_that(is.logical(consume))
  
  structure(list(
    opcode = OPCODE.EOL,              # 1
    on.success = on.success,          # 2
    on.failure = on.failure,          # 3
    consume = consume                 # 4
  ), class="parser.instruction")
}


op.eof <- function(on.success, on.failure) {
  structure(list(
    opcode = OPCODE.EOF,              # 1
    on.success = on.success,          # 2
    on.failure = on.failure           # 3
  ), class="parser.instruction")
}

op.line <- function(on.success, on.failure, consume) {
  assert_that(is.logical(consume))
  
  structure(list(
    opcode = OPCODE.LINE,             # 1
    on.success = on.success,          # 2
    on.failure = on.failure,          # 3
    consume = consume                 # 4
  ), class="parser.instruction")
}

# op.multiline <- function(barrier.addr, on.success, on.failure, consume) {
#   assert_that(is.logical(consume))
#
#   structure(list(
#     opcode = OPCODE.LINE,             # 1
#     on.success = on.success,          # 2
#     on.failure = on.failure,          # 3
#     consume = consume                 # 4
#     barrier.addr = barrier.addr       # 5
#   ), class="parser.instruction")
# }


op.invoke <- function(address, on.success, on.failure, consume) {
  assert_that(is.logical(consume))
  
  structure(list(
    opcode = OPCODE.INVOKE,           # 1
    on.success = on.success,          # 2
    on.failure = on.failure,          # 3
    consume = consume,                # 4
    address = address                 # 5
  ), class="parser.instruction")
}

op.invoke.recursive <- function(address, slot, precedence, on.success, on.failure, consume) {
  assert_that(is.logical(consume))
  assert_that(is.numeric(precedence))
  
  structure(list(
    opcode = OPCODE.INVOKE.RECURSIVE, # 1
    on.success = on.success,          # 2
    on.failure = on.failure,          # 3
    consume = consume,                # 4
    address = address,                # 5
    slot = slot,                      # 6
    precedence = precedence           # 7
  ), class="parser.instruction")
}

op.eval <- function(expr, env) {
  assert_that(is.language(expr))
  assert_that(is.environment(env))
  
  structure(list(
    opcode = OPCODE.EVAL,             # 1
    expr = expr,                      # 2
    env = env                         # 3
  ), class="parser.instruction")
}

op.bind <- function(name) {
  assert_that(is.character(name))
  
  structure(list(
    opcode = OPCODE.BIND,             # 1
    name = name                       # 2
  ), class="parser.instruction")
}

op.mark <- function(name) {
  assert_that(is.character(name))
  
  structure(list(
    opcode = OPCODE.MARK,             # 1
    name = name                       # 2
  ), class="parser.instruction")
}

op.test <- function(on.success, on.failure) {
  structure(list(
    opcode = OPCODE.TEST,         # 1
    on.success = on.success,          # 2
    on.failure = on.failure           # 3
  ), class="parser.instruction")
}



op.fail <- function(frames, reporters) {    
  assert_that(is.list(reporters))
  assert_that(all(map_lgl(reporters, is.function)))
  
  structure(list(
    opcode    = OPCODE.FAIL,          # 1
    reporters = reporters,            # 2
    frames    = frames                # 3
  ), class="parser.instruction")
}


op.success <- function() {
  structure(list(
    opcode = OPCODE.SUCCESS           # 1
  ), class="parser.instruction")
}


make_label <- function(line, label) {
  list(line=line, label=label)
} 


# ===================================================
#   Parser machine
#
# ===================================================

# We set up the instruction table, where each entry
# stores an unevaluated R code for each opcode
#
# The parse machine then simply runs eval() on the instructions
#
# The parser machine has access to the following variables
#
#    pic            the current instruction address
#    bindings       the list of bindings in the current context
#    invoke.stack   a stack of call frames in the following format
#
#                       the stack is implemented as R pairlist
#    mtable             the memoisation table (recursive matching support)
#    invoke.subparser()
parser.instruction.table <- local({
  impl <- function(e) {
    # optimize named list access to direct subset operator
    index.table <- list(
      instruction = list(
        opcode = 1, 
        on.success = 2,
        on.failure = 3,
        consume = 4, 
        pattern = 5, 
        ignore.case = 6, 
        address = 5,   
        slot = 6,      
        precedence = 7,
        name = 2,
        expr = 2,
        env = 3,
        reporters = 2,
        frames = 3
      ),
      location = list(
        row = 1,
        col = 2,
        line = 3,
        buffer = 4
      )
    )
      
    optimize_getters <- function(e) {
      if(is.pairlist(e))  {
        as.pairlist(map(e, optimize_getters))
      } else if (length(e) <= 1L) {
        e
      } else if (e[[1]] == quote(`$`) &&  (as.character(e[[2]]) %in% names(index.table)) ) {
        table <- index.table[[as.character(e[[2]])]]
        index <- table[[as.character(e[[3]])]]
        assert_that(!is.null(index), 
          msg = fmt("Unknown element {as.character(e[[2]])}${as.character(e[[3]])}")
        )
        e[[3]] <- index
        e[[1]] <- as.symbol("[[")
        e
      } else {
        as.call(map(e, optimize_getters))
      }
    }
    
    optimize_getters(substitute(e))
  }
  
  instructions = list(
    TEXT = impl({
      line <- location$line
      pattern <- instruction$pattern

      # check if we have a match
      if(isTRUE(startsWith(line, pattern))) {
        success <- TRUE
        value <- pattern
    
        # do we need to consume the buffer?
        if(instruction$consume && (len <- nchar(pattern))>0) {
          location$col <- location$col + len
          location$line <- str_sub(line, start = len + 1)
          
          mtable <- .empty.mtable
        }

        pic <- instruction$on.success
      } else {
        # failure
        value <- NULL
        success <- FALSE

        pic <- instruction$on.failure
      }
    }),
    TEXT.IGNORE.CASE = impl({
      line <- location$line
      pattern <- instruction$pattern

      # check if we have a match
      if(isTRUE(startsWith(tolower(line), pattern))) {
        # match length
        len <- nchar(pattern)
      
        success <- TRUE
        # value <- instruction$pattern
        value <- str_sub(line, 1, len)
    
        # do we need to consume the buffer?
        if(instruction$consume && len>0) {
          location$col <- location$col + len
          location$line <- str_sub(line, start = len + 1)
          
          mtable <- .empty.mtable
        }

        pic <- instruction$on.success
      } else {
        # failure
        value <- NULL
        success <- FALSE

        pic <- instruction$on.failure
      }
    }),
    REGEX = impl({
      line <- location$line
        
      if(isTRUE((match <- regexpr(instruction$pattern, line, instruction$ignore.case)) > 0)) {
        len <- attr(match, "match.length")
      
        success <- TRUE    
        value <- str_sub(line, 1, len)
    
        # do we need to consume the buffer?
        if(instruction$consume && len>0) {
          location$col <- location$col + len
          location$line <- str_sub(line, start = len + 1)
          mtable <- .empty.mtable
        }
        
        pic <- instruction$on.success
      } else {
        # failure
        value <- NULL
        success <- FALSE

        pic <- instruction$on.failure
      }      
    }),  
    SOL = impl({
      if(isTRUE(location$col == 1 && !is.na(location$line))) {
        success <- TRUE
        value <- ""
    
        pic <- instruction$on.success
      } else {
        success <- FALSE
        value <- NULL
    
        pic <- instruction$on.failure
      }
    }),
    EOL = impl({
      if(isTRUE(location$line == "")) {
        success <- TRUE
        value <- ""
    
        # advance if needed
        if(instruction$consume) {
          location$row = location$row + 1
          location$col = 1
          location$line = location$buffer$get.lines(location$row, NA)
          
          mtable <- .empty.mtable
        }
    
        pic <- instruction$on.success
      } else {
        success <- FALSE
        value <- NULL
    
        pic <- instruction$on.failure
      }
    }),
    EOF = impl({      
      if(is.na(location$line)) {
        success <- TRUE
        value <- ""
    
        pic <- instruction$on.success
      } else {
        success <- FALSE
        value <- NULL
    
        pic <- instruction$on.failure
      }
      
      #fmt_print("EOF jumping to {pic}")
    }),
    LINE = impl({
      if(!is.na(line <- location$line)) {
        success <- TRUE
        value <- line
    
        # advance if needed
        if(instruction$consume) {
          location$row = location$row + 1
          location$col = 1
          location$line = location$buffer$get.lines(location$row, NA)
          
          mtable <- .empty.mtable
        }
    
        pic <- instruction$on.success
      } else {
        success <- FALSE
        value <- NULL
    
        pic <- instruction$on.failure
      }
    }),
    INVOKE = impl({
      # save the call frame
      frame <- list(
        bindings,               # 1
        location,               # 2
        mtable,                 # 3
        instruction$on.success, # 4
        instruction$on.failure, # 5
        instruction$consume     # 6
      )
      
      stack <- new_node(frame, stack)
      
      # prepare the state for the subparser
      bindings <- list()
      
      # and jump to the address
      pic <- instruction$address
    }),
    INVOKE.RECURSIVE = impl({
      mslot <- instruction$slot

      # check the memoisation table
      #
      # entries in the memosation table have the format
      #
      # (value, location, precedence)
      #
      if(!is.null(mentry <- mtable[[mslot]])) {
        # memoised value is empty (match of depth 0) or we have a precedence mismatch
        if(identical(mentry, .recursion.failure) || (instruction$precedence < mentry[[3]])) {
          #fmt_print("memoisation returns failure")
          # jump to failure
          success <- FALSE
          value <- NULL
          pic <- instruction$on.failure
        } else {
          #fmt_print("memoisation returns success")
          # return the memoised value          
          success <- TRUE
          value <- mentry[[1]]
          location <- mentry[[2]]
          
          # jump to success
          pic <- instruction$on.success
        }
      } else {
        # we are starting the new recursive match
        address <- instruction$address
        precedence <- instruction$precedence
        #precedence <- 0
        
        # get the first match
        mtable[[mslot]] <- .recursion.failure        
        result.0 <- .parse_machine_thread(program, address, location, mtable)
        
        #print(result.0)
                
        # if there is no match, we have failed
        if(!result.0[[1]]) {
          # copy the failure reporters
          success <- FALSE
          value <- result.0[[2]]
          
          pic <- instruction$on.failure
        } else {
          # continue matching
          result.0.loc <- result.0[[3]]
          mtable[[mslot]] <- list(result.0[[2]], result.0.loc, precedence)
          
          # continue incremental matching until we fail or
          # we start getting the same result as result.0
          # n <- 1
          while({
            # n <- n + 1
            result.i <- .parse_machine_thread(program, address, location, mtable)
            (success <- result.i[[1]]) && !identical(result.0.loc, result.i[[3]])
          }) {
            # if(n>5) {
            #   print(result.0)
            #   print(result.i)
            #   abort()
            # }
            
            # print(result.i)
            # cat("---\n")

            mtable[[mslot]] <- list(result.i[[2]], result.i[[3]], precedence)
          }
          
          # cat("----\n")
          # print(result.i)
          # # print(mtable[[mslot]])
          #
          # stop()
          
          # the recursive match suceeds if we match result.0 again
          # we guard against it by checking the location 
          # so to check if the call failed it is sufficient to check whether
          # result.i is a failure
          if(success) {
            # the parse result is the memoised value since the last value is result.0 again
            result.last <- mtable[[mslot]]
              
            value <- result.last[[1]]

            # advance the location if requested
            if(instruction$consume) {
              location <- result.last[[2]]
            }

            # jump to the success label
            pic <- instruction$on.success
          } else {
            # fmt_print("found failure")
            # print(result.i[[2]])
            # stop()
            #
            # propagate the failure
            value <- result.i[[2]]
            
            pic <- instruction$on.failure
          }      
        }
      }      
    }),
    EVAL = impl({
      value <- eval(instruction$expr, bindings, instruction$env)
      pic <- pic + 1L
    }),
    BIND = impl({
      bindings[instruction$name] <- if(success) list(value) else list(NULL)
      #fmt_print("Bound {instruction$name} = {deparse(bindings[[instruction$name]])[1]}")
      pic <- pic + 1L
    }),
    MARK = impl({
      bindings[[instruction$name]] <- location
      pic <- pic + 1L
    }),
    TEST = impl({
      if(isTRUE(value)) {
        pic <- instruction$on.success
      } else {
        pic <- instruction$on.failure
      }
    }),
    FAIL = impl({
      # if (pic == 44) {
         # fmt_print("FAIL encountered at {pic}")
         # print(location)
     #    print(value)
     #    stop()
     #  }
      
      # push the error reporters
      envdata <- bindings
      envdata$.location <- location
      trace <- list(list(instruction$reporters, envdata))
      
      if(success) {
        success <- FALSE
        value <- trace
      } else {
        value <- c(value, trace)
      }
      
      # restore the frame
      n <- instruction$frames
      repeat {
        # if we are ut of frames, stop the machine
        if(is.null(stack) || (n == -1)) {
          #fmt_print("FAIL at {pic} stopping the thread")
          # print(success)
          #
          # stop()
          pic <- 0L
          break
        }
                
        # pop as many frames as it is requested
        if(n > 1) {
          stack <- node_cdr(stack)
          n <- n -1
          next
        }
        
        # pop the frame
        frame <- node_car(stack)
        stack <- node_cdr(stack)
        
        # restore the frame
        bindings <- frame[[1]]
        location <- frame[[2]]
        mtable   <- frame[[3]]
        
        # jump to next failure addrss
        pic      <- frame[[5]]
        #fmt_print("FAIL jumping to {pic}")
        
        break
      } 
    }),
    SUCCESS = impl({
      # set the success status
      success <- TRUE
      
      # if we are out of frames, stop the machine
      if(is.null(stack)) {
        pic <- 0L
      } else {   
        # pop the frame
        frame <- node_car(stack)
        stack <- node_cdr(stack)
        
        # restore the frame
        bindings <- frame[[1]]
        
        # restore the location if consumption is not required 
        if(!frame[[6]]) {
          location <- frame[[2]]
          mtable   <- frame[[3]]
        }
        
        # jump to next success address
        pic      <- frame[[4]]
      }
    })
  )
    
  assert_that(identical(names(instructions), PARSER.INSTRUCTIONS))
  
  #instructions$INVOKE.RECURSIVE <- instructions$INVOKE
  
  map(instructions, compiler::compile)
  instructions
})



# ===================================================
#   Failure reporter construction
# ===================================================

# return a copy of the function with envdata as proxy environment
inject.function.env <- function(fun, envdata) {
  environment(fun) <- list2env(envdata, parent=environment(fun))
  
  fun
}

make.next.reporter <- function(stack) {
  # find the next trace with an active reporter
  i <- length(stack)
  while(i>0 && length(stack[[i]][[1]])==0) { i <- i - 1 }
  
  # raise an error if this is the last trace
  if(i==0) { 
    abort("cannot invoke previous_failure() at the bottom of the failure stack") 
  }
    
  reporters <- stack[[i]][[1]]
  envdata <-  stack[[i]][[2]]
  stack <- stack[seq_along(stack)<i]
  
  
  # build the initial reporter function
  reporter.promise <- bquote({
    fun <- inject.function.env(.(reporters[[1]]), .(envdata))
    delayedAssign(
      "previous_failure", 
      make.next.reporter(.(stack)), 
      assign.env=environment(fun)
    )
    fun
  })
  
  # add the other reporters (whese will wrap the previous reporter...)
  for(reporter in reporters[-1]) {
    reporter.promise <- bquote({
      fun <- inject.function.env(.(reporter), .(envdata))
      delayedAssign(
        "previous_failure", 
        .(reporter.promise), 
        assign.env=environment(fun)
      )
      fun
    })
  }
    
  # evaluate the promise to get the reporter function
  eval(reporter.promise)
}



# ===================================================
#   Parser machine
# ===================================================


rlang_eval <- rlang:::rlang_eval

.recursion.failure <- new.env(parent=emptyenv())


.parse_machine_thread <- function(program, pic, location, mtable) {
  bindings <- list()
  stack <- NULL
  success <- TRUE
  currentenv <- environment()
  value <- NULL
  
  #cat("Started subparser at location:")
  #print(location)
  
  while(pic > 0) {    
    # fetch
    instruction <- program[[pic]]
    # execute
    #fmt_print("[{pic}] {format(instruction, as.matrix=FALSE)} $${deparse(location$line)}")
    .Call(rlang_eval, parser.instruction.table[[instruction[[1]]]], currentenv)
    #print(value)
  }
  
  # print(value)
  # fmt_print("trhread done with success={success}, value={if(success) deparse(value) else '...failures..}")

  list(success, value, location)
}


parse_machine_invoke <- function(program, location) {
  .empty.mtable <- vector("list", length(attr(program, "recursive.symbols")))
  environment(.parse_machine_thread) <- environment()
  
  result <- .parse_machine_thread(program, 1L, location, .empty.mtable)

  if(result[[1]]) {
    parse.success(result[[2]], result[[3]])
  } else {
    # print("aaaaa")
    # print(result[[2]])
    #stop()
    # return the reporter for the top-most failure
    reporter <- with_handlers( {
        make.next.reporter(result[[2]])
      },
      # if there are no reporters in the failure stack
      # make a default one for the inner-most failure
      error = function(e) {
        inject.function.env(default.failure.reporter, result[[2]][[1]][[2]])
      }
    )
    parse.failure(reporter)
  }
}

#' Run the parser machine
#'
#'
#' @param program
#' @param location
#'
#' @export
run_parser <- parse_machine_invoke




# ===================================================
#   Instruction printing
# ===================================================
short_deparse <- function(expr, maxwidth=20) {
  text <- str_c(str_trim(deparse(expr, control="niceNames")), collapse=" ")

  if(nchar(text)>maxwidth) {
    str_trunc(text, maxwidth, side="right")
  } else {
    text
  }
}

format.parser.instruction <- function(instruction, as.matrix=TRUE, ...) {
  assert_that(inherits(instruction, "parser.instruction"))
  
  op <- PARSER.INSTRUCTIONS[instruction$opcode]
    
  fmt.jumps <- function(instruction) {
    fmt("{instruction$on.success} !{instruction$on.failure}")
  }
  
  modifiers <- c()
  
  add.modifier <- function(condition, true.value, false.value = character(0)) {
    modifiers <<- c(modifiers, if(condition) true.value else false.value)
  }
  
  format.failure.reporter <- function(reporter) {
    as.call(c(list(quote(on_failure)), names(formals(reporter)) %>% map(as.symbol)))
  }
  
  mat <- switch(op,
    TEXT = {
      add.modifier(instruction$consume,"→", "⊙")
      
      cbind(
        fmt("'{instruction$pattern}'"),
        fmt.jumps(instruction)
      )
    },
    TEXT.IGNORE.CASE = {
      add.modifier(instruction$consume,"→", "⊙")
      
      cbind(
        fmt("'{instruction$pattern}'"),
        fmt.jumps(instruction)
      )
    },    
    REGEX = {
      add.modifier(instruction$consume,"→", "⊙")
      if(instruction$ignore.case) op <- "REGEX.IGNORE.CASE"
      
      cbind(
        fmt("'{instruction$pattern}'"),
        fmt.jumps(instruction)
      )
    },
    SOL = {
      cbind(
        "",
        fmt.jumps(instruction)
      )      
    },
    EOL = {
      add.modifier(instruction$consume,"→", "⊙")
      
      cbind(
        "",
        fmt.jumps(instruction)
      )
    },
    EOF = {      
      cbind(
        "",
        fmt.jumps(instruction)
      )
    },
    LINE = {
      add.modifier(instruction$consume,"→", "⊙")
      
      cbind(
        "",
        fmt.jumps(instruction)
      )
    },
    INVOKE = {
      add.modifier(instruction$consume,"→", "⊙")

      cbind(
        as.character(instruction$address),
        fmt.jumps(instruction)
      )        
    },
    INVOKE.RECURSIVE = {
      op <- "INVOKE"
      add.modifier(instruction$consume,"→", "⊙")
      add.modifier(TRUE, "R")

      cbind(
        fmt("{instruction$address} <{instruction$slot}^{instruction$precedence}>"),
        fmt.jumps(instruction)
      )        
    },
    EVAL = {
      cbind(
        short_deparse(instruction$expr),
        ""
      )        
    },
    BIND = {
      cbind(
        instruction$name,
        ""
      )        
    },
    MARK = {
      cbind(
        instruction$name,
        ""
      )        
    },
    TEST = {
      cbind(
        "isTRUE(value)",
        fmt.jumps(instruction)
      )        
    },
    FAIL = {
      n <- length(instruction$reporters) 
      cbind(
        fmt("[{n} reporter{if(n==1) '' else 's'}]"),
        fmt("frames={instruction$frames}")
      )
    },
    SUCCESS = {
      cbind(
        "",
        ""
      )
    },
    abort(fmt("Unknown instruction {op}"))
  )
  
  if(length(modifiers)==0) modifiers <- ""
  
  
  mat <- cbind(op, str_c(modifiers, collapse=" "), mat)
  if(isTRUE(as.matrix)) {
    mat
  } else {
    str_c(as.character(mat), collapse=" ")
  }
}

print.parser.instruction <- function(instruction, ...) {
  txt <- format(instruction, ...)
    
  map_chr(1:nrow(txt), ~ str_c(txt[., ], collapse=" ")) %>%
  writeLines()
}

#' @export
print.parser.program <- function(program, ...) {
  if(length(program)==0) {
    cat("(empty program)\n")
    return()
  }
  
  txt <- map(program, format.parser.instruction) %>% do.call(rbind, .)
  txt <- cbind(1:nrow(txt), txt)
  for(col in 1:ncol(txt)) {
    txt[, col] <- str_pad(txt[, col], max(nchar(txt[, col])) + 1, side="right")
  }
  
  txt <- map(1:nrow(txt), ~ str_c(txt[., ], collapse=" ")) 
  
  # add the labels
  labels <- attr(program, "labels")
  for(label in labels) {
    txt[[label$line]] <- c(fmt("{label$label}:"), txt[[label$line]])
  }
  
  writeLines(unlist(txt))
} 

