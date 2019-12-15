# SPDX-License-Identifier: MIT
# Copyright (c) 2019 Taras Zakharko


#' src.buffer.R 
#' -------
#' 
#' Implements a text buffer as well as references to its content that 
#' track row and column location of the original text. 
#'
#' The text buffer references are lightweight objects with value semantics
#' and can be used in immutable data structures. 
#'
#'
#' 
#' @name src.buffer
#' 
#' 
#' @importFrom glue glue
#' @importFrom crayon %+% 
#' @importFrom stringr str_pad str_c str_sub
#' @importFrom assertthat assert_that on_failure<-
NULL

# ------------------------------------------------ 
#    Source buffer initialisation
# ------------------------------------------------


#' Create a source text buffer
#'
#' Creates an immutable text buffer given the source text
#'
#' @param text A character vector that represents lines of the source text
#' @param file A name of a file or a connection object to read the source text from
#' @param x 
#'
#' @rdname src.buffer
#' @return A buffer location to the start of the newly created buffer
#' @export
src.buffer <- function(file, text, filename=NULL) {
  assert_that(xor(missing(text), missing(file)) , msg="src.buffer() must specify either file or text, but not both")
  
  if(missing(text)) {
    text <- readLines(file, warn=FALSE)
    fname <- if(is.character(file)) file else filename
  } else {
    fname <- filename
  }
    
  # the environment that holds the buffer data
  buffer <- new.env(parent = emptyenv(), size=4)
  
  n.rows   <- length(text)
  
  get.lines <- function(range, empty.line="") {
    ifelse(range>0 & range<=n.rows, text[range], empty.line)
  }
  
  buffer$text      <- text
  buffer$n.rows    <- n.rows
  buffer$get.lines <- get.lines
  buffer$filename  <- fname
  
  lockEnvironment(buffer, bindings=TRUE)
  class(buffer) <- c("src.buffer")
  
  
  # return the location
  structure(
    list(row = 1, col = 1, line = buffer$get.lines(1), buffer = buffer),
    class = "src.buffer.location"
  )
}

#' @rdname src.buffer
#' @export 
is.src.buffer.location <- function(x) inherits(x, "src.buffer.location")

on_failure(is.src.buffer.location) <- function(call, env) {
  fmt("{deparse(call$x)} is not a source buffer location")
}

#' @rdname src.buffer
#' @export 
src.buffer.eof <- structure(list(),  class = c("src.buffer.location", "src.buffer.eof"))

#' @rdname src.buffer
#' @export 
is.src.buffer.eof <- function(x) inherits(x, "src.buffer.eof") || (is.src.buffer.location(x) && x$row > x$buffer$n.rows)

# ------------------------------------------------ 
#    Consuming buffer 
# ------------------------------------------------


#' Read a number of characters from a source buffer and return a new source buffer 
#' location
#'
#' @param location The source buffer location
#' @param n   The number of characters to advance/read
#' 
#' @return A list with two components, `location` and `text` for the new 
#'         source buffer location and the text read, respectively
#'
#' @description
#'
#' Only the current line in the buffer can be read, meaning that it is an error to 
#' attempt to read more characters than the line currently contains. EOL (end of line)
#' is treated in a special way: if buffer is at EOL, exactly one character must be read
#' and the text returned is `\\n`
#'
#' @rdname src.buferr.read
#' @export
src.buffer.read  <- function(location, n) {
  assert_that(is.src.buffer.location(location))
  assert_that(is.numeric(n) && isTRUE(floor(n) == n))
  assert_that(location != src.buffer.eof, msg="Cannot advance a source buffer location past EOF")
  
  # empty read
  if(n == 0) {
    list(location = location, value="")
  } else 
  # EOL
  if(location$line == "") {
    assert_that(n == 1, msg=fmt("Can only read exactly one character at the end of a line, {n} characters requested"))
    
    # return new input, advancing the line
    list(
      value = "\n",
      location = structure(
        list(
          row = location$row+1, 
          col = 1, 
          line = location$buffer$get.lines(location$row+1, empty.line=NA),
          buffer = location$buffer
        ),
        class = "src.buffer.location"
      )
    )
  } 
  # consume n characters
  else {
    assert_that(
      n <= nchar(location$line), 
      msg=fmt("Line contains {nchar(location$line)} characters, {n} characters requested")
    )  

    # return new input, advancing the line
    list(
      value = str_sub(location$line, end=n),
      location = structure(
        list(
          row = location$row, 
          col = location$col + n, 
          line = str_sub(location$line, start=n+1),
          buffer = location$buffer
        ),
        class = "src.buffer.location"
      )
    )
  }   
} 

#' @rdname src.buferr.read
#' @export
src.buffer.read.line  <- function(location) {
  assert_that(is.src.buffer.location(location))
  assert_that(location != src.buffer.eof, msg="Cannot advance a source buffer location past EOF")

  
  # return new input, advancing the line
  list(
    value =  location$line,
    location = structure(
      list(
        row = location$row +1, 
        col = 1,
        line = location$buffer$get.lines(location$row+1, empty.line=NA),
        buffer = location$buffer
      ),
      class = "src.buffer.location"
    )
  )
}

# ------------------------------------------------ 
#    Buffer offsetting
# ------------------------------------------------
# `+`.src.buffer.location <- function(location, offset) {
#   assert_that(is.src.buffer.location(location))
#   assert_that(is.numeric(offset) && (abs(offset) == 1),
#     msg="can only offset location by one character"
#   )
#
#   if(offset>0) {
#
#   } else {
#
#   }
#
# }


# ------------------------------------------------ 
#    Buffer range
# ------------------------------------------------

# @export
`%to%` <- function(a, b) UseMethod("%to%") 


# @export
`%to%.src.buffer.location` <- function(location0, location1) {
  assert_that(is.src.buffer.location(location0))
  assert_that(is.src.buffer.location(location1))
  
  assert_that(location0 < location1, msg="Start of a source buffer range must come before its end")
   
  buffer <- location0$buffer
  if(location1 == src.buffer.eof) {
    location1 <- list(row = buffer$n.rows, col=nchar(buffer$get.lines(buffer$n.rows))+1)
  }
  
  text <- buffer$get.lines(location0$row:location1$row)
  
  if(location0$row == location1$row) {
    text <- str_sub(text, start = location0$col, end = location1$col-1)
  } else {
    text[1] <- str_sub(text[1], start = location0$col)
    text[length(text)] <- str_sub(text[length(text)], end = location1$col-1)
  }
  
  structure(
    list(
      start = list(row = location0$row, col = location0$col),
      end = list(row = location1$row, col = location1$col),
      text = text,
      buffer = buffer
    ),
    class = "src.buffer.range"
  )
}




# ------------------------------------------------ 
#    Cursor comparison
# ------------------------------------------------

#' @export 
`==.src.buffer.location` <- function(location0, location1) {
  assert_that(is.src.buffer.location(location0))
  assert_that(is.src.buffer.location(location1))
  
  if(is.src.buffer.eof(location0)) {
    is.src.buffer.eof(location1) || location1$row > location1$buffer$n.rows
  } else 
  if(is.src.buffer.eof(location1)) {
    is.src.buffer.eof(location0) || location0$row > location0$buffer$n.rows
  } else {
    assert_that(identical(location0$buffer, location1$buffer), msg="Cannot compare locations across different source buffers")
      
    (location0$row == location1$row && location0$col == location1$col) 
  }  
}

#' @export 
`>.src.buffer.location` <- function(location0, location1) {
  assert_that(is.src.buffer.location(location0))
  assert_that(is.src.buffer.location(location1))
  
  
  if(is.src.buffer.eof(location0) || is.src.buffer.eof(location1)) {
    is.src.buffer.eof(location0) && !is.src.buffer.eof(location1)
  } else {
    assert_that(identical(location0$buffer, location1$buffer), msg="Cannot compare locations across different source buffers")
      
    (location0$row > location1$row) || (location0$row = location1$row && location0$col > location1$col)
  }    
}

#' @export 
`>=.src.buffer.location` <- function(location0, location1) (location0 > location1) || (location0 == location1)

#' @export 
`!=.src.buffer.location` <- function(location0, location1) !(location0 == location1)

#' @export 
`<.src.buffer.location` <- function(location0, location1) (location1 > location0)

#' @export 
`<=.src.buffer.location` <- function(location0, location1) (location1 > location0) || (location0 == location1)

# ------------------------------------------------ 
#    Pretty printing
# ------------------------------------------------

#' @rdname format.src.buffer
#'
#' @param location
#' @param before
#' @param after
#' @param ...
#' @param show.row.numbers
#' @param range
#' @param mark
#' @param row
#' @param col
#' @param pos
#'
#' @export
as.data.frame.src.buffer.location <- function(location, before=2, after=2, ...) {
    # get the source lines surrounding the context
    rows <- location$row + (-before):after
    rows <- rows[rows > 0 & rows <= location$buffer$n.rows]

    text <- location$buffer$get.lines(rows)
    
    data.frame(row = rows, text = text, stringsAsFactors = FALSE)
}

#' @rdname format.src.buffer
#' @export
format.src.buffer.location <- function(location, before=2, after=2, show.row.numbers = TRUE, ...) {
  # get the surrounding text lines
  df <- as.data.frame(location, before=before, after=after)
  
  # location position (column)
  col <- if(location == src.buffer.eof) nchar(df$text[nrow(df)]) + 1 else location$col-1
  
  # build the output
  df <- rbind(
    df[df$row <= location$row, ],
    data.frame(row = 0,  text=str_pad("", col, side="left") %+% crayon::red("^~~~"), stringsAsFactors = FALSE),
    df[df$row > location$row, ]
  ) 
  
  # transform the output into a formatted character vertor
  
  df$label <- if(show.row.numbers) {
    ifelse(df$row == 0, "", as.character(df$row) %+% ":") %>%
    str_pad(., max(nchar(.))+2, side="right")
  } else {
    ""
  }
  
  str_c(df$label, df$text)
}

#' @rdname format.src.buffer
#' @export
print.src.buffer.location <- function(location, before=2, after=2, ...) {
  filename <- location$buffer$filename
  if(is.null(filename)) filename <-  "" 
  
  
  header <- if(location == src.buffer.eof) {
    ("{filename} [EOF]")
  } else {
    fmt("{filename} [{location$row}:{location$col}]")
  }
    
  writeLines(c(
    header,
    "",
    "  " %+% format(location, before=before, after=after)
  ))      
}

#' @rdname format.src.buffer
#' @export
as.data.frame.src.buffer.range <- function(range, before=2, after=2, ...) {
    # get the source lines surrounding the context
    rows <- (range$start$row-before):(range$end$row + after)
    rows <- rows[rows > 0 & rows <= range$buffer$n.rows]

    text <- range$buffer$get.lines(rows)
    
    data.frame(row = rows, text = text, stringsAsFactors = FALSE)
}

#' Source buffer formatting 
#'
#' @rdname format.src.buffer
#' @export
add.mark <- function(mark, row, col, pos=c("before", "after")) {
  structure(list(mark = mark, row=row, col=col, pos=match.arg(pos)), class="src.buffer.print.mark")
}

#' @rdname format.src.buffer
#' @export
format.src.buffer.range <- function(range, ..., before=2, after=2, show.row.numbers = TRUE) {
  # get the surrounding text lines
  df <- as.data.frame(range, before=before, after=after)
  
  marks <- list()
  
  # handle varargs
  for(arg in list(...)) {
    assert_that(inherits(arg, "src.buffer.print.mark"), msg=fmt("unknown format parameter {deparse(arg)}"))
    marks <- c(marks, list(arg))
  }
  
  # default marks
  if(length(marks) == 0) {
    marks <- list(
      add.mark("▼", range$start$row, range$start$col, pos="before"),
      add.mark("▲", range$end$row, range$ed$col, pos="after")
    )
  }
  
  # build the output
  df$label <- if(show.row.numbers) paste0(df$row, ":") else ""
  
  # add the marks
  while(length(marks) > 0) {
    assert_that(!is.null(row <- marks[[1]]$row))
    assert_that(!is.null(col <- marks[[1]]$col))
    assert_that(!is.null(mark <- marks[[1]]$mark))
    
    pre <- switch(marks[[1]]$pos, 
      before = df$row < row,
      after = df$row <= row,
      stop(fmt("unknown mark position '{marks[[1]]$pos}'"))
    )
    
    df <- rbind(
      df[pre, ],
      data.frame(row=row, text=paste0(str_pad("", col), mark), label="", stringsAsFactors=FALSE),
      df[!pre, ]
    )
    
    marks <- marks[-1]
  }
  
  df$label <- str_pad(df$label, max(nchar(df$label))+2, side="right")
  
  paste0(df$label, df$text)
}


#' @export
print.src.buffer.range <- function(range, before=2, after=2, ...) {
  filename <- range$buffer$filename
  if(is.null(filename)) filename <-  "" 
  
  
  header <- fmt("{filename} [{range$start$row}:{range$start$col}] - [{range$end$row}:{range$end$col}]")
  
    
  writeLines(c(
    header,
    "",
    "  " %+% format(range, before=before, after=after)
  ))      
}
