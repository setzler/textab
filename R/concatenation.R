# basic building blocks for TeX tables (not exported)
tt_block <- function(nrow = 0, ncol = 0, row_list = list(), row_ending = list(), allow_rbind = TRUE) {
  block <- list(ncol = ncol, nrow = nrow, row_list = row_list, row_ending = row_ending, allow_rbind = allow_rbind)
  class(block) <- list("tt_block", "tt_")
  return(block)
}

#' Print a textab block as a LaTeX tabular.
#' @param x A textab block.
#' @param ... Placeholder for print.
#' @return Print prints its argument and returns it invisibly.
#' @export print.tt_block
#' @export
print.tt_block <- function(x, ...) {
  tabular <- tt_tabularize(x)
  cat(paste(tabular, collapse = "\n"))
}

#' Concatenate textab blocks horizontally (side-by-side).
#' @param left_block The left block of the tabular row.
#' @param right_block The right block of the tabular row.
#' @return The output is a textab block, formed by horizontally concatenating the two provided textab blocks.
#' @examples
#' # define some textab blocks
#' first_block = TexRow(c(1,2))
#' first_block
#'
#' second_block = TexRow(3)
#' second_block
#'
#' third_block = TexRow(4)
#' third_block
#'
#' # concatenate two blocks horizontally
#' first_block / second_block
#'
#' # concatenate three blocks horizontally
#' first_block / second_block / third_block
#'
#' # concatenate both horizontally and vertically
#' # note: horizontal concatenation takes precedence over vertical concatenation
#' first_block + second_block / third_block
#'
#' @export /.tt_
#' @export
`/.tt_` <- function(left_block, right_block) {
  if (("tt_block" %in% class(left_block)) && ("tt_block" %in% class(right_block))) {
    block <- tt_block()
    block$row_list[[1]] <- c(left_block$row_list[[1]], right_block$row_list[[1]])
    block$row_ending <- right_block$row_ending
    block$ncol <- left_block$ncol + right_block$ncol
    block$nrow <- left_block$nrow
    return(block)
  }
}



#' Concatenate textab blocks vertically.
#' @param upper_block The upper block of the tabular row.
#' @param lower_block The lower block of the tabular row.
#' @return The output is a textab block, formed by vertically concatenating the two provided textab blocks.
#' @examples
#' # define some textab blocks
#' first_block = TexRow(c(1,2))
#' first_block
#'
#' second_block = TexRow(3)
#' second_block
#'
#' third_block = TexRow(4)
#' third_block
#'
#' # concatenate two blocks vertically
#' first_block + second_block
#'
#' # concatenate three blocks vertically
#' first_block + second_block + third_block
#'
#' # concatenate both horizontally and vertically
#' # note: horizontal concatenation takes precedence over vertical concatenation
#' first_block + second_block / third_block
#'
#' @export +.tt_
#' @export
`+.tt_` <- function(upper_block, lower_block) {
  if (("tt_block" %in% class(upper_block)) && ("tt_block" %in% class(lower_block))) {
    upper_block$ncol <- pmax(upper_block$ncol, lower_block$ncol)
    upper_block$row_list <- c(upper_block$row_list, lower_block$row_list)
    upper_block$row_ending <- c(upper_block$row_ending, lower_block$row_ending)
    upper_block$nrow <- upper_block$nrow + lower_block$nrow
    return(upper_block)
  }
  if (("tt_block" %in% class(upper_block)) && ("tt_mod_ending" %in% class(lower_block))) {
    upper_block$row_ending[[length(upper_block$row_ending)]] <- lower_block$str
    return(upper_block)
  }
}
