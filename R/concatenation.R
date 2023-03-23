# basic building blocks for TeX tables (not exported)
tt_block <- function(nrow = 0, ncol = 0, row_list = list(), row_ending = list(), allow_rbind = TRUE) {
  block <- list(ncol = ncol, nrow = nrow, row_list = row_list, row_ending = row_ending, allow_rbind = allow_rbind)
  class(block) <- list("tt_block", "tt_")
  return(block)
}

#' Print a textab block as a LaTeX tabular.
#' @param x A textab block.
#' @param ... Placeholder for print.
#' @export print.tt_block
#' @export
print.tt_block <- function(x, ...) {
  tabular <- tt_tabularize(x)
  cat(paste(tabular, collapse = "\n"))
}

#' Concatenate textab blocks horizontally (side-by-side).
#' @param top_row The top tabular row.
#' @param bottom_row The bottom tabular row.
#' @export -.tt_
#' @export
`-.tt_` <- function(top_row, bottom_row) {
  if (("tt_block" %in% class(top_row)) && ("tt_block" %in% class(bottom_row))) {
    block <- tt_block()
    block$row_list[[1]] <- c(top_row$row_list[[1]], bottom_row$row_list[[1]])
    block$row_ending <- bottom_row$row_ending
    block$ncol <- top_row$ncol + bottom_row$ncol
    block$nrow <- top_row$nrow
    return(block)
  }
}

#' Concatenate textab blocks vertically.
#' @param left_block The left block of the tabular row.
#' @param right_block The right block of the tabular row.
#' @export +.tt_
#' @export
`+.tt_` <- function(left_block, right_block) {
  if (("tt_block" %in% class(left_block)) && ("tt_block" %in% class(right_block))) {
    left_block$ncol <- pmax(left_block$ncol, right_block$ncol)
    left_block$row_list <- c(left_block$row_list, right_block$row_list)
    left_block$row_ending <- c(left_block$row_ending, right_block$row_ending)
    left_block$nrow <- left_block$nrow + right_block$nrow
    return(left_block)
  }
  if (("tt_block" %in% class(left_block)) && ("tt_mod_ending" %in% class(right_block))) {
    left_block$row_ending[[length(left_block$row_ending)]] <- right_block$str
    return(left_block)
  }
}
