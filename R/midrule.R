
#' Create one (or many) partial midrule(s).
#' @param rule_list (list). A list of integer vectors. Each integer vector must contain two integers which indicate the start and end column of the partial midrule. If rule_list = NULL, it returns the full midrule across all columns. The default is rule_list = NULL.
#' @examples
#' # set up two textab blocks:
#' block1 = TexRow(c("hello","world","block"))
#' block2 = TexRow(c(5.081, 2.345, 6.789), dec=1)
#'
#' # add a full midrule between the two blocks
#' block1 + TexMidrule() + block2
#'
#' # add a partial midrule to the first column and spanning the second-third columns:
#' block1 + TexMidrule(list(c(1,1), c(2,3))) + block2
#'
#' @return The output is a textab block.
#' @export
TexMidrule <- function(rule_list = NULL) {
  if(is.null(rule_list)){
    return(tt_block(1, 1, list(c("\\midrule")), list(c("")), FALSE))
  }
  # make sure rule_list is a list
  if (!is.list(rule_list)) {
    stop("rule_list must be a list")
  }
  # make sure each element of rule_list is an integer of length 2
  for (i in 1:length(rule_list)) {
    if(length(rule_list[[i]]) != 2){
      stop("each element of rule_list must be an integer of length 2")
    }
  }
  # construct midrule, looping over the list
  str <- ""
  for (i in 1:length(rule_list)) {
    str <- sprintf("%s \\cmidrule(lr){%i-%i}", str, rule_list[[i]][[1]], rule_list[[i]][[2]])
  }
  # finish
  tt_block(1, 1, list(c(str)), list(c("")), FALSE)
}

# create a top rule (not exported)
tt_rule_top <- function() {
  tt_block(1, 1, list(c("\\toprule")), list(c("")), FALSE)
}

# create a bottom rule (not exported)
tt_rule_bottom <- function() {
  tt_block(1, 1, list(c("\\bottomrule")), list(c("")), FALSE)
}

