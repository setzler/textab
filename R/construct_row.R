
#' This function creates a row of a LaTeX table.
#' @param value The value(s) to be formatted. Must be a numeric or character vector.
#' @param cspan (integer). If greater than 1, `multicolumn{cspan}{position}{value}` will be used. For example, cspan=c(1,2,1) means that the second entry of `value` should span 2 columns. Default is cspan = rep(1, length(value)).
#' @param position (character). If cspan > 1, `multicolumn{cspan}{position}{value}` will be used. For example, position=c("l","c","r") means that the second entry of `value` should be centered. Default is "c".
#' @param surround (character). This will be applied to the value as sprintf(surround, value), so surround must contain the "%s" placeholder. Default is "%s".
#' @param space (numeric). The number of points (pt) of vertical space to append to the end of the row. Default is 0.
#' @param dec (integer). Only relevant if `value` is numeric. Number of decimal places. If scalar, the same decimal will be used for each entry of `value`. If vector, must be the same length as `value`. Default is 3.
#' @param percentage (logical). Only relevant if `value` is numeric. If TRUE, a percentage symbol "%" will be added to the end of each entry of `value`. If scalar, it will be used for all entries of `value`. If vector, must be the same length as `value`.
#' @param dollar (logical). Only relevant if `value` is numeric. If TRUE, a dollar sign "$" will be added to the end of each entry of `value`. If scalar, it will be used for all entries of `value`. If vector, must be the same length as `value`.
#' @param se (logical). Only relevant if `value` is numeric. If TRUE, `value` will be wrapped in parentheses. If scalar, it will be used for all entries of `value`. If vector, must be the same length as `value`.
#' @param pvalues (numeric). Only relevant if `value` is numeric. If not NULL, must be numeric. If less than 0.1, a star will be added to the end of each entry of `value`. If less than 0.05, a second star will be appended. If less than 0.01, a third star  will be appended. If scalar, the same p-value will be assumed for all entries of `value`. If vector, must be the same length as `value`.
#' @examples
#' # basic character row:
#' vec = c("hello", "world")
#' TexRow(vec)
#'
#' # character row with LaTeX formatting:
#' vec = c('Hello','\\textbf{World}','$\\alpha$','$\\frac{1}{2}$')
#' TexRow(vec)
#'
#' # basic numeric row:
#' vec <- c(1.0, 1.01, 1.001)
#' TexRow(vec)
#' TexRow(vec, dec = 2) # round to second decimal place
#'
#' # custom formatting of numbers using surround argument:
#' vec = c(5.081, 2.345, 6.789)
#' TexRow(vec, dec = 1, surround = "{\\color{red} %s}")
#'
#' # use cspan argument to merge the second and third rows:
#' vec = c("hello", "world")
#' TexRow(vec, cspan = c(1,2))
#' TexRow(vec, cspan = c(1,2), position = "c") # center merged columns
#'
#' # concatenate blocks vertically or horizontally:
#' block1 = TexRow(c("hello","world","block"))
#' block2 = TexRow(c(5.081, 2.345, 6.789), dec=1)
#' block1 / block2 # horizontal
#' block1 + block2 # vertical
#'
#' # add 3pt of vertical space between two rows using the space argument:
#' TexRow(c("hello", "world"), space=3) + TexRow(c('$\\alpha$','$\\frac{1}{2}$'))
#'
#' @return The output is a textab block.
#' @export
TexRow <- function(value, cspan = rep(1, length(value)), position = "c", surround = "%s", space = 0, dec = 3, percentage = FALSE, dollar = FALSE, se = FALSE, pvalues = NULL) {

  # if numeric, apply formatting rules to value. return character.
  if (class(value) %in% c("numeric","integer")) {
    value <- tt_formatNum(x = value, dec = dec, percentage = percentage, dollar = dollar, se = se, pvalues = pvalues)
  }

  # make sure value is now character.
  if (!(class(value) %in% c("character"))) {
    stop("Argument `value` must be numeric or character vector. See ?TexRow for details.")
  }

  # apply expansions.
  surround <- expansion_function(value, surround)
  position <- expansion_function(value, position)

  for(ii in 1:length(value)){
    value[ii] = sprintf(surround[ii], value[ii])
  }

  # apply column spanning.
  I <- which(cspan > 1)
  value[I] <- sprintf("\\multicolumn{%i}{%s}{%s}", cspan[I], position[I], value[I])

  # convert to list block structure.
  end_term = "\\\\"
  if(space > 0){
    end_term = sprintf("\\\\[%ipt]", space)
  }
  row_list <- list()
  row_list[[1]] <- value
  ending <- list(rep(end_term, length(row_list)))
  row_ending <- list()
  row_ending[[1]] <- ending

  # finish.
  output = tt_block(sum(cspan), length(cspan), row_list, row_ending = ending)
  return(output)

}
