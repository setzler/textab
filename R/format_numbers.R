
# This function expands a scalar to a vector.
expansion_function <- function(x, y) {
  if (length(y) > 1) {
    stopifnot(length(x) == length(y))
  }
  if (length(y) == 1) {
    y <- rep(y, length(x))
  }
  return(y)
}

# This function formats numbers in a table within a LaTeX document
tt_formatNum <- function(x, dec = 3, percentage = FALSE, dollar = FALSE, se = FALSE, pvalues = NULL) {

  # require that x is numeric
  stopifnot(is.numeric(x))
  # require that dec is numeric
  stopifnot(is.numeric(dec))
  # require that pvalues is numeric if not NULL
  if (!is.null(pvalues)) {
    stopifnot(is.numeric(pvalues))
  }
  # require that pvalues are between 0 and 1 if not NULL
  if (!is.null(pvalues)) {
    stopifnot(all(pvalues >= 0))
    stopifnot(all(pvalues <= 1))
  }
  # require that percentage is logical
  stopifnot(is.logical(percentage))
  # require that dollar is logical
  stopifnot(is.logical(dollar))
  # require that se is logical
  stopifnot(is.logical(se))

  # expand inputs
  dec <- expansion_function(x, dec)
  percentage <- expansion_function(x, percentage)
  dollar <- expansion_function(x, dollar)
  se <- expansion_function(x, se)

  # format numbers
  y <- NULL
  for (itera in 1:length(x)) {
    if (!is.na(x[itera])) {
      val <- round(x[itera], digits = dec[itera])
      val <- format(val, big.mark = ",", nsmall = dec[itera], scientific = FALSE)
      val <- trimws(val)
      if (percentage[itera]) {
        val <- paste0(val, "\\%")
      }
      if (dollar[itera]) {
        val <- paste0("\\$", val)
      }
      if (se[itera]) {
        val <- paste0("(", val, ")")
      }
      if (!is.null(pvalues)) {
        if (pvalues[itera] <= .01) {
          val <- paste0(val, "*")
        }
        if (pvalues[itera] <= .05) {
          val <- paste0(val, "*")
        }
        if (pvalues[itera] <= .1) {
          val <- paste0(val, "*")
        }
      }
    } else {
      val <- ""
    }
    y <- c(y, val)
  }

  return(y)
}
