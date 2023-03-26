
#' Compile a tabular object to a pdf file
#' @param tab textab block, created by TexRow().
#' @param filename (character). The file will be saved as filename.tex.
#' @param positions (character). Vector of positions, e.g., "c("l","c","r")" means that the first column will be left-aligned, second column will be center-aligned, and third column will be right-aligned.
#' @param pretty_rules (logical). If TRUE, extra formatting rules will be added to the bottom and top of the tabular.
#' @param output_path (character). This is the directory path where the file should be saved. Default is the current directory.
#' @param stand_alone (logical). If TRUE, the tabular will be exported in a .tex file that can be compiled to PDF directly (rather than included in a separate .tex file). Default is FALSE.
#' @param compile_tex (logical). If TRUE and stand_alone is TRUE, pdflatex is used to compile the TeX table into a PDF. This is only allowed if stand_alone=TRUE. Default is FALSE.
#' @examples
#' # consider the following example textab object:
#' tt = TexRow(c("hello", "world")) + TexRow(c(1,2))
#'
#' # define the positions for each column:
#' pos = c("l","c")
#'
#' # choose an output path:
#' op = tempdir()
#'
#' # Save a simple .tex document containing this table:
#' TexSave(tab = tt, positions = pos, filename = "example1", output_path = op)
#'
#' # Save the .tex document as stand-alone, which includes LaTeX headers and packages:
#' TexSave(tab = tt, positions = pos, filename = "example2", output_path = op, stand_alone = TRUE)
#'
#' @return A list containing the path to the .tex file and the name of the .tex file.
#' @export
TexSave <- function(tab, filename, positions, pretty_rules = TRUE, output_path = getwd(), stand_alone=FALSE, compile_tex = FALSE) {
  tab <- tt_tabularize(tab, pretty_rules = pretty_rules, positions = positions)
  current_wd <- getwd()
  on.exit(setwd(current_wd))
  if (!(class(output_path) %in% c("character"))) {
    stop("output_path must be character.")
  }
  if(!dir.exists(output_path)){
    stop(sprintf("Cannot setwd(output_path). The specified output_path %s does not exist.", output_path))
  }
  setwd(output_path)
  filename <- paste0(filename, ".tex")
  if(stand_alone){
    tt_save(tab, filename = filename, stand_alone = TRUE)
    if(compile_tex){
      tools::texi2pdf(file = filename, clean = TRUE)
    }
  }
  if(!stand_alone){
    tt_save(tab, filename = filename, stand_alone = FALSE)
  }
  return(list(output_path = output_path, filename = filename))
}

# Convert the textab object to a LaTeX tabular.
tt_tabularize <- function(tt, positions = rep("r", tt$ncol), pretty_rules = FALSE) {
  if (pretty_rules) {
    tt <- tt_rule_top() + TexMidrule() + tt + TexMidrule() + tt_rule_bottom()
  }

  tabular <- sprintf("\\begin{tabular}{%s}", paste(positions, collapse = ""))

  for (i in 1:length(tt$row_list)) {
    tabular <- c(
      tabular,
      paste(paste(tt$row_list[[i]], collapse = " & "), tt$row_ending[[i]])
    )
  }

  tabular <- c(tabular, "\\end{tabular}")

  # append comment with date of creation
  tabular <- c(sprintf("%% created using textab on %s", format(Sys.time(), "%a %b %d %X %Y")), tabular)

  return(tabular)
}

# Save a textab object to .tex, with stand_alone option (not exported)
tt_save <- function(tabular, filename, stand_alone = FALSE) {
  if (stand_alone) {
    tabular <- c(
      "\\documentclass{standalone}",
      "\\usepackage{booktabs}",
      "\\usepackage{graphicx}",
      "\\usepackage{multirow}",
      "\\usepackage{amssymb}",
      "\\usepackage{amsmath}",
      "\\usepackage{pifont}",
      "\\usepackage{xcolor}",
      "\\usepackage[margin=1in]{geometry}",
      "\\begin{document}",
      tabular,
      "\\end{document}"
    )
  }
  openfile <- file(filename)
  writeLines(tabular, openfile)
  close(openfile)
}
