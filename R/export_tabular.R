
# Convert the textable object to a LaTeX tabular.
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
  tabular <- c(sprintf("%% created using textables on %s", format(Sys.time(), "%a %b %d %X %Y")), tabular)

  return(tabular)
}

# Save a textable object to .tex, with stand_alone option (not exported)
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

#' Compile a tabular object to a pdf file
#' @param tab Object created by TexRow()
#' @param filename (character). The file will be saved as filename.tex.
#' @param positions (character). Vector of positions, e.g., "c("l","c","r")" means that the first column will be left-aligned, second column will be center-aligned, and third column will be right-aligned.
#' @param pretty_rules (logical). If TRUE, extra formatting rules will be added to the bottom and top of the tabular.
#' @param output_path (character). This is the directory path where the file should be saved. Default is the current directory.
#' @param stand_alone (logical). If TRUE, the tabular will be exported in a TeX file that can be compiled to PDF directly (rather than included in a separate TeX file). Default is FALSE.
#' @param compile_tex (logical). If TRUE, pdflatex is used to compile the TeX table into a PDF. This is only allowed if stand_alone=TRUE. Default is FALSE.
#' @export
TexSave <- function(tab, filename, positions, pretty_rules = TRUE, output_path = getwd(), stand_alone=FALSE, compile_tex = FALSE) {
  tab <- tt_tabularize(tab, pretty_rules = pretty_rules, positions = positions)
  current_wd <- getwd()
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
  setwd(current_wd)
}
