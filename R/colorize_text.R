#' Colorize html output
#'
#' Render html or latex output with a specified color, based on https://bookdown.org/yihui/rmarkdown-cookbook/font-color.html
#'
#' @param x string.
#' @param color string with the color code.
#'
#' @return html-formated string.
#'
#' @examples
#' colorize_text("this text is red", "red)
#' bmi3(bmi.vals)
#'
#' @export
colorize_text <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color,
            x)
  } else x
}
