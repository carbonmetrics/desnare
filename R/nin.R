#' Filter out
#'
#' This function is the opposite of the %in% operator.
#' You specify the vector that you want to test on the left side, and the test values on the right side, see example.
#'
#'
#' @param vector of elements to be filtered out
#' @return vector of filtered elements
#' @export
#' @examples
#' data(mtcars); setDT(mtcars)
#' mtcars[cyl %in% c(4,8), ]
#' mtcars[cyl %nin% c(4,8), ]


`%nin%` = function (x, table) {
  match(x, table, nomatch = 0) == 0
}
