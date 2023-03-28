

#' Wrapper around `table`
#'
#' @param data a dataframe
#' @param ... columns to cross-tabulate
#'
#' @return the cross-tabulation
#' @export
xglimpse = function(data, ...) {
  # exprs = enexprs(...)
  # tidyselect::eval_select(expr(c(...)),data)
  # browser()
  data %>% select(...) %>%
    dplyr::select(all_of(rev(colnames(.)))) %>% table(useNA = "ifany")
}
