

#' Wrapper around `table`
#'
#' @param data a dataframe
#' @param ... columns to cross-tabulate
#'
#' @return the cross-tabulation
#' @export
xglimpse = function(data, ...) {
  # exprs = rlang::enexprs(...)
  # tidyselect::eval_select(rlang::expr(c(...)),data)
  # browser()
  data %>% dplyr::select(...) %>%
    dplyr::select(tidyselect::all_of(rev(colnames(.)))) %>% table(useNA = "ifany")
}
