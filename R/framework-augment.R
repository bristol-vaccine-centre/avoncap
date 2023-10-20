## Standard data manipulation post load ----

# TODO: use this more?
.opt = function(...) {
  tryCatch(rlang::eval_tidy(...), error=function(e) rlang::eval_tidy(FALSE))
}

# Apply an augmentation to the dataframe

#' Applies a set of functions to the whole dataframe
#'
#' This sequences, catches errors and allows parameters to be passed by name
#'
#' @param df a data frame
#' @param ... unnamed parameters are a list of functions, named parameters
#'   are passed to those functions (if they match formal arguments).
#'
#' @return the altered df
#' @export
#'
#' @examples
#' fn1 = function(df,v) {df %>% dplyr::filter(cut=="Fair") %>% dplyr::mutate(x_col = color)}
#' fn2 = function(df,v) {df %>% dplyr::filter(color==v$color$J)}
#' df = ggplot2::diamonds %>% augment_generic(fn1, fn2)
augment_generic = function(df, ...) {
  dots = rlang::list2(...)
  if (!is.null(names(dots))) {
    fns = dots[names(dots) == ""]
    params = dots[names(dots) != ""]
  } else {
    fns = dots
    params = list()
  }
  #TODO: store error messages in an attribute and retrieve later.
  .cached({
    tmp = df
    for (fn in fns) {
      fn = purrr::as_mapper(fn)
      used_cols = all.vars(body(fn)) %>% intersect(colnames(df))
      if ("..." %in% names(formals(fn))) {
        used_params = names(params)
      } else {
        used_params = names(formals(fn)) %>% intersect(names(params))
      }
      tmp_params = params[names(params) %in% used_params]
      v = avoncap::get_value_sets(tmp)

      tmp = tryCatch({




        df2 = tmp %>% dtrackr::pause()
        df2 = rlang::exec(fn, df2, v, !!!tmp_params)
        df2 = df2 %>% dtrackr::resume()

        # copy old attributes to new data
        df2 = df2 %>% .restore_attributes(tmp)

        new_cols = colnames(df2) %>% setdiff(colnames(tmp))
        # TODO: look into dtrackr for this.
        df2 = df2 %>%
          dplyr::mutate(dplyr::across(tidyselect::any_of(new_cols), .fns = ~ .x %>% magrittr::set_attr("depends", used_cols)))
        message("Created ",paste0(new_cols,collapse=", ")," using: ",paste0(used_cols,collapse=", "))
        df2
      }, error = function(e) {
        message("Skipping function due to error: ", e$message)
        tmp
      })
    }
    tmp
  }, df, deparse1(fns), params, .prefix = "augment") %>% return()
}

#' Derived data function template
#'
#' @param df the dataframe.
#' @param v the value set. usually precomputed by the augment framework the value
#' set can be explicitly supplied with `v = get_value_sets(df)`
#' @param ... ignored
#'
#' @return a dataframe
#' @export
derive_template = function(df,v,...) {
  df
}

## Specific dataset dispatch ----

#' Sanitise AvonCap data columns
#'
#' AvonCap data has lots of columns which are named in a difficult to remember
#' fashion, composed of data items that have enumerated values with no
#' semantics. This makes displaying them difficult and any filtering done on the
#' raw data inscrutable. Depending on the source of the data some different
#' columns may be present due to differences in the NHS and UoB data sets. The
#' redcap database has some options that may be checklists and some that are
#' radio buttons, both of these end up with mysterious names in the data.
#'
#' This function maps the data into a tidy dataframe with consistently named
#' columns, and named factors where appropriate. If not present in the data the
#' ethnicity
#'
#' files Most of the sanitisation code is held in the
#' `zzz-avoncap-mappings.R` file.
#'
#' @param x - the raw data from `load_data()`
#' @inheritDotParams augment_generic
#'
#' @return a tracked dataframe with
#' @export
augment_data = function(
    x,
    ...
) {

  type = attr(x,"type")
  subtype = attr(x,"subtype")
  instrument = attr(x,"instrument")
  norm_fn = c("augment",type,subtype,instrument) %>% stringr::str_replace("-","_") %>% paste0(collapse=".")
  message("Augmenting data using: ",norm_fn)
  norm_fn = tryCatch(
    # utils::getFromNamespace(norm_fn,"avoncap"),
    get(norm_fn),
    error = function(e) {
      message("No data augmentation defined for ",norm_fn)
      return(function(x, ...) x)
    }
  )
  # dispatch the call to the specific subtype of the call
  # or to a noop function if there is no specific method
  norm_fn(x, ...)

}


