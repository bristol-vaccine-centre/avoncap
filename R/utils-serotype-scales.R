
#' A ggplot scale for pneumococcal serotypes that keeps PCV groups together
#'
#' The scale groups colours by PCV group, but it is important to have the source
#' data using the same levels as this scale otherwise the colour legend will be
#' ordered in a different sequence. This can be achieved using `relevel_serotypes`,
#'
#' @inheritDotParams ggplot2::scale_fill_manual
#' @param palette_fn a function that returns a set of colours for a number of
#'   levels. Such functions can be obtained from things like `scales::brewer_pal(...)`
#' @param undefined the colour for the last group which is assumed to be the
#'   `Unknown` types
#' @inheritParams relevel_serotypes
#'
#' @return A ggplot2 scale
#' @export
scale_fill_serotype = function(..., palette_fn = scales::brewer_pal(palette="Dark2"), undefined = "#606060", exprs = rlang::exprs()) {

  # TODO: investigate ggh4x grouped scales:
  # https://stackoverflow.com/questions/23207878/ggplot2-group-x-axis-discrete-values-into-subgroups

  serotype_groups = .serotype_groups(!!!exprs)

  major = tibble::tibble(
    pneumo.group = levels(serotype_groups$pneumo.group),
    colour = c( palette_fn(length(levels(serotype_groups$pneumo.group))-1), undefined)
  )

  named_shades = serotype_groups %>% dplyr::inner_join(major, by = "pneumo.group") %>%
    dplyr::group_by(pneumo.group,colour) %>%
    dplyr::mutate(
      fade = 1 - (1 - 1/dplyr::n())^(dplyr::row_number() - 1),
      shade = colorspace::lighten(colour, fade)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(pneumo.phe_serotype, shade) %>%
    tibble::deframe()


  return(list(
    ggplot2::scale_fill_manual(values = named_shades, name=NULL, ...),
    ggplot2::guides(
      fill = ggplot2::guide_legend(ncol=2),
      colour = ggplot2::guide_none()
    )
  ))

}

# .sort_serotypes(pcv24_Affinivax)
# sorts into an ascending type/subtype text order e.g. 1,2A,2B,4,10B,11A
.sort_serotypes = function(serotypes) {
  tmp = serotypes
  t1 = tmp %>% unique() %>% stringr::str_extract("[0-9]*") %>% as.integer()
  t2 = tmp %>% unique() %>% stringr::str_remove_all("[0-9]*") %>% rank()
  tmp[order(t1+t2/100)]
}

#' Relevel serotype data into an factor based on PCV group status
#' and serotype name.
#'
#' @param serotypes a vector of serotypes as a factor or character.
#' @param ... an unwrapped version of the `exprs` parameter
#' @param exprs a list of formulae with a predicate on the LHS and a PCV group name on
#'   the RHS. which are interpreted as the parameters for a `dplyr::case_when`
#'   call. This must be protected against interpretation by wrapping it in
#'   `rlang::exprs()`. The predicates are tested against `avoncap::serotype_data$map`
#'   and could use any of  the following columns `r paste0("'", avoncap::serotype_data$map, "'", collapse =",")`
#'   a default option of the form `TRUE ~ "Non PCV serotype"` must exist to capture
#'   unmatched items.
#' @export
#' @examples
#' x = rlang::exprs(
#'   PCV7 ~ "PCV7",
#'   PCV15 ~ "PCV15-7",
#'   TRUE ~ "Non-PCV15 serotype"
#' )
#' relevel_serotypes(avoncap::phe_serotypes, exprs=x)
#' relevel_serotypes(avoncap::phe_serotypes)
#'
#' relevel_serotypes(avoncap::phe_serotypes,
#'   PCV24Affinivax ~ "Affinivax",
#'   TRUE ~ "Non-affinivax"
#' )
relevel_serotypes = function(serotypes, ..., exprs) {
  if (!rlang::is_missing(exprs)) {
    tmp = .serotype_groups(!!!exprs)
  } else {
    tmp = .serotype_groups(...)
  }
  l = levels(tmp$pneumo.phe_serotype)
  factor(as.character(serotypes),levels = l)
}

#' @noRd
#' @examples
#' .serotype_groups(
#'   PCV7 ~ "PCV7",
#'   PCV15 ~ "PCV15-7",
#'   PCV20 ~  "PCV20-15",
#'   TRUE ~ "Non-PCV serotype"
#' )
#'
#' .serotype_groups()
.serotype_groups = function(...) {
  dotsexp = rlang::enexprs(...)
  if (length(dotsexp) == 0) {
    return(
      .serotype_groups(
        PCV7 ~ "PCV7",
        PCV13 ~ "PCV13-7",
        PCV15 ~ "PCV15-13",
        PCV20 ~  "PCV20-15",
        TRUE ~ "Non-PCV serotype"
      )
    )
  }
  dots = rlang::list2(...)
  levels = sapply(dots, rlang::f_rhs)
  out = avoncap::serotype_data$map %>% dplyr::transmute(
    pneumo.phe_serotype=serotype,
    pneumo.group = dplyr::case_when(!!!dotsexp)
  ) %>% tidyr::complete(
    pneumo.phe_serotype = avoncap::phe_serotypes$serotype, fill=list(pneumo.group = utils::tail(levels,1))
  ) %>% dplyr::mutate(
    pneumo.group = factor(pneumo.group,levels),
  ) %>% dplyr::arrange(pneumo.group) %>%
  tidyr::nest(data = -pneumo.group) %>%
  dplyr::mutate(
    data = purrr::map(data, ~ .x %>% dplyr::mutate(pneumo.phe_serotype = .sort_serotypes(pneumo.phe_serotype)))
  ) %>% tidyr::unnest(data) %>%
  dplyr::mutate(
    pneumo.phe_serotype = forcats::as_factor(pneumo.phe_serotype)
  )
  return(out)
}
